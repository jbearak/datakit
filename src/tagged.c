// .a to .z in doubles, stored the way haven and dtaparser store them.
// na_tag() and the grouping of `:=` read them here.
//
// A double is 64 bits: bit 63 is the sign, bits 52 to 62 the exponent
// and bits 0 to 51 the fraction. When the exponent bits are all 1 and
// the fraction is not 0, the double is a NaN ("not a number"). So there
// are many NaNs, differing only in their fraction bits, and the
// processor treats them all alike: it never reads a NaN's fraction as
// part of a number.
//
// R uses those spare bits to tell NA from NaN. In hexadecimal, its NaN
// is 7FF8000000000000 and its NA is 7FF00000000007A2: also a NaN, but
// with 7A2 in its low word, the 32 bits 0 to 31 (R's own source writes
// it in decimal, 1954; NA_LOW_WORD below). is.na() is TRUE
// for both; is.nan() and R_IsNA() look at the low word to tell them
// apart.
//
// haven's tagged NAs go one step further: R's NA with a letter's ASCII
// code ('a' is 97, hexadecimal 61; 'z' is 122) in bits 32 to 39, so .a
// is 7FF00061000007A2. The exponent is still all 1s, so it is still a
// NaN, and the low word is still 7A2, so R takes it for NA: is.na() is
// TRUE, and R code that knows nothing of letters treats it as a plain
// NA. Only code that reads the bits, like this file, sees the letter.
//
// The letter survives whatever copies the value: subsetting, c(),
// saving to .rds or Parquet. Arithmetic usually keeps it too (.a + 1
// gives 7FF80061000007A2: bit 51 is set, the letter and 7A2 are kept),
// but neither C nor R promises that, so the letters are for storing and
// recoding missing values, not for computing with them. The functions
// below ignore bit 51 and so take either form for the same letter.
//
// To look at those bits, the functions below copy the double into a
// 64-bit integer with memcpy(), the way C allows reading one type's
// bytes as another; compilers turn it into a single move. Then
// `bits >> 32` shifts bit 32 and those above it down to the bottom,
// `& 0xff` keeps the lowest 8 bits, and `(uint32_t) bits` keeps bits 0
// to 31.
//
// The functions R calls take and return SEXPs, R values (see init.c).
// The parts of R's C interface used here:
//
// - TYPEOF(x): which kind of value x is: REALSXP (double vector), INTSXP
//   (integer), LGLSXP (logical), STRSXP (character), VECSXP (list).
// - XLENGTH(x): the number of elements, as an R_xlen_t, an integer type
//   wide enough for vectors of more than 2^31 - 1 elements.
// - REAL_RO(x), INTEGER_RO(x), LOGICAL_RO(x): a pointer to a vector's
//   numbers, for reading only ("RO"). INTEGER(x) gives one to write
//   through. R stores logicals as ints: 0, 1 or NA_LOGICAL.
// - NA_INTEGER, NA_LOGICAL, NA_STRING: R's NA in integer, logical and
//   character vectors. ISNAN(x) is true for every NaN, R's NA included.
// - Rf_allocVector(type, n): a new R vector of n elements. The prefix
//   Rf_ marks a function of R's C interface.
// - PROTECT(x) and UNPROTECT(n): R's garbage collector frees values that
//   nothing in R refers to, and it may run whenever R allocates memory.
//   A vector C has just made is not yet referred to by anything R knows
//   of, so the next allocation could free it. PROTECT(x) puts x on a
//   list the collector leaves alone, and UNPROTECT(n) takes the last n
//   off it again. Every function must take off all it put on before it
//   returns. The value it returns is safe without protection, since
//   nothing allocates between the UNPROTECT and the return, and R keeps
//   the value once it has it.
// - Strings: a character vector holds pointers to strings, which are R
//   values too. Rf_mkCharLen(s, n) makes one from n bytes of C text.
//   STRING_ELT(x, i) reads element i of a character vector, and
//   SET_STRING_ELT(x, i, s) sets it. Elements are set only through
//   SET_STRING_ELT, never through a pointer, because the collector must
//   be told when one R value starts to refer to another.
// - VECTOR_ELT(x, i): element i of a list.
// - Rf_ScalarLogical(b): a new logical vector holding one TRUE or FALSE.
// - Rf_error(msg): raises an R error. It does not return: R leaves the
//   function there and takes off its PROTECTs itself.

#include <stdint.h>
#include <string.h>
#include <R.h>
#include <Rinternals.h>

// The low word of R's NA, and so of every tagged NA.
#define NA_LOW_WORD 0x7A2u

// The letter of a tagged NA, or 0 for a number, NA or another NaN.
// `tag - 'a' < 26u` tests 'a' <= tag <= 'z' in one comparison: below
// 'a', the unsigned subtraction wraps round to a huge number.
static inline char letter_of(double x) {
  uint64_t bits;
  memcpy(&bits, &x, sizeof bits);
  unsigned tag = (unsigned) ((bits >> 32) & 0xff);
  return ISNAN(x) && (uint32_t) bits == NA_LOW_WORD && tag - 'a' < 26u ? (char) tag : 0;
}

// 1 if the double's bits are a tagged NA. Branchless (& and an unsigned
// range test), so the loops below vectorise or at least do not
// mispredict. `(bits >> 52) & 0x7ff` is the exponent; all 1s means a
// NaN or an infinity, and the low word 7A2 rules out infinity, whose
// fraction is 0.
static inline unsigned tagged(const double *x) {
  uint64_t bits;
  memcpy(&bits, x, sizeof bits);
  return (((bits >> 52) & 0x7ff) == 0x7ff) & ((uint32_t) bits == NA_LOW_WORD) &
         ((uint32_t) ((bits >> 32) & 0xff) - 'a' < 26u);
}

// R_IsNA(), inlined, since it runs once per group: a NaN whose low
// word is 7A2, true for NA and .a to .z, false for NaN.
static inline int is_na(double x) {
  uint64_t bits;
  memcpy(&bits, &x, sizeof bits);
  return ISNAN(x) && (uint32_t) bits == NA_LOW_WORD;
}

// The letter of each element of a double vector ("a" to "z"), NA where
// it is a number, NA or NaN. Names are left to the R caller.
SEXP dta_na_tag(SEXP x) {
  if (TYPEOF(x) != REALSXP) Rf_error("na_tag: x must be a double vector");
  R_xlen_t n = XLENGTH(x);
  const double *p = REAL_RO(x);
  SEXP out = PROTECT(Rf_allocVector(STRSXP, n));
  // The 26 one-letter strings, made once, so that the loop below copies
  // one of them rather than making a string for each element.
  SEXP letters = PROTECT(Rf_allocVector(STRSXP, 26));
  for (int i = 0; i < 26; i++) {
    char c = (char) ('a' + i);
    SET_STRING_ELT(letters, i, Rf_mkCharLen(&c, 1));
  }
  for (R_xlen_t i = 0; i < n; i++) {
    char c = letter_of(p[i]);
    SET_STRING_ELT(out, i, c ? STRING_ELT(letters, c - 'a') : NA_STRING);
  }
  UNPROTECT(2);
  return out;
}

// Each element's place among the missing values, as a plain integer
// vector: 0 for a number, 1 for NA or NaN, 2 to 27 for .a to .z. `:=`
// groups by it so that each letter is a group of its own.
SEXP dta_missing_rank(SEXP x) {
  R_xlen_t n = XLENGTH(x);
  SEXP out = PROTECT(Rf_allocVector(INTSXP, n));
  int *o = INTEGER(out);
  switch (TYPEOF(x)) {
  case REALSXP: {
    const double *p = REAL_RO(x);
    for (R_xlen_t i = 0; i < n; i++) {
      char c = letter_of(p[i]);
      o[i] = c ? c - 'a' + 2 : ISNAN(p[i]); // 'a' is 2; ISNAN gives 1 or 0
    }
    break;
  }
  case INTSXP: {
    const int *p = INTEGER_RO(x);
    for (R_xlen_t i = 0; i < n; i++) o[i] = p[i] == NA_INTEGER;
    break;
  }
  case LGLSXP: {
    const int *p = LOGICAL_RO(x);
    for (R_xlen_t i = 0; i < n; i++) o[i] = p[i] == NA_LOGICAL;
    break;
  }
  default: // Rf_error() leaves here, so UNPROTECT is not needed
    Rf_error("missing_rank: x must be a double, integer or logical vector");
  }
  UNPROTECT(1);
  return out;
}

// Whether the double vector x holds a tagged NA in a group whose key is
// NA, which vctrs groups with plain NA (their bits differ only in the
// letter). Checked without allocating, so grouping a column with no
// tagged NA costs no more than before. `key` is the group keys of this
// column and `loc` each group's rows (1-based) in x, as
// vctrs::vec_group_loc() gives them. A tagged NA is NA to is_na(), so it
// is only in such a group; NaN groups are skipped. When those groups
// hold more than a sixteenth of the rows, one sequential pass over x is
// cheaper than reading their rows.
SEXP dta_na_groups_tagged(SEXP x, SEXP key, SEXP loc) {
  const double *p = REAL_RO(x), *k = REAL_RO(key);
  R_xlen_t n = XLENGTH(x), groups = XLENGTH(key), missing = 0;
  for (R_xlen_t g = 0; g < groups; g++) {
    if (is_na(k[g])) missing += XLENGTH(VECTOR_ELT(loc, g));
  }
  if (!missing) return Rf_ScalarLogical(FALSE);
  unsigned found = 0;
  if (missing > n / 16) {
    for (R_xlen_t i = 0; i < n; i++) found |= tagged(p + i);
    return Rf_ScalarLogical(found != 0);
  }
  for (R_xlen_t g = 0; g < groups && !found; g++) {
    if (!is_na(k[g])) continue;
    SEXP rows = VECTOR_ELT(loc, g);
    const int *r = INTEGER_RO(rows);
    // R numbers rows from 1, C from 0: row r[i] is p[r[i] - 1].
    for (R_xlen_t i = 0, m = XLENGTH(rows); i < m; i++) found |= tagged(p + r[i] - 1);
  }
  return Rf_ScalarLogical(found != 0);
}
