// .a to .z in doubles: R's NA (a NaN whose low word is 1954) with the
// letter's ASCII code in bits 32 to 39, the bits haven and dtaparser
// use. na_tag() and the grouping of `:=` read them here.

#include <stdint.h>
#include <string.h>
#include <R.h>
#include <Rinternals.h>

// The letter of a tagged NA, or 0 for a number, NA or another NaN.
static inline char letter_of(double x) {
  uint64_t bits;
  memcpy(&bits, &x, sizeof bits);
  unsigned tag = (unsigned) ((bits >> 32) & 0xff);
  return ISNAN(x) && (uint32_t) bits == 1954 && tag - 'a' < 26u ? (char) tag : 0;
}

// 1 if the double's bits are a tagged NA. Branchless (& and an unsigned
// range test), so the loops below vectorise or at least do not
// mispredict.
static inline unsigned tagged(const double *x) {
  uint64_t bits;
  memcpy(&bits, x, sizeof bits);
  return (((bits >> 52) & 0x7ff) == 0x7ff) & ((uint32_t) bits == 1954) &
         ((uint32_t) ((bits >> 32) & 0xff) - 'a' < 26u);
}

// R_IsNA(), inlined, since it runs once per group: a NaN whose low
// word is 1954, true for NA and .a to .z, false for NaN.
static inline int is_na(double x) {
  uint64_t bits;
  memcpy(&bits, &x, sizeof bits);
  return ISNAN(x) && (uint32_t) bits == 1954;
}

// The letter of each element of a double vector ("a" to "z"), NA where
// it is a number, NA or NaN. Names are left to the R caller.
SEXP dta_na_tag(SEXP x) {
  if (TYPEOF(x) != REALSXP) Rf_error("na_tag: x must be a double vector");
  R_xlen_t n = XLENGTH(x);
  const double *p = REAL_RO(x);
  SEXP out = PROTECT(Rf_allocVector(STRSXP, n));
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
      o[i] = c ? c - 'a' + 2 : ISNAN(p[i]);
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
  default:
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
    for (R_xlen_t i = 0, m = XLENGTH(rows); i < m; i++) found |= tagged(p + r[i] - 1);
  }
  return Rf_ScalarLogical(found != 0);
}
