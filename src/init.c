// How R finds datakit's C functions.
//
// R code calls C through .Call(), as in `.Call(C_dta_na_tag, x)` in
// R/tagged.R. For that to work, R needs a list of the C functions it may
// call: each one's name, address and number of arguments. This file
// gives R that list when the package loads. The functions themselves
// are in tagged.c.
//
// Terms from R's C interface, used here and in tagged.c:
//
// - SEXP: the C type of every R value, whatever kind it is (a double
//   vector, a character vector, a list, NULL). It is a pointer to R's own
//   record of the value. The name stands for "S expression", from R's
//   roots in the S language. A function called by .Call() takes SEXPs
//   and returns one. TYPEOF(x) tells which kind of value it points to:
//   REALSXP for a double vector, INTSXP for integer, LGLSXP for logical,
//   STRSXP for character.
// - DllInfo: R's record of a shared library it has loaded, here
//   datakit.so (datakit.dll on Windows), which holds the compiled C.
// - DL_FUNC: a function pointer with no particular signature. The table
//   below stores every function as one, and R calls each with the
//   number of arguments given next to it.

#include <R.h>              // R's basic C interface
#include <Rinternals.h>     // SEXP and the functions that work on R values
#include <R_ext/Rdynload.h> // R_CallMethodDef, DllInfo and registration

// The functions R may call, defined in tagged.c. They are declared
// here so that the table below can take their addresses.
SEXP dta_na_tag(SEXP x);
SEXP dta_missing_rank(SEXP x);
SEXP dta_na_groups_tagged(SEXP x, SEXP key, SEXP loc);

// The list R reads: one row per function, giving the name R knows it
// by, its address and its number of arguments. The row of NULLs marks
// the end. In NAMESPACE, `useDynLib(datakit, .registration = TRUE,
// .fixes = "C_")` makes an R object for each row, named with the prefix
// C_ (C_dta_na_tag and so on), which the R code passes to .Call().
static const R_CallMethodDef routines[] = {
  {"dta_na_tag", (DL_FUNC) &dta_na_tag, 1},
  {"dta_missing_rank", (DL_FUNC) &dta_missing_rank, 1},
  {"dta_na_groups_tagged", (DL_FUNC) &dta_na_groups_tagged, 3},
  {NULL, NULL, 0}
};

// R runs this once, when it loads the shared library. It finds the
// function by its name: R_init_ followed by the package's name.
void R_init_datakit(DllInfo *dll) {
  // Hand R the list. The three NULLs are for the other ways of calling
  // compiled code (.C(), .Fortran() and .External()), which datakit
  // does not use.
  R_registerRoutines(dll, NULL, routines, NULL, NULL);
  // R may call only the functions in the list: it will not search the
  // library for any other name.
  R_useDynamicSymbols(dll, FALSE);
  // .Call() must be given the R object (C_dta_na_tag), not the name as
  // a string ("dta_na_tag"). The object carries the address, so R does
  // not look the name up on each call, and a call can reach only this
  // package's functions, never another package's of the same name.
  R_forceSymbols(dll, TRUE);
}
