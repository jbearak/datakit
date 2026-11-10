#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

SEXP dta_na_tag(SEXP x);
SEXP dta_missing_rank(SEXP x);
SEXP dta_na_groups_tagged(SEXP x, SEXP key, SEXP loc);

static const R_CallMethodDef routines[] = {
  {"dta_na_tag", (DL_FUNC) &dta_na_tag, 1},
  {"dta_missing_rank", (DL_FUNC) &dta_missing_rank, 1},
  {"dta_na_groups_tagged", (DL_FUNC) &dta_na_groups_tagged, 3},
  {NULL, NULL, 0}
};

void R_init_datakit(DllInfo *dll) {
  R_registerRoutines(dll, NULL, routines, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}
