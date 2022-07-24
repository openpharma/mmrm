#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Call calls */
extern SEXP run_testthat_tests();

static const R_CallMethodDef CallEntries[] = {
  {"run_testthat_tests", (DL_FUNC) &run_testthat_tests, 0},
  {NULL, NULL, 0}
};

void R_init_mmrm(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
