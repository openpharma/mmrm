#ifndef INIT_H
#define INIT_H

#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>
#include "tmb_includes.h" // for TMB_CALLDEFS

extern "C" {

/* .Call calls */
extern SEXP run_testthat_tests(SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"run_testthat_tests", (DL_FUNC) &run_testthat_tests,  1},
  TMB_CALLDEFS,
  {NULL, NULL, 0}
};

void R_init_mmrm(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  // C-callable routines.
#ifdef TMB_CCALLABLES
  TMB_CCALLABLES("mmrm");
#endif
}

}

#endif
