#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Call calls */
extern SEXP run_testthat_tests();
extern SEXP MakeADFunObject(SEXP);
extern SEXP InfoADFunObject(SEXP);
extern SEXP EvalADFunObject(SEXP);
extern SEXP MakeDoubleFunObject(SEXP);
extern SEXP EvalDoubleFunObject(SEXP);
extern SEXP getParameterOrder(SEXP);
extern SEXP MakeADGradObject(SEXP);
extern SEXP MakeADHessObject2(SEXP);
extern SEXP usingAtomics();

static const R_CallMethodDef CallEntries[] = {
  {"run_testthat_tests", (DL_FUNC) &run_testthat_tests,  0},
  {"MakeADFunObject",    (DL_FUNC) &MakeADFunObject,     4},
  {"InfoADFunObject",    (DL_FUNC) &InfoADFunObject,     1},
  {"EvalADFunObject",    (DL_FUNC) &EvalADFunObject,     3},
  {"MakeDoubleFunObject",(DL_FUNC) &MakeDoubleFunObject, 3},
  {"EvalDoubleFunObject",(DL_FUNC) &EvalDoubleFunObject, 3},
  {"getParameterOrder",  (DL_FUNC) &getParameterOrder,   3},
  {"MakeADGradObject",   (DL_FUNC) &MakeADGradObject,    3},
  {"MakeADHessObject2",  (DL_FUNC) &MakeADHessObject2,   4},
  {"usingAtomics",       (DL_FUNC) &usingAtomics,        0},
  {NULL, NULL, 0}
};

void R_init_mmrm(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
