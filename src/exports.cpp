#include <RcppEigen.h>
#include "utils.h"

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

List get_pqr(List mmrm_fit, NumericVector theta);
RcppExport SEXP _mmrm_get_pqr(SEXP mmrm_fitSEXP, SEXP thetaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type mmrm_fit(mmrm_fitSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type theta(thetaSEXP);
    rcpp_result_gen = Rcpp::wrap(get_pqr(mmrm_fit, theta));
    return rcpp_result_gen;
END_RCPP
}

RcppExport SEXP run_testthat_tests(SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_mmrm_get_pqr", (DL_FUNC) &_mmrm_get_pqr, 2},
    {"run_testthat_tests", (DL_FUNC) &run_testthat_tests, 1},
    TMB_CALLDEFS,
    {NULL, NULL, 0}
};

RcppExport void R_init_mmrm(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    #ifdef TMB_CCALLABLES
    TMB_CCALLABLES("mmrm");
    #endif
}
