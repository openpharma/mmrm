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

NumericMatrix get_empirical(List mmrm_fit, NumericVector theta, NumericVector beta, NumericMatrix beta_vcov, bool jackknife);
RcppExport SEXP _mmrm_get_empirical(SEXP mmrm_fitSEXP, SEXP thetaSEXP, SEXP betaSEXP, SEXP beta_vcovSEXP, SEXP jackknifeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type mmrm_fit(mmrm_fitSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type theta(thetaSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type beta(betaSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type beta_vcov(beta_vcovSEXP);
    Rcpp::traits::input_parameter< bool >::type jackknife(jackknifeSEXP);
    rcpp_result_gen = Rcpp::wrap(get_empirical(mmrm_fit, theta, beta, beta_vcov, jackknife));
    return rcpp_result_gen;
END_RCPP
}

RcppExport SEXP run_testthat_tests(SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_mmrm_get_pqr", (DL_FUNC) &_mmrm_get_pqr, 2},
    {"_mmrm_get_empirical", (DL_FUNC) &_mmrm_get_empirical, 5},
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
