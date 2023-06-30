#include <RcppEigen.h>
#include "utils.h"

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

List get_pqr(List mmrm_fit, NumericVector theta);
RcppExport SEXP _mmrm_get_pqr(SEXP mmrm_fit_SEXP, SEXP theta_SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type mmrm_fit(mmrm_fit_SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type theta(theta_SEXP);
    rcpp_result_gen = Rcpp::wrap(get_pqr(mmrm_fit, theta));
    return rcpp_result_gen;
END_RCPP
}

List get_jacobian(List mmrm_fit, NumericVector theta, NumericMatrix beta_vcov);
RcppExport SEXP _mmrm_get_jacobian(SEXP mmrm_fit_SEXP, SEXP theta_SEXP, SEXP beta_vcov_SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type mmrm_fit(mmrm_fit_SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type theta(theta_SEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type beta_vcov(beta_vcov_SEXP);
    rcpp_result_gen = Rcpp::wrap(get_jacobian(mmrm_fit, theta, beta_vcov));
    return rcpp_result_gen;
END_RCPP
}

List get_empirical(List mmrm_fit, NumericVector theta, NumericVector beta, NumericMatrix beta_vcov, std::string type);
RcppExport SEXP _mmrm_get_empirical(SEXP mmrm_fit_SEXP, SEXP theta_SEXP, SEXP beta_SEXP, SEXP beta_vcov_SEXP, SEXP type_SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type mmrm_fit(mmrm_fit_SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type theta(theta_SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type beta(beta_SEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type beta_vcov(beta_vcov_SEXP);
    Rcpp::traits::input_parameter< std::string >::type type(type_SEXP);
    rcpp_result_gen = Rcpp::wrap(get_empirical(mmrm_fit, theta, beta, beta_vcov, type));
    return rcpp_result_gen;
END_RCPP
}

List predict(List mmrm_fit, NumericVector theta, NumericVector beta, NumericMatrix beta_vcov);
RcppExport SEXP _mmrm_predict(SEXP mmrm_fit_SEXP, SEXP theta_SEXP, SEXP beta_SEXP, SEXP beta_vcov_SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type mmrm_fit(mmrm_fit_SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type theta(theta_SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type beta(beta_SEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type beta_vcov(beta_vcov_SEXP);
    rcpp_result_gen = Rcpp::wrap(predict(mmrm_fit, theta, beta, beta_vcov));
    return rcpp_result_gen;
END_RCPP
}


RcppExport SEXP run_testthat_tests(SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_mmrm_get_pqr", (DL_FUNC) &_mmrm_get_pqr, 2},
    {"_mmrm_get_jacobian", (DL_FUNC) &_mmrm_get_jacobian, 3},
    {"_mmrm_get_empirical", (DL_FUNC) &_mmrm_get_empirical, 5},
    {"_mmrm_predict", (DL_FUNC) &_mmrm_predict, 4},
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
