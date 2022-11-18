#include <RcppEigen.h>
#include <Rcpp.h>
#define INCLUDE_RCPP
#include "tmb_includes.h" // for TMB_CALLDEFS

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// List get_pqr(NumericMatrix x, IntegerVector subject_zero_inds, IntegerVector visits_zero_inds, int n_subjects, IntegerVector subject_n_visits, int n_visits, String cov_type, bool is_spatial, NumericVector theta);
// RcppExport SEXP _mmrm_get_pqr(SEXP xSEXP, SEXP subject_zero_indsSEXP, SEXP visits_zero_indsSEXP, SEXP n_subjectsSEXP, SEXP subject_n_visitsSEXP, SEXP n_visitsSEXP, SEXP cov_typeSEXP, SEXP is_spatialSEXP, SEXP thetaSEXP) {
// BEGIN_RCPP
//     Rcpp::RObject rcpp_result_gen;
//     Rcpp::RNGScope rcpp_rngScope_gen;
//     Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
//     Rcpp::traits::input_parameter< IntegerVector >::type subject_zero_inds(subject_zero_indsSEXP);
//     Rcpp::traits::input_parameter< IntegerVector >::type visits_zero_inds(visits_zero_indsSEXP);
//     Rcpp::traits::input_parameter< int >::type n_subjects(n_subjectsSEXP);
//     Rcpp::traits::input_parameter< IntegerVector >::type subject_n_visits(subject_n_visitsSEXP);
//     Rcpp::traits::input_parameter< int >::type n_visits(n_visitsSEXP);
//     Rcpp::traits::input_parameter< String >::type cov_type(cov_typeSEXP);
//     Rcpp::traits::input_parameter< bool >::type is_spatial(is_spatialSEXP);
//     Rcpp::traits::input_parameter< NumericVector >::type theta(thetaSEXP);
//     rcpp_result_gen = Rcpp::wrap(get_pqr(x, subject_zero_inds, visits_zero_inds, n_subjects, subject_n_visits, n_visits, cov_type, is_spatial, theta));
//     return rcpp_result_gen;
// END_RCPP
// }

RcppExport SEXP run_testthat_tests(SEXP);

static const R_CallMethodDef CallEntries[] = {
    //{"_mmrm_get_pqr", (DL_FUNC) &_mmrm_get_pqr, 9},
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
