#include "utils.h"

// Quick helpers (with shortened function)
matrix<double> as_num_matrix_tmb(Rcpp::NumericMatrix x) {
  return as_matrix<matrix<double>, Rcpp::NumericMatrix>(x);
}

Rcpp::NumericMatrix as_num_matrix_rcpp(matrix<double> x) {
  return as_matrix<Rcpp::NumericMatrix, matrix<double>>(x);
}

vector<double> as_num_vector_tmb(Rcpp::NumericVector x) {
  return as_vector<vector<double>, Rcpp::NumericVector>(x);
}

Rcpp::NumericVector as_num_vector_rcpp(vector<double> x){
  return as_vector<Rcpp::NumericVector, vector<double>>(x);
}
