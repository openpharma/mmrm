#include "utils.h"
using namespace Rcpp;
// conversion between Rcpp data and eigen vector/matrix.
vector<double> as_vector(NumericVector input) {
  vector<double> ret(input.size());
  for (int i = 0; i < input.size(); i++) {
    ret(i) = input(i);
  }
  return ret;
}
vector<int> as_vector(IntegerVector input) {
  vector<int> ret(input.size());
  for (int i = 0; i < input.size(); i++) {
    ret(i) = input(i);
  }
  return ret;
}
NumericVector as_nv(vector<double> input) {
  NumericVector ret(input.size());
  for (int i = 0; i < input.size(); i++) {
    ret[i] = input(i);
  }
  return ret;
}

NumericMatrix as_mv(matrix<double> input) {
  NumericMatrix ret(input.rows(), input.cols());
  for (int i = 0; i < input.rows(); i++) {
    for (int j = 0; j < input.cols(); j++) {
      ret(i,j) = input(i,j);
    }
  }
  return ret;
}
matrix<double> as_matrix(NumericMatrix input) {
  matrix<double> ret(input.rows(), input.cols());
  for (int i = 0; i < input.rows(); i++) {
    for (int j = 0; j < input.cols(); j++) {
      ret(i,j) = input(i,j);
    }
  }
  return ret;
}
