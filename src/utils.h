#ifndef UTILS_INCLUDED_
#define UTILS_INCLUDED_

#include "tmb_includes.h"

// Producing a sparse selection matrix to select rows and columns from
// covariance matrix.
template <class Type>
Eigen::SparseMatrix<Type> get_select_matrix(const vector<int>& visits_i, const int& n_visits) {
  Eigen::SparseMatrix<Type> result(visits_i.size(), n_visits);
  for (int i = 0; i < visits_i.size(); i++) {
    result.insert(i, visits_i(i)) = (Type) 1.0;
  }
  return result;
}

// Calculate tcrossprod(lower_chol) = lower_chol * t(lower_chol).
// If complete, then adds the upper triangular part to the result as well.
// By default only the lower triangular part is populated, as this should be
// sufficient for downstream use of the result in most cases.
template <class Type>
matrix<Type> tcrossprod(const matrix<Type>& lower_chol, bool complete = false) {
  int n = lower_chol.rows();
  matrix<Type> result = matrix<Type>::Zero(n, n);
  result.template selfadjointView<Eigen::Lower>().rankUpdate(lower_chol);
  if (complete) {
    result.template triangularView<Eigen::Upper>() = result.transpose();
  }
  return result;
}

// Calculate crossprod(x) = t(x) * x.
// Only the lower triangular part is populated, as this should be
// sufficient for downstream use of the result in most cases.
// Note that x does not need to be symmetric or square.
template <class Type>
matrix<Type> crossprod(const matrix<Type>& x) {
  int n = x.cols();
  matrix<Type> result = matrix<Type>::Zero(n, n);
  result.template selfadjointView<Eigen::Lower>().rankUpdate(x.transpose());
  return result;
}

// Mapping from real values to correlation parameters in (-1, 1).
template <class T>
vector<T> map_to_cor(const vector<T>& theta) {
  return theta / sqrt(T(1.0) + theta * theta);
}

// Generic correlation function class containing and initializing correlation
// values from variance parameters theta.
template <class T>
struct generic_corr_fun {
  const vector<T> corr_values;

  generic_corr_fun(const vector<T>& theta) :
    corr_values(map_to_cor(theta)) {}
};

// Correlation function based Cholesky factor of correlation matrix.
// This is used directly for homogeneous covariance matrices.
template <class T, template<class> class F>
matrix<T> get_corr_mat_chol(int n_visits, const F<T>& corr_fun) {
  matrix<T> correlation(n_visits, n_visits);
  correlation.setIdentity();
  for(int i = 0; i < n_visits; i++) {
    for(int j = 0; j < i; j++){
      correlation(i, j) = corr_fun(i, j);
    }
  }
  Eigen::LLT<Eigen::Matrix<T,Eigen::Dynamic,Eigen::Dynamic> > correlation_chol(correlation);
  matrix<T> L = correlation_chol.matrixL();
  return L;
}

// Heterogeneous covariance matrix calculation given vector of standard deviations (sd_values)
// and a correlation function (corr_fun).
template <class T, template<class> class F>
matrix<T> get_heterogeneous_cov(const vector<T>& sd_values, const F<T>& corr_fun) {
  matrix<T> correlation_chol = get_corr_mat_chol(sd_values.size(), corr_fun);
  Eigen::DiagonalMatrix<T,Eigen::Dynamic,Eigen::Dynamic> D = sd_values.matrix().asDiagonal();
  matrix<T> result = D * correlation_chol;
  return result;
}

// Obtain the Euclidean distance
template <class T>
matrix<T> euclidean(const matrix<T>& coordinates) {
  matrix<T> result(coordinates.rows(), coordinates.rows());
  for (int i = 0; i < coordinates.rows(); i++) {
    result(i, i) = 0;
    for (int j = 0; j < i; j ++) {
      vector<T> diff = coordinates.row(i) - coordinates.row(j);
      T d = sqrt((diff * diff).sum());
      result(i, j) = d;
      result(j, i) = d;
    }
  }
  return result;
}

#endif
