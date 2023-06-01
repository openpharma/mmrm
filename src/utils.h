#ifndef UTILS_INCLUDED_
#define UTILS_INCLUDED_
#include <Rcpp.h>
#define INCLUDE_RCPP
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

// Producing a sparse selection matrix from visits to select rows and columns from
// covariance matrix.
template <class Type>
Eigen::SparseMatrix<Type> get_select_matrix(const std::vector<int>& visits_i, const int& n_visits) {
  Eigen::SparseMatrix<Type> result(visits_i.size(), n_visits);
  for (std::size_t i = 0, max = visits_i.size(); i != max; ++i) {
    result.insert(i, visits_i[i]) = (Type) 1.0;
  }
  return result;
}
// Conversion from Rcpp vector/matrix to eigen vector/matrix
template <typename T1, typename T2>
T1 as_vector(T2 input) {
  T1 ret(input.size());
  for (int i = 0; i < input.size(); i++) {
    ret(i) = input(i);
  }
  return ret;
}

template <typename T1, typename T2>
T1 as_matrix(T2 input) {
  T1 ret(input.rows(), input.cols());
  for (int i = 0; i < input.rows(); i++) {
    for (int j = 0; j < input.cols(); j++) {
      ret(i,j) = input(i,j);
    }
  }
  return ret;
}

template <typename T>
T segment(T input, int start, int n) {
  T ret(n);
  for (int i = 0, j = start; i < n; i++, j++) {
    ret(i) = input(j);
  }
  return ret;
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

// Element wise power function of a matrix
template <class T>
Eigen::Matrix<T, -1, -1> cpow(const Eigen::Matrix<T, -1, -1> & input, double p) {
  Eigen::Matrix<T, -1, -1> ret = Eigen::Matrix<T, -1, -1>(input.rows(), input.cols());
  for (int i = 0; i < ret.rows(); i ++) {
    for (int j = 0; j < ret.cols(); j++) {
      ret(i, j) = std::pow(input(i, j), p);
    }
  }
  return ret;
}

// Convert Eigen matrix to tmb matrix
template<typename T>
matrix<T> convert_eigen(const Eigen::Matrix<T, -1, -1> &input) {
  matrix<T> ret = matrix<T>::Zero(input.rows(), input.cols());
  for (int i = 0; i < input.rows(); i++) {
    for (int j = 0; j < input.cols(); j++) {
      ret(i, j) = input(i, j);
    }
  }
  return(ret);
}
// Convert tmb matrix to Eigen matrix
template<typename T>
Eigen::Matrix<T, -1, -1> convert_tmb(const matrix<T> &input) {
  Eigen::Matrix<T, -1, -1> eigen_mat(input.rows(), input.cols());
  for (int i = 0; i < input.rows(); i++) {
    for (int j = 0; j < input.cols(); j++) {
      eigen_mat(i, j) = input(i, j);
    }
  }
  return eigen_mat;
}

// Calculate the square root of the pseudo inverse of a matrix
// adapted from the method for calculating the pseudo-Inverse as recommended by the Eigen developers
template<typename T>
matrix<T> pseudoInverseSqrt(const matrix<T> &input, double epsilon = std::numeric_limits<double>::epsilon()) {
  Eigen::Matrix<T, -1, -1> eigen_mat = convert_tmb(input);
	Eigen::JacobiSVD< Eigen::Matrix<T, -1, -1> > svd(eigen_mat ,Eigen::ComputeFullU | Eigen::ComputeFullV);
	double tolerance = epsilon * std::max(input.cols(), input.rows()) *svd.singularValues().array().abs()(0);
  auto singular_vals = Matrix<T,-1,-1>((svd.singularValues().array() > tolerance).select(svd.singularValues().array().inverse(), 0).matrix());
	Eigen::Matrix<T, -1, -1> ret_eigen = svd.matrixV() *  cpow(singular_vals, 0.5).asDiagonal() * svd.matrixU().adjoint();
  return convert_eigen(ret_eigen);
}

// Quick helpers of conversion
matrix<double> as_num_matrix_tmb(Rcpp::NumericMatrix x);
Rcpp::NumericMatrix as_num_matrix_rcpp(matrix<double> x);
vector<double> as_num_vector_tmb(Rcpp::NumericVector x);
Rcpp::NumericVector as_num_vector_rcpp(vector<double> x);
#endif
