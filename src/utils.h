#ifndef UTILS_INCLUDED_
#define UTILS_INCLUDED_
#include <Rcpp.h>
#define INCLUDE_RCPP
#include "tmb_includes.h"

#define as_num_matrix_tmb as_matrix<matrix<double>, NumericMatrix>
#define as_num_matrix_rcpp as_matrix<NumericMatrix, matrix<double>>
#define as_num_vector_tmb as_vector<vector<double>, NumericVector>
#define as_num_vector_rcpp as_vector<NumericVector, vector<double>>

// Obtain submatrix from index

template <typename T1, typename T2>
T1 subset_matrix(T1 input, T2 index1, T2 index2) {
  #if EIGEN_VERSION_AT_LEAST(3,4,0)
    T1 ret = input(index1, index2);
  #else
    T1 ret(index1.size(), index2.size());
    for (int i = 0; i < index1.size(); i++) {
      for (int j = 0; j < index2.size(); j++) {
        ret(i, j) = input(index1[i], index2[j]);
      }
    }
  #endif
  return ret;
}

template <typename T1, typename T2>
T1 subset_matrix(T1 input, T2 index1) {
  #if EIGEN_VERSION_AT_LEAST(3,4,0)
    T1 ret = input(index1, Eigen::placeholders::all);
  #else
    T1 ret(index1.size(), input.cols());
    for (int i = 0; i < index1.size(); i++) {
      for (int j = 0; j < input.cols(); j++) {
        ret(i, j) = input(index1[i], j);
      }
    }
  #endif
  return ret;
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

// Calculate the square root of the pseudo inverse of a matrix
// adapted from the method for calculating the pseudo-Inverse as recommended by the Eigen developers
template<typename T>
matrix<T> pseudoInverseSqrt(const matrix<T> &input, double epsilon = std::numeric_limits<double>::epsilon()) {
  Eigen::Matrix<T, -1, -1> eigen_mat = as_matrix<Eigen::Matrix<T, -1, -1>, matrix<T>>(input);
	Eigen::JacobiSVD< Eigen::Matrix<T, -1, -1> > svd(eigen_mat ,Eigen::ComputeFullU | Eigen::ComputeFullV);
	double tolerance = epsilon * std::max(input.cols(), input.rows()) *svd.singularValues().array().abs()(0);
  auto singular_vals = Matrix<T,-1,-1>((svd.singularValues().array() > tolerance).select(svd.singularValues().array().inverse(), 0).matrix());
	Eigen::Matrix<T, -1, -1> ret_eigen = svd.matrixV() *  cpow(singular_vals, 0.5).asDiagonal() * svd.matrixU().adjoint();
  return as_matrix<matrix<T>, Eigen::Matrix<T, -1, -1>>(ret_eigen);
}

#endif
