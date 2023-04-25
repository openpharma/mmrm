#ifndef COV_INCLUDED_
#define COV_INCLUDED_

#include "utils.h"

// Unstructured covariance:
// Cholesky factor.
template <class T>
matrix<T> get_unstructured(const vector<T>& theta, int n_visits) {
  vector<T> sd_values = exp(theta.head(n_visits));
  vector<T> lower_tri_chol_values = theta.tail(theta.size() - n_visits);
  matrix<T> covariance_lower_chol = matrix<T>::Zero(n_visits, n_visits);
  int k = 0;
  for(int i = 0; i < n_visits; i++) {
    covariance_lower_chol(i, i) = sd_values(i);
    for(int j = 0; j < i; j++){
      covariance_lower_chol(i, j) = sd_values(i) * lower_tri_chol_values(k++);
    }
  }
  return covariance_lower_chol;
}

// Ante-dependence:

// Correlation function.
template <class T>
struct corr_fun_ante_dependence : generic_corr_fun<T> {
  using generic_corr_fun<T>::generic_corr_fun;
  const T operator() (int i, int j) const {
    return this->corr_values.segment(j, i - j).prod();
  }
};
// Homogeneous Ante-dependence Cholesky factor.
template <class T>
matrix<T> get_ante_dependence(const vector<T>& theta, int n_visits) {
  T const_sd = exp(theta(0));
  corr_fun_ante_dependence<T> fun(theta.tail(n_visits - 1));
  matrix<T> ad_cor_mat_chol = get_corr_mat_chol(n_visits, fun);
  return const_sd * ad_cor_mat_chol;
}
// Heterogeneous Ante-dependence Cholesky factor.
template <class T>
matrix<T> get_ante_dependence_heterogeneous(const vector<T>& theta, int n_visits) {
  vector<T> sd_values = exp(theta.head(n_visits));
  corr_fun_ante_dependence<T> fun(theta.tail(n_visits - 1));
  return get_heterogeneous_cov(sd_values, fun);
}

// Toeplitz:

// Correlation function.
template <class T>
struct corr_fun_toeplitz : generic_corr_fun<T> {
  using generic_corr_fun<T>::generic_corr_fun;
  const T operator() (int i, int j) const {
    int index = (i - j) - 1;  // Note: We need to start at 0.
    return this->corr_values(index);
  }
};
// Homogeneous Toeplitz Cholesky factor.
template <class T>
matrix<T> get_toeplitz(const vector<T>& theta, int n_visits) {
  T const_sd = exp(theta(0));
  corr_fun_toeplitz<T> fun(theta.tail(n_visits - 1));
  matrix<T> toep_cor_mat_chol = get_corr_mat_chol(n_visits, fun);
  return const_sd * toep_cor_mat_chol;
}
// Heterogeneous Toeplitz Cholesky factor.
template <class T>
matrix<T> get_toeplitz_heterogeneous(const vector<T>& theta, int n_visits) {
  vector<T> sd_values = exp(theta.head(n_visits));
  corr_fun_toeplitz<T> fun(theta.tail(n_visits - 1));
  return get_heterogeneous_cov(sd_values, fun);
}

// Autoregressive:

// Correlation function.
template <class T>
struct corr_fun_autoregressive : generic_corr_fun<T> {
  using generic_corr_fun<T>::generic_corr_fun;
  const T operator() (int i, int j) const {
    T diff = T((i - j) * 1.0);
    return pow(this->corr_values(0), diff);  // rho^{|i-j|}
  }
};
// Homogeneous autoregressive Cholesky factor.
template <class T>
matrix<T> get_auto_regressive(const vector<T>& theta, int n_visits) {
  T const_sd = exp(theta(0));
  corr_fun_autoregressive<T> fun(theta.tail(1));
  matrix<T> ar1_cor_mat_chol = get_corr_mat_chol(n_visits, fun);
  return const_sd * ar1_cor_mat_chol;
}
// Heterogeneous autoregressive Cholesky factor.
template <class T>
matrix<T> get_auto_regressive_heterogeneous(const vector<T>& theta, int n_visits) {
  vector<T> sd_values = exp(theta.head(n_visits));
  corr_fun_autoregressive<T> fun(theta.tail(1));
  return get_heterogeneous_cov(sd_values, fun);
}

// Compound symmetry:

// Correlation function.
template <class T>
struct corr_fun_compound_symmetry : generic_corr_fun<T> {
  using generic_corr_fun<T>::generic_corr_fun;
  const T operator() (int i, int j) const {
    return this->corr_values(0);  // rho (constant)
  }
};
// Homogeneous compound symmetry Cholesky factor.
template <class T>
matrix<T> get_compound_symmetry(const vector<T>& theta, int n_visits) {
  T const_sd = exp(theta(0));
  corr_fun_compound_symmetry<T> fun(theta.tail(1));
  matrix<T> cs_cor_mat_chol = get_corr_mat_chol(n_visits, fun);
  return const_sd * cs_cor_mat_chol;
}
// Heterogeneous compound symmetry Cholesky factor.
template <class T>
matrix<T> get_compound_symmetry_heterogeneous(const vector<T>& theta, int n_visits) {
  vector<T> sd_values = exp(theta.head(n_visits));
  corr_fun_compound_symmetry<T> fun(theta.tail(1));
  return get_heterogeneous_cov(sd_values, fun);
}

// Spatial Exponential Cholesky factor.
template <class T>
matrix<T> get_spatial_exponential(const vector<T>& theta, const matrix<T>& distance) {
  T const_sd = exp(theta(0));
  T rho = invlogit(theta(1));
  matrix<T> expdist = exp(distance.array() * log(rho));
  matrix<T> result = expdist * const_sd;
  Eigen::LLT<Eigen::Matrix<T,Eigen::Dynamic,Eigen::Dynamic> > cov_i_chol(result);
  return cov_i_chol.matrixL();
}

// Creates a new correlation object dynamically.
template <class T>
matrix<T> get_covariance_lower_chol(const vector<T>& theta, int n_visits, std::string cov_type) {
  matrix<T> result;

  if (cov_type == "us") {
    result = get_unstructured<T>(theta, n_visits);
  } else if (cov_type == "toep") {
    result = get_toeplitz<T>(theta, n_visits);
  } else if (cov_type == "toeph") {
    result = get_toeplitz_heterogeneous<T>(theta, n_visits);
  } else if (cov_type == "ar1") {
    result = get_auto_regressive<T>(theta, n_visits);
  } else if (cov_type == "ar1h") {
    result = get_auto_regressive_heterogeneous<T>(theta, n_visits);
  } else if (cov_type == "ad") {
    result = get_ante_dependence<T>(theta, n_visits);
  } else if (cov_type == "adh") {
    result = get_ante_dependence_heterogeneous<T>(theta, n_visits);
  } else if (cov_type == "cs") {
    result = get_compound_symmetry<T>(theta, n_visits);
  } else if (cov_type == "csh") {
    result = get_compound_symmetry_heterogeneous<T>(theta, n_visits);
  } else {
    Rf_error(("Unknown covariance type '" + cov_type + "'.").c_str());
  }

  return result;
}

// Creates a new spatial covariance cholesky.
template <class T>
matrix<T> get_spatial_covariance_lower_chol(const vector<T>& theta, const matrix<T>& distance, std::string cov_type) {
  matrix<T> result;
  if (cov_type == "sp_exp") {
    result = get_spatial_exponential<T>(theta, distance);
  } else {
    Rf_error(("Unknown spatial covariance type '" + cov_type + "'.").c_str());
  }
  return result;
}

#endif
