#ifndef CORR_INCLUDED_
#define CORR_INCLUDED_

#include "tmb_includes.h"
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

// Heterogeneous Ante-dependence:

// Correlation function.
template <class T>
struct corr_fun_ante_dependence : generic_corr_fun<T> {
  using generic_corr_fun<T>::generic_corr_fun;
  const T operator() (int i, int j) const {
    return this->corr_values.segment(j, i - j).prod();
  }
};
// Cholesky factor.
template <class T>
matrix<T> get_ante_dependence(const vector<T>& theta, int n_visits) {
  vector<T> sd_values = exp(theta.head(n_visits));
  corr_fun_ante_dependence<T> fun(theta.tail(n_visits - 1));
  return get_heterogeneous_cov(sd_values, fun);
}

// Heterogeneous Toeplitz:

// Correlation function.
template <class T>
struct corr_fun_toeplitz : generic_corr_fun<T> {
  using generic_corr_fun<T>::generic_corr_fun;
  const T operator() (int i, int j) const {
    int index = (i - j) - 1;  // Note: We need to start at 0.
    return this->corr_values(index);
  }
};
// Cholesky factor.
template <class T>
matrix<T> get_toeplitz(const vector<T>& theta, int n_visits) {
  vector<T> sd_values = exp(theta.head(n_visits));
  corr_fun_toeplitz<T> fun(theta.tail(n_visits - 1));
  return get_heterogeneous_cov(sd_values, fun);
}

// Coding of corr_type coming from the R side.
enum corr_type_code {
  unstructured_corr = 1,
  toeplitz_corr = 2,
  auto_regressive_corr = 3,
  compound_symmetry_corr = 4,
  ante_dependence_corr = 5
};

// Creates a new correlation object dynamically.
template <class T>
matrix<T> get_covariance_lower_chol(const vector<T>& theta, int n_visits, int corr_type) {
  matrix<T> result;
  switch (corr_type) {
  case unstructured_corr:
    result = get_unstructured<T>(theta, n_visits);
    break;
  case toeplitz_corr:
    result = get_toeplitz<T>(theta, n_visits);
    break;
  case auto_regressive_corr:
    // result = get_auto_regressive<T>(theta, n_visits);
    break;
  case compound_symmetry_corr:
    // result = get_compound_symmetry<T>(theta, n_visits);
    break;
  case ante_dependence_corr:
    result = get_ante_dependence<T>(theta, n_visits);
    break;
  }
  return result;
}

#endif
