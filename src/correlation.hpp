#ifndef CORR_INCLUDED_
#define CORR_INCLUDED_

#include "tmb_includes.hpp"

// Unstructured covariance.
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
    // result = get_toeplitz<T>(theta, n_visits);
    break;
  case auto_regressive_corr:
    // result = get_auto_regressive<T>(theta, n_visits);
    break;
  case compound_symmetry_corr:
    // result = get_compound_symmetry<T>(theta, n_visits);
    break;
  case ante_dependence_corr:
    // result = get_ante_dependence<T>(theta, n_visits);
    break;
  }
  return result;
}

// template <class Type>
// matrix<Type> get_select_matrix(const vector<int>& visits_i, const int& n_visits) {
//   matrix<Type> result = matrix<Type>::Zero(n_visits, visits_i.size());
//   for (int i = 0; i < visits_i.size(); i++) {
//     result(visits_i(i), i) = (Type) 1.0;
//   }
//   return result;
// }

template <class Type>
Eigen::SparseMatrix<Type> get_select_matrix(const vector<int>& visits_i, const int& n_visits) {
  Eigen::SparseMatrix<Type> result(visits_i.size(), n_visits);
  for (int i = 0; i < visits_i.size(); i++) {
    result.insert(i, visits_i(i)) = (Type) 1.0;
  }
  return result;
}


#endif
