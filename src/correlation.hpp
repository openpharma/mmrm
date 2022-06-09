#ifndef TMB_INCLUDED_
#define TMB_INCLUDED_
#include <TMB.hpp>
#endif

#ifndef CORR_INCLUDED_
#define CORR_INCLUDED_

// Common interface for all correlation classes.
template <class T>
class Correlation {
public:
  // Virtual destructor.
  virtual ~Correlation() {};

  // Virtual methods.
  virtual T neg_log_dens(const vector<T>& x, const vector<int>& visits) = 0;

  int n_visits;                        // Number of visits, i.e. dimension of covariance.
  matrix<T> covariance_matrix;         // Covariance matrix.
  matrix<T> covariance_lower_chol;     // Lower triangular Cholesky factor of covariance.
};

// Unstructured correlation class.
template <class T>
class Unstructured : public Correlation<T> {
public:
  // Constructor.
  Unstructured(const vector<T>& theta, int n_visits_) {
    this->n_visits = n_visits_;

    vector<T> sd_values = exp(theta.head(this->n_visits));
    vector<T> lower_tri_chol_values = theta.tail(theta.size() - this->n_visits);
    density::UNSTRUCTURED_CORR_t<T> unscaled_neg_log_dmvnorm(lower_tri_chol_values);
    matrix<T> corr_mat = unscaled_neg_log_dmvnorm.cov();
    matrix<T> sd_mat = Eigen::DiagonalMatrix<T, Eigen::Dynamic>(this->n_visits);
    sd_mat.diagonal() = sd_values;
    this->covariance_matrix = sd_mat * corr_mat * sd_mat;

    this->covariance_lower_chol = sd_mat * unscaled_neg_log_dmvnorm.L_Sigma;
  }

  T neg_log_dens(const vector<T>& x, const vector<int>& visits) {
    return 0.0;
  }
};

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
Correlation<T>* get_correlation(const vector<T>& theta, int corr_type, int n_visits) {
  Correlation<T>* corr;
  switch (corr_type) {
  case unstructured_corr:
    corr = new Unstructured<T>(theta, n_visits);
    break;
  case toeplitz_corr:
    corr = 0;
    break;
  case auto_regressive_corr:
    corr = 0;
    break;
  case compound_symmetry_corr:
    corr = 0;
    break;
  }
  return corr;
}

template <class Type>
matrix<Type> get_select_matrix(const vector<int>& visits_i, const int& n_visits) {
  matrix<Type> result = matrix<Type>::Zero(n_visits, visits_i.size());
  for (int i = 0; i < visits_i.size(); i++) {
    result(visits_i(i), i) = (Type) 1.0;
  }
  return result;
}


#endif
