#include <TMB.hpp>

template <class T>
matrix<T> covariance_fun(vector<T> theta) {
  int n_theta = theta.size();
  int n_visits = int(floor(sqrt(2.0 * n_theta)));
  vector<T> sd_values = exp(theta.head(n_visits));
  vector<T> lower_tri_chol_values = theta.tail(n_theta - n_visits);
  density::UNSTRUCTURED_CORR_t<T> unscaled_neg_log_dmvnorm(lower_tri_chol_values);
  matrix<T> corr_mat = unscaled_neg_log_dmvnorm.cov();
  matrix<T> sd_mat = Eigen::DiagonalMatrix<T, Eigen::Dynamic>(n_visits);
  sd_mat.diagonal() = sd_values;
  matrix<T> result = sd_mat * corr_mat * sd_mat;
  return result;
}

template<class Type>
Type objective_function<Type>::operator() ()
{
  Type neg_log_lik = 0.0;

  return neg_log_lik;
}
