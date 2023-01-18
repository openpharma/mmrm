#include "derivatives.h"

using namespace Rcpp;
using std::string;
// Obtain the empirical given beta, beta_vcov, theta.
NumericMatrix get_empirical(List mmrm_data, NumericVector theta, NumericVector beta, NumericMatrix beta_vcov, bool jackknife) {
  NumericMatrix x = mmrm_data["x_matrix"];
  matrix<double> x_matrix = as_matrix(x);
  NumericVector y = mmrm_data["y_vector"];
  matrix<double> beta_vcov_matrix = as_matrix(beta_vcov);
  IntegerVector subject_zero_inds = mmrm_data["subject_zero_inds"];
  IntegerVector visits_zero_inds = mmrm_data["visits_zero_inds"];
  int n_subjects = mmrm_data["n_subjects"];
  IntegerVector subject_n_visits = mmrm_data["subject_n_visits"];
  int n_visits = mmrm_data["n_visits"];
  String cov_type = mmrm_data["cov_type"];
  int is_spatial_int = mmrm_data["is_spatial_int"];
  bool is_spatial = is_spatial_int == 1;
  int n_groups = mmrm_data["n_groups"];
  IntegerVector subject_groups = mmrm_data["subject_groups"];
  NumericVector weights_vector = mmrm_data["weights_vector"];
  NumericMatrix coordinates = mmrm_data["coordinates"];
  matrix<double> coords = as_matrix(coordinates);
  matrix<double> beta_m = as_vector(beta).matrix();
  vector<double> theta_v = as_vector(theta);
  matrix<double> fitted = x_matrix * beta_m;
  matrix<double> y_matrix = as_vector(y).matrix();
  matrix<double> residual = y_matrix - fitted;
  vector<double> G_sqrt = as_vector(sqrt(weights_vector));
  int n_theta = theta.size();
  int theta_size_per_group = n_theta / n_groups;
  int p = x.cols();
  // Use map to hold these base class pointers (can also work for child class objects).
  std::map<int, derivatives_base<double>*> derivatives_by_group;
  for (int r = 0; r < n_groups; r++) {
    // in loops using new keyword is required so that the objects stays on the heap
    // otherwise this will be destroyed and you will get unexpected result
    if (is_spatial) {
      derivatives_by_group[r] = new derivatives_sp_exp<double>(vector<double>(theta_v.segment(r * theta_size_per_group, theta_size_per_group)));
    } else {
      derivatives_by_group[r] = new derivatives_nonspatial<double>(vector<double>(theta_v.segment(r * theta_size_per_group, theta_size_per_group)), n_visits, cov_type);
    }
  }
  matrix<double> meat = matrix<double>::Zero(beta_vcov_matrix.rows(), beta_vcov_matrix.cols());
  
  for (int i = 0; i < n_subjects; i++) {
    int start_i = subject_zero_inds[i];
    int n_visits_i = subject_n_visits[i];
    std::vector<int> visit_i(n_visits_i);
    matrix<double> dist_i(n_visits_i, n_visits_i);
    if (!is_spatial) {
      for (int i = 0; i < n_visits_i; i++) {
        visit_i[i] = visits_zero_inds[i + start_i];
      }
    } else {
      dist_i = euclidean(matrix<double>(coords.block(start_i, 0, n_visits_i, coordinates.cols())));
    }
    int subject_group_i = subject_groups[i] - 1;
    matrix<double> sigma_inv_chol = derivatives_by_group[subject_group_i]->get_inverse_chol(visit_i, dist_i);
    matrix<double> Xi = x_matrix.block(start_i, 0, n_visits_i, x_matrix.cols());
    matrix<double> residual_i = residual.block(start_i, 0, n_visits_i, 1);
    matrix<double> gi_sqrt_root = G_sqrt.segment(start_i, n_visits_i).matrix().asDiagonal();
    matrix<double> gi_simga_inv_chol = gi_sqrt_root * sigma_inv_chol;
    matrix<double> xt_gi_simga_inv_chol = Xi.transpose() * gi_simga_inv_chol;
    matrix<double> identity(n_visits_i, n_visits_i);
    identity.setIdentity();
    if (jackknife) {
      identity = identity - xt_gi_simga_inv_chol.transpose() * beta_vcov_matrix * xt_gi_simga_inv_chol;
    }
    matrix<double> z =  xt_gi_simga_inv_chol * identity.inverse() * gi_simga_inv_chol.transpose() * residual_i;
    meat = meat + z * z.transpose();
  }
  for (int r = 0; r < n_groups; r++) {
    delete derivatives_by_group[r];
  }
  // beta_vcov already take gi into consideration;
  matrix<double> ret = beta_vcov_matrix * meat * beta_vcov_matrix;
  if (jackknife) {
    ret = ret * (n_subjects - 1) / n_subjects;
  }
  return as_mv(ret);
}

