#include "derivatives.h"

using namespace Rcpp;
using std::string;
// Obtain the empirical given beta, beta_vcov, theta.
List get_empirical(List mmrm_fit, NumericVector theta, NumericVector beta, NumericMatrix beta_vcov, bool jackknife) {
  NumericMatrix x = mmrm_fit["x_matrix"];
  auto x_matrix = as_matrix(x);
  NumericVector y = mmrm_fit["y_vector"];
  auto beta_vcov_matrix = as_matrix(beta_vcov);
  IntegerVector subject_zero_inds = mmrm_fit["subject_zero_inds"];
  IntegerVector visits_zero_inds = mmrm_fit["visits_zero_inds"];
  int n_subjects = mmrm_fit["n_subjects"];
  IntegerVector subject_n_visits = mmrm_fit["subject_n_visits"];
  int n_visits = mmrm_fit["n_visits"];
  String cov_type = mmrm_fit["cov_type"];
  int is_spatial_int = mmrm_fit["is_spatial_int"];
  bool is_spatial = is_spatial_int == 1;
  int n_groups = mmrm_fit["n_groups"];
  IntegerVector subject_groups = mmrm_fit["subject_groups"];
  NumericVector weights_vector = mmrm_fit["weights_vector"];
  NumericMatrix coordinates = mmrm_fit["coordinates"];
  auto coords = as_matrix(coordinates);
  auto beta_m = as_vector(beta).matrix();
  auto theta_v = as_vector(theta);
  auto fitted = x_matrix * beta_m;
  auto residual = as_vector(y).matrix() - fitted;
  auto G_sqrt = as_vector(sqrt(weights_vector));
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
    for (int i = 0; i < n_visits_i; i++) {
      visit_i[i] = visits_zero_inds[i + start_i];
    }
    dist_i = euclidean(matrix<double>(coords.block(start_i, 0, n_visits_i, coordinates.cols())));
    int subject_group_i = subject_groups[i] - 1;
    
    matrix<double> sigma_inv_chol = derivatives_by_group[subject_group_i]->get_inverse_chol(visit_i, dist_i);
    matrix<double> Xi = x_matrix.block(start_i, 0, n_visits_i, x_matrix.cols());
    matrix<double> residual_i = residual.block(start_i, 1, n_visits_i, 1);
    auto gi_sqrt_root = G_sqrt.segment(start_i, n_visits_i).matrix().asDiagonal();
    auto gi_simga_inv_chol = gi_sqrt_root * sigma_inv_chol;
    auto xt_gi_simga_inv_chol = Xi.transpose() * gi_simga_inv_chol;
    matrix<double> identity(beta_vcov_matrix.rows(), beta_vcov_matrix.cols());
    identity.setIdentity();
    if (jackknife) {
      identity = identity - xt_gi_simga_inv_chol * beta_vcov_matrix * xt_gi_simga_inv_chol.transpose();
    } 
    auto z =  xt_gi_simga_inv_chol * identity * gi_simga_inv_chol.transpose() * residual_i;
    meat = meat + z * z.transpose();
  }
  for (int r = 0; r < n_groups; r++) {
    delete derivatives_by_group[r];
  }
  // beta_vcov already take gi into consideration;
  auto ret = beta_vcov_matrix * meat * beta_vcov_matrix;
  return List::create(
    Named("empirical") = as_mv(ret)
  );
}

