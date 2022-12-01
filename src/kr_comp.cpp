#include "chol_derivative.h"

using namespace Rcpp;
using std::string;
// Obtain P,Q,R element from a mmrm fit, given theta.
List get_pqr(List mmrm_fit, NumericVector theta) {
  NumericMatrix x = mmrm_fit["x_matrix"];
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
  if (is_spatial) {
    Rf_error("Spatial Kenward-Roger not implemented yet!");
  }
  auto theta_v = as_vector(theta);
  std::string covtype_str = string(cov_type);
  auto G_sqrt = as_vector(sqrt(weights_vector));
  int n_theta = theta.size();
  int theta_size_per_group = n_theta / n_groups;
  std::map<int, chols<double>> mychol_by_group;
  for (int r = 0; r < n_groups; r++) {
    mychol_by_group[r] = chols<double>(vector<double>(theta_v.segment(r * theta_size_per_group, theta_size_per_group)), n_visits, cov_type);
  }
  int p = x.cols();
  matrix<double> P = matrix<double>::Zero(p * n_theta, p);
  matrix<double> Q = matrix<double>::Zero(p * theta_size_per_group * n_theta, p);
  matrix<double> R = matrix<double>::Zero(p * theta_size_per_group * n_theta, p);
  for (int i = 0; i < n_subjects; i++) {
    int start_i = subject_zero_inds[i];
    int n_visits_i = subject_n_visits[i];
    std::vector<int> visit_i(n_visits_i);
    for (int i = 0; i < n_visits_i; i++) {
      visit_i[i] = visits_zero_inds[i + start_i];
    }
    int subject_group_i = subject_groups[i] - 1;
    auto x_matrix = as_matrix(x);
    matrix<double> Xi = x_matrix.block(start_i, 0, n_visits_i, x_matrix.cols());
    auto sigma_inv = mychol_by_group[subject_group_i].get_inverse(visit_i);
    auto sigma_d1 = mychol_by_group[subject_group_i].get_sigma_derivative1(visit_i);
    auto sigma_d2 = mychol_by_group[subject_group_i].get_sigma_derivative2(visit_i);
    auto sigma = mychol_by_group[subject_group_i].get_sigma(visit_i);
    auto sigma_inv_d1 = mychol_by_group[subject_group_i].get_inverse_derivative(visit_i);
    auto gi_sqrt_root = G_sqrt.segment(start_i, n_visits_i).matrix().asDiagonal();
    for (int r = 0; r < theta_size_per_group; r ++) {
      auto Pi = Xi.transpose() * gi_sqrt_root * sigma_inv_d1.block(r * n_visits_i, 0, n_visits_i, n_visits_i) * gi_sqrt_root * Xi;
      P.block(r * p + theta_size_per_group * subject_group_i * p, 0, p, p) += Pi;
      for (int j = 0; j < theta_size_per_group; j++) {
        auto Qij = Xi.transpose() * gi_sqrt_root * sigma_inv_d1.block(r * n_visits_i, 0, n_visits_i, n_visits_i) * sigma * sigma_inv_d1.block(j * n_visits_i, 0, n_visits_i, n_visits_i) * gi_sqrt_root * Xi;
        // switch the order so that in the matrix partial(i) and partial(j) increase j first
        Q.block((r * theta_size_per_group + j + theta_size_per_group * theta_size_per_group * subject_group_i) * p, 0, p, p) += Qij;
        auto Rij = Xi.transpose() * gi_sqrt_root * sigma_inv * sigma_d2.block((j * theta_size_per_group + r) * n_visits_i, 0, n_visits_i, n_visits_i) * sigma_inv * gi_sqrt_root * Xi;
        R.block((r * theta_size_per_group + j + theta_size_per_group * theta_size_per_group * subject_group_i) * p, 0, p, p) += Rij;
      }
    }
  }
  return List::create(
    Named("P") = as_mv(P),
    Named("Q") = as_mv(Q),
    Named("R") = as_mv(R)
  );
}
