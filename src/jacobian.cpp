#include "derivatives.h"

using namespace Rcpp;
using std::string;

// Obtain Jacobian from a mmrm fit, given theta.
List get_jacobian(List mmrm_fit, NumericVector theta, NumericMatrix beta_vcov) {
  NumericMatrix x = mmrm_fit["x_matrix"];
  matrix<double> x_matrix = as_num_matrix_tmb(x);
  IntegerVector subject_zero_inds = mmrm_fit["subject_zero_inds"];
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
  matrix<double> coords = as_num_matrix_tmb(coordinates);
  matrix<double> beta_vcov_m = as_num_matrix_tmb(beta_vcov);
  vector<double> theta_v = as_num_vector_tmb(theta);
  vector<double> G_sqrt = as_num_vector_tmb(sqrt(weights_vector));
  int n_theta = theta.size();
  int theta_size_per_group = n_theta / n_groups;
  int p = x.cols();
  matrix<double> P = matrix<double>::Zero(p * n_theta, p);
  // Use map to hold these base class pointers (can also work for child class objects).
  auto derivatives_by_group = cache_obj<double, derivatives_base<double>, derivatives_sp_exp<double>, derivatives_nonspatial<double>>(theta_v, n_groups, is_spatial, cov_type, n_visits);
  for (int i = 0; i < n_subjects; i++) {
    int start_i = subject_zero_inds[i];
    int n_visits_i = subject_n_visits[i];
    std::vector<int> visit_i(n_visits_i);
    matrix<double> dist_i(n_visits_i, n_visits_i);
    if (!is_spatial) {
      for (int i = 0; i < n_visits_i; i++) {
        visit_i[i] = int(coordinates(i + start_i, 0));
      }
    } else {
      dist_i = euclidean(matrix<double>(coords.block(start_i, 0, n_visits_i, coordinates.cols())));
    }
    int subject_group_i = subject_groups[i] - 1;
    matrix<double> sigma_inv_d1 = derivatives_by_group.cache[subject_group_i]->get_inverse_derivative(visit_i, dist_i);

    matrix<double> Xi = x_matrix.block(start_i, 0, n_visits_i, x_matrix.cols());
    auto gi_sqrt_root = G_sqrt.segment(start_i, n_visits_i).matrix().asDiagonal();
    for (int r = 0; r < theta_size_per_group; r ++) {
      auto Pi = Xi.transpose() * gi_sqrt_root * sigma_inv_d1.block(r * n_visits_i, 0, n_visits_i, n_visits_i) * gi_sqrt_root * Xi;
      P.block(r * p + theta_size_per_group * subject_group_i * p, 0, p, p) += Pi;
    }
  }
  if (Rcpp::any(Rcpp::is_infinite(as_num_matrix_rcpp(P)))) {
    stop("Jacobian is not finite. The model can be over-parameterized.");
  }
  auto ret = List::create();
  for (int i = 0; i < n_theta; i++) {
    // the P is derivative of (XWX), the covariance is (XWX)^{-1}.
    ret.push_back(as_num_matrix_rcpp(-beta_vcov_m * P.block(i * p, 0, p, p) * beta_vcov_m));
  }
  return ret;
}
