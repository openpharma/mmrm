#include "derivatives.h"
#include <unsupported/Eigen/MatrixFunctions>
#include <chrono>

using namespace Rcpp;
using std::string;
// Obtain the empirical given beta, beta_vcov, theta.
List get_empirical(List mmrm_data, NumericVector theta, NumericVector beta, NumericMatrix beta_vcov, int type) {
  NumericMatrix x = mmrm_data["x_matrix"];
  matrix<double> x_matrix = as_matrix(x);
  NumericVector y = mmrm_data["y_vector"];
  matrix<double> beta_vcov_matrix = as_matrix(beta_vcov);
  IntegerVector subject_zero_inds = mmrm_data["subject_zero_inds"];
  IntegerVector visits_zero_inds = mmrm_data["visits_zero_inds"];
  int n_subjects = mmrm_data["n_subjects"];
  int n_observations = x_matrix.rows();
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
  std::chrono::steady_clock::time_point s1 = std::chrono::steady_clock::now();
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
  std::chrono::steady_clock::time_point s2 = std::chrono::steady_clock::now();
  matrix<double> meat = matrix<double>::Zero(p, p);
  matrix<double> hx = x_matrix * beta_vcov_matrix * x_matrix.transpose();
  matrix<double> w = matrix<double>::Zero(n_observations, n_observations);
  matrix<double> chols = matrix<double>::Zero(n_observations, n_observations);
  matrix<double> phi = matrix<double>::Zero(n_observations, n_observations);
  std::chrono::steady_clock::time_point s3 = std::chrono::steady_clock::now();
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
    matrix<double> sigma_inv = derivatives_by_group[subject_group_i]->get_inverse(visit_i, dist_i);
    matrix<double> gi_sqrt_root = G_sqrt.segment(start_i, n_visits_i).matrix().asDiagonal();
    w.block(start_i, start_i, n_visits_i, n_visits_i) = gi_sqrt_root * sigma_inv * gi_sqrt_root;
    chols.block(start_i, start_i, n_visits_i, n_visits_i) = derivatives_by_group[subject_group_i]->get_inverse_chol(visit_i, dist_i);
    phi.block(start_i, start_i, n_visits_i, n_visits_i) = derivatives_by_group[subject_group_i]->get_sigma(visit_i, dist_i);
  }
  std::chrono::steady_clock::time_point s4 = std::chrono::steady_clock::now();
  matrix<double> weighted_phi = matrix<double>::Zero(n_observations, n_observations);
  weighted_phi = (1 / G_sqrt).matrix().asDiagonal() * phi * (1 / G_sqrt).matrix().asDiagonal();
  matrix<double> hxw = hx * w; // probably slow. use block calculation!
  matrix<double> i_hxw = matrix<double>::Identity(n_observations, n_observations) - hxw;
  matrix<double> g = matrix<double>::Zero(n_observations, p * n_subjects);
  std::chrono::steady_clock::time_point s5 = std::chrono::steady_clock::now();
  for (int i = 0; i < n_subjects; i++) {
    int start_i = subject_zero_inds[i];
    int n_visits_i = subject_n_visits[i];
    matrix<double> ai = matrix<double>::Identity(n_visits_i, n_visits_i);
    if (type == 1) { // 1 for jackknife
      ai = (ai - hxw.block(start_i, start_i, n_visits_i, n_visits_i)).inverse();
    } else if (type == 2) {
      matrix<double> di = chols.block(start_i, start_i, n_visits_i, n_visits_i);
      matrix<double> bi1 = di * i_hxw.block(start_i, 0, n_visits_i, n_observations);
      matrix<double> bi = bi1 * phi * bi1.transpose();
      matrix<double> bisq = matrix<double>(bi.completeOrthogonalDecomposition().pseudoInverse()).sqrt();
      ai = di.transpose() * bisq * di;
    }
    matrix<double> Xi = x_matrix.block(start_i, 0, n_visits_i, x_matrix.cols());
    matrix<double> wi = w.block(start_i, start_i, n_visits_i, n_visits_i);
    matrix<double> residual_i = residual.block(start_i, 0, n_visits_i, 1);
    matrix<double> xtwa = Xi.transpose() * wi * ai;
    matrix<double> z =  xtwa * residual_i;
    meat = meat + z * z.transpose();
    g.block(0, i * p, n_observations, p) = i_hxw.block(start_i, 0, n_visits_i, n_observations).transpose() * xtwa.transpose() * beta_vcov_matrix;
  }
  std::chrono::steady_clock::time_point s6 = std::chrono::steady_clock::now();
  matrix<double> gtvg = g.transpose() * weighted_phi * g; // very large matrix calculation? also use block?
  std::chrono::steady_clock::time_point s7 = std::chrono::steady_clock::now();
  for (int r = 0; r < n_groups; r++) {
    delete derivatives_by_group[r];
  }
  // beta_vcov already take gi into consideration;
  matrix<double> ret = beta_vcov_matrix * meat * beta_vcov_matrix;
  // Removed because this scale factor can be applied by user manually
  // not important.
  //if (jackknife) {
  //  ret = ret * (n_subjects - 1) / n_subjects;
  //}
  std::chrono::steady_clock::time_point s8 = std::chrono::steady_clock::now();

  std::cout << "Time difference = " << std::chrono::duration_cast<std::chrono::microseconds>(s2 - s1).count() << "[µs]" << std::endl;
  std::cout << "Time difference = " << std::chrono::duration_cast<std::chrono::microseconds>(s3 - s2).count() << "[µs]" << std::endl;
  std::cout << "Time difference = " << std::chrono::duration_cast<std::chrono::microseconds>(s4 - s3).count() << "[µs]" << std::endl;
  std::cout << "Time difference = " << std::chrono::duration_cast<std::chrono::microseconds>(s5 - s4).count() << "[µs]" << std::endl;
  std::cout << "Time difference = " << std::chrono::duration_cast<std::chrono::microseconds>(s6 - s5).count() << "[µs]" << std::endl;
  std::cout << "Time difference = " << std::chrono::duration_cast<std::chrono::microseconds>(s7 - s6).count() << "[µs]" << std::endl;
  std::cout << "Time difference = " << std::chrono::duration_cast<std::chrono::microseconds>(s8 - s7).count() << "[µs]" << std::endl;

  return List::create(
    Named("cov") = as_mv(ret),
    Named("df_mat") = as_mv(gtvg)
  );
}

