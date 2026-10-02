#include "kr_comp.h"

using namespace Rcpp;
using std::string;
// Linear KR contracts Q before storing, retaining P in the fitting coordinates.
List get_pqr_contracted(List mmrm_fit, NumericVector theta, NumericMatrix w) {
  matrix<double> x = as_num_matrix_tmb(as<NumericMatrix>(mmrm_fit["x_matrix"]));
  matrix<double> coords = as_num_matrix_tmb(as<NumericMatrix>(mmrm_fit["coordinates"]));
  vector<double> weights = as_num_vector_tmb(sqrt(as<NumericVector>(mmrm_fit["weights_vector"])));
  vector<double> theta_v = as_num_vector_tmb(theta);
  IntegerVector starts = mmrm_fit["subject_zero_inds"];
  IntegerVector sizes = mmrm_fit["subject_n_visits"];
  IntegerVector groups = mmrm_fit["subject_groups"];
  int n_groups = mmrm_fit["n_groups"];
  int n_visits = mmrm_fit["n_visits"];
  bool spatial = as<int>(mmrm_fit["is_spatial_int"]) == 1;
  std::string cov_type = as<std::string>(mmrm_fit["cov_type"]);
  int k = theta.size() / n_groups;
  int p = x.cols();
  matrix<double> W = as_num_matrix_tmb(w);
  matrix<double> P = matrix<double>::Zero(p * theta.size(), p);
  matrix<double> Q_sum = matrix<double>::Zero(p, p);
  auto derivatives = derivatives_cache<double>(theta_v, n_groups, spatial, cov_type, n_visits, false);
  std::vector<kr_directions> directions;
  std::vector<std::map<std::vector<int>, kr_pattern>> patterns(n_groups);
  for (int g = 0; g < n_groups; ++g) {
    // Only same-group Q blocks are nonzero. Cross-group W is retained in S_P
    // downstream, using the original full W and the original-coordinate P.
    directions.emplace_back(matrix<double>(W.block(g * k, g * k, k, k)));
  }
  for (int i = 0; i < starts.size(); ++i) {
    int start = starts[i], m = sizes[i], g = groups[i] - 1;
    std::vector<int> visits(m);
    matrix<double> dist(0, 0);
    if (spatial) {
      dist = euclidean(matrix<double>(coords.block(start, 0, m, coords.cols())));
    } else {
      for (int j = 0; j < m; ++j) visits[j] = int(coords(start + j, 0));
    }
    auto accumulate = [&](const kr_pattern& pattern) {
      matrix<double> Xi = weights.segment(start, m).matrix().asDiagonal() * x.block(start, 0, m, p);
      for (int h = 0; h < k; ++h) {
        P.block((g * k + h) * p, 0, p, p) +=
          Xi.transpose() * pattern.inverse_d1.block(h * m, 0, m, m) * Xi;
        matrix<double> Z = pattern.inverse_directions.block(h * m, 0, m, m) * Xi;
        Q_sum += directions[g].signs(h) * Z.transpose() * pattern.sigma * Z;
      }
    };
    if (spatial) {
      // Spatial distances vary by subject; do not key them by visit indices.
      accumulate(kr_pattern(derivatives.cache[g].get(), visits, dist, directions[g].B));
    } else {
      auto found = patterns[g].find(visits);
      if (found == patterns[g].end()) {
        found = patterns[g].emplace(visits,
          kr_pattern(derivatives.cache[g].get(), visits, dist, directions[g].B)).first;
      }
      accumulate(found->second);
    }
  }
  return List::create(Named("P") = as_num_matrix_rcpp(P),
                      Named("Q") = R_NilValue, Named("R") = R_NilValue,
                      Named("S_Q") = as_num_matrix_rcpp(Q_sum));
}

// Obtain P,Q,R elements, or contracted linear components when W is supplied.
List get_pqr(List mmrm_fit, NumericVector theta, bool linear, Nullable<NumericMatrix> w) {
  if (w.isNotNull()) {
    return get_pqr_contracted(mmrm_fit, theta, NumericMatrix(w));
  }
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
  vector<double> theta_v = as_num_vector_tmb(theta);
  vector<double> G_sqrt = as_num_vector_tmb(sqrt(weights_vector));
  int n_theta = theta.size();
  int theta_size_per_group = n_theta / n_groups;
  int p = x.cols();
  matrix<double> P = matrix<double>::Zero(p * n_theta, p);
  matrix<double> Q = matrix<double>::Zero(p * theta_size_per_group * n_theta, p);
  matrix<double> R;
  if (!linear) {
    R = matrix<double>::Zero(p * theta_size_per_group * n_theta, p);
  }
  // Use map to hold these base class pointers (can also work for child class objects).
  auto derivatives_by_group = derivatives_cache<double>(theta_v, n_groups, is_spatial, cov_type, n_visits, !linear);
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
    matrix<double> sigma_inv, sigma_d2, sigma, sigma_inv_d1;

    sigma_inv = derivatives_by_group.cache[subject_group_i]->get_sigma_inverse(visit_i, dist_i);
    if (!linear) {
      sigma_d2 = derivatives_by_group.cache[subject_group_i]->get_sigma_derivative2(visit_i, dist_i);
    }
    sigma = derivatives_by_group.cache[subject_group_i]->get_sigma(visit_i, dist_i);
    sigma_inv_d1 = derivatives_by_group.cache[subject_group_i]->get_inverse_derivative(visit_i, dist_i);

    matrix<double> Xi = x_matrix.block(start_i, 0, n_visits_i, x_matrix.cols());
    auto gi_sqrt_root = G_sqrt.segment(start_i, n_visits_i).matrix().asDiagonal();
    for (int r = 0; r < theta_size_per_group; r ++) {
      auto Pi = Xi.transpose() * gi_sqrt_root * sigma_inv_d1.block(r * n_visits_i, 0, n_visits_i, n_visits_i) * gi_sqrt_root * Xi;
      P.block(r * p + theta_size_per_group * subject_group_i * p, 0, p, p) += Pi;
      for (int j = 0; j < theta_size_per_group; j++) {
        auto Qij = Xi.transpose() * gi_sqrt_root * sigma_inv_d1.block(r * n_visits_i, 0, n_visits_i, n_visits_i) * sigma * sigma_inv_d1.block(j * n_visits_i, 0, n_visits_i, n_visits_i) * gi_sqrt_root * Xi;
        // switch the order so that in the matrix partial(i) and partial(j) increase j first
        Q.block((r * theta_size_per_group + j + theta_size_per_group * theta_size_per_group * subject_group_i) * p, 0, p, p) += Qij;
        if (!linear) {
          auto Rij = Xi.transpose() * gi_sqrt_root * sigma_inv * sigma_d2.block((j * theta_size_per_group + r) * n_visits_i, 0, n_visits_i, n_visits_i) * sigma_inv * gi_sqrt_root * Xi;
          R.block((r * theta_size_per_group + j + theta_size_per_group * theta_size_per_group * subject_group_i) * p, 0, p, p) += Rij;
        }
      }
    }
  }
  List ret = List::create(
    Named("P") = as_num_matrix_rcpp(P),
    Named("Q") = as_num_matrix_rcpp(Q),
    Named("R") = R_NilValue
  );
  if (!linear) {
    ret["R"] = as_num_matrix_rcpp(R);
  }
  return ret;
}
