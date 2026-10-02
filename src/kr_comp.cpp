#include "kr_comp.h"

using namespace Rcpp;
using std::string;
// Linear KR contracts Q before storing it, retaining P. See "Contracted linear
// covariance adjustment" in vignettes/kenward.Rmd.
List get_pqr_contracted(List mmrm_fit, NumericVector theta, NumericMatrix w) {
  kr_data data(mmrm_fit, theta);
  int k = data.theta_per_group;
  int p = data.p;
  matrix<double> W = as_num_matrix_tmb(w);
  // Non-finite W (a convergence problem that mmrm() only warns about) yields
  // a non-finite S_Q, as the pairwise contraction would.
  bool finite_w = W.allFinite();
  matrix<double> P = matrix<double>::Zero(p * data.n_theta, p);
  matrix<double> Q_sum = matrix<double>::Zero(p, p);
  auto derivatives = data.derivatives(false);
  std::vector<kr_directions> directions;
  std::vector<std::map<std::vector<int>, kr_pattern>> patterns(data.n_groups);
  for (int g = 0; g < data.n_groups; ++g) {
    // Only same-group Q blocks are nonzero. Cross-group W enters S_P in
    // h_var_adj_contracted(), using the full W.
    matrix<double> w_g = finite_w ? matrix<double>(W.block(g * k, g * k, k, k)) : matrix<double>(matrix<double>::Zero(k, k));
    directions.emplace_back(w_g);
  }
  for (int i = 0; i < data.n_subjects; ++i) {
    kr_subject subject = data.subject(i);
    int start = subject.start, m = subject.n_visits, g = subject.group;
    auto accumulate = [&](const kr_pattern& pattern) {
      matrix<double> X_tilde = data.weights_sqrt.segment(start, m).matrix().asDiagonal() * data.x.block(start, 0, m, p);
      for (int h = 0; h < k; ++h) {
        P.block((g * k + h) * p, 0, p, p) +=
          X_tilde.transpose() * pattern.inverse_d1.block(h * m, 0, m, m) * X_tilde;
      }
      for (int ell = 0; ell < k; ++ell) {
        matrix<double> Z = pattern.inverse_directions.block(ell * m, 0, m, m) * X_tilde;
        Q_sum += directions[g].signs(ell) * Z.transpose() * pattern.sigma * Z;
      }
    };
    if (data.spatial) {
      // Spatial distances vary by subject; do not key them by visit indices.
      accumulate(kr_pattern(derivatives.cache[g].get(), subject.visits, subject.dist, directions[g].B));
    } else {
      auto found = patterns[g].find(subject.visits);
      if (found == patterns[g].end()) {
        found = patterns[g].emplace(subject.visits,
          kr_pattern(derivatives.cache[g].get(), subject.visits, subject.dist, directions[g].B)).first;
      }
      accumulate(found->second);
    }
  }
  if (!finite_w) {
    Q_sum.setConstant(R_NaN);
  }
  return List::create(Named("P") = as_num_matrix_rcpp(P),
                      Named("Q") = R_NilValue, Named("R") = R_NilValue,
                      Named("S_Q") = as_num_matrix_rcpp(Q_sum));
}

// Obtain P,Q,R elements, or contracted linear components when W is supplied.
// mmrm() always supplies W for linear KR; the pairwise linear blocks are kept
// as an independent reference for tests.
List get_pqr(List mmrm_fit, NumericVector theta, bool linear, Nullable<NumericMatrix> w) {
  if (w.isNotNull()) {
    return get_pqr_contracted(mmrm_fit, theta, NumericMatrix(w));
  }
  kr_data data(mmrm_fit, theta);
  int n_theta = data.n_theta;
  int theta_size_per_group = data.theta_per_group;
  int p = data.p;
  matrix<double> P = matrix<double>::Zero(p * n_theta, p);
  matrix<double> Q = matrix<double>::Zero(p * theta_size_per_group * n_theta, p);
  matrix<double> R;
  if (!linear) {
    R = matrix<double>::Zero(p * theta_size_per_group * n_theta, p);
  }
  // Use map to hold these base class pointers (can also work for child class objects).
  auto derivatives_by_group = data.derivatives(!linear);
  for (int i = 0; i < data.n_subjects; i++) {
    kr_subject subject = data.subject(i);
    int start_i = subject.start;
    int n_visits_i = subject.n_visits;
    int subject_group_i = subject.group;
    matrix<double> sigma_inv, sigma_d2, sigma, sigma_inv_d1;

    sigma_inv = derivatives_by_group.cache[subject_group_i]->get_sigma_inverse(subject.visits, subject.dist);
    if (!linear) {
      sigma_d2 = derivatives_by_group.cache[subject_group_i]->get_sigma_derivative2(subject.visits, subject.dist);
    }
    sigma = derivatives_by_group.cache[subject_group_i]->get_sigma(subject.visits, subject.dist);
    sigma_inv_d1 = derivatives_by_group.cache[subject_group_i]->get_inverse_derivative(subject.visits, subject.dist);

    matrix<double> Xi = data.x.block(start_i, 0, n_visits_i, p);
    auto gi_sqrt_root = data.weights_sqrt.segment(start_i, n_visits_i).matrix().asDiagonal();
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
