#ifndef KR_COMP_INCLUDED_
#define KR_COMP_INCLUDED_

#include "derivatives.h"

// Observed visits of one subject, see kr_data::subject().
struct kr_subject {
  int start, n_visits, group;
  std::vector<int> visits;
  matrix<double> dist;
};

// Fitted model inputs shared by the pairwise and contracted backends.
struct kr_data {
  matrix<double> x, coords;
  vector<double> weights_sqrt, theta;
  IntegerVector starts, sizes, groups;
  int n_subjects, n_groups, n_visits, n_theta, theta_per_group, p;
  bool spatial;
  std::string cov_type;
  kr_data(List mmrm_fit, NumericVector theta_r) {
    x = as_num_matrix_tmb(as<NumericMatrix>(mmrm_fit["x_matrix"]));
    coords = as_num_matrix_tmb(as<NumericMatrix>(mmrm_fit["coordinates"]));
    weights_sqrt = as_num_vector_tmb(sqrt(as<NumericVector>(mmrm_fit["weights_vector"])));
    theta = as_num_vector_tmb(theta_r);
    starts = mmrm_fit["subject_zero_inds"];
    sizes = mmrm_fit["subject_n_visits"];
    groups = mmrm_fit["subject_groups"];
    n_subjects = mmrm_fit["n_subjects"];
    n_groups = mmrm_fit["n_groups"];
    n_visits = mmrm_fit["n_visits"];
    n_theta = theta_r.size();
    theta_per_group = n_theta / n_groups;
    p = x.cols();
    spatial = as<int>(mmrm_fit["is_spatial_int"]) == 1;
    cov_type = as<std::string>(mmrm_fit["cov_type"]);
  }
  derivatives_cache<double> derivatives(bool second_order) const {
    return derivatives_cache<double>(theta, n_groups, spatial, cov_type, n_visits, second_order);
  }
  // Zero-based group with visit indices (non-spatial) or distances (spatial).
  kr_subject subject(int i) const {
    kr_subject s{starts[i], sizes[i], groups[i] - 1, std::vector<int>(sizes[i]), matrix<double>(0, 0)};
    if (spatial) {
      s.dist = euclidean(matrix<double>(coords.block(s.start, 0, s.n_visits, coords.cols())));
    } else {
      for (int j = 0; j < s.n_visits; ++j) s.visits[j] = int(coords(s.start + j, 0));
    }
    return s;
  }
};

// W = B diag(signs) B'. Keep every direction, including negative eigenvalues
// when W is indefinite: clipping them would change the supplied contraction.
// W is an inverse Hessian, symmetric up to rounding, so use its symmetric part.
struct kr_directions {
  matrix<double> B;
  vector<double> signs;
  explicit kr_directions(const matrix<double>& w_r) {
    matrix<double> w = (w_r + w_r.transpose()) / 2;
    int k = w.rows();
    signs = vector<double>::Ones(k);
    Eigen::LLT<Eigen::MatrixXd> llt(w);
    if (llt.info() == Eigen::Success) {
      B = llt.matrixL();
    } else {
      Eigen::SelfAdjointEigenSolver<Eigen::MatrixXd> eig(w);
      if (eig.info() != Eigen::Success) {
        Rcpp::stop("Could not factor covariance parameter covariance matrix.");
      }
      B = eig.eigenvectors();
      for (int ell = 0; ell < k; ++ell) {
        double value = eig.eigenvalues()(ell);
        signs(ell) = value < 0 ? -1 : 1;
        B.col(ell) *= std::sqrt(std::abs(value));
      }
    }
  }
};

// Pattern-level products, reused across subjects in the same nonspatial group.
// Block ell of inverse_directions is sum_h B(h, ell) * d(Sigma^-1)/d(theta_h).
struct kr_pattern {
  matrix<double> sigma, inverse_d1, inverse_directions;
  kr_pattern(derivatives_base<double>* cache, const std::vector<int>& visits,
             const matrix<double>& dist, const matrix<double>& B) {
    sigma = cache->get_sigma(visits, dist);
    inverse_d1 = cache->get_inverse_derivative(visits, dist);
    int m = sigma.rows();
    int k = B.rows();
    inverse_directions = matrix<double>::Zero(k * m, m);
    for (int ell = 0; ell < k; ++ell) {
      for (int h = 0; h < k; ++h) {
        inverse_directions.block(ell * m, 0, m, m) +=
          B(h, ell) * inverse_d1.block(h * m, 0, m, m);
      }
    }
  }
};

#endif
