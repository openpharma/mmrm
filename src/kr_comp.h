#ifndef KR_COMP_INCLUDED_
#define KR_COMP_INCLUDED_

#include "derivatives.h"

// W = B diag(signs) B'. Keep every direction, including negative eigenvalues
// when W is indefinite: clipping them would change the supplied contraction.
struct kr_directions {
  matrix<double> B;
  vector<double> signs;
  explicit kr_directions(const matrix<double>& w) {
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
      for (int h = 0; h < k; ++h) {
        double value = eig.eigenvalues()(h);
        signs(h) = value < 0 ? -1 : 1;
        B.col(h) *= std::sqrt(std::abs(value));
      }
    }
  }
};

// Pattern-level products, reused across subjects in the same nonspatial group.
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
