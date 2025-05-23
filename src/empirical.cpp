#include "derivatives.h"

using namespace Rcpp;
using std::string;
// Obtain the empirical given beta, beta_vcov, theta.
//
// Note: This function previously (version < 0.3.15) returned `df_mat` which was the crossproduct of the `g` matrix.
// This was removed because this matrix can be very large if the number of subjects is large and/or the number of
// coefficients is large. Instead, now the `g_mat` element includes the `g` matrix.
List get_empirical(List mmrm_data, NumericVector theta, NumericVector beta, NumericMatrix beta_vcov, string type) {
  NumericMatrix x = mmrm_data["x_matrix"];
  matrix<double> x_matrix = as_num_matrix_tmb(x);
  NumericVector y = mmrm_data["y_vector"];
  matrix<double> beta_vcov_matrix = as_num_matrix_tmb(beta_vcov);
  IntegerVector subject_zero_inds = mmrm_data["subject_zero_inds"];
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
  matrix<double> coords = as_num_matrix_tmb(coordinates);
  matrix<double> beta_m = as_num_vector_tmb(beta).matrix();
  vector<double> theta_v = as_num_vector_tmb(theta);
  matrix<double> fitted = x_matrix * beta_m;
  matrix<double> y_matrix = as_num_vector_tmb(y).matrix();
  matrix<double> residual = y_matrix - fitted;
  vector<double> G_sqrt = as_num_vector_tmb(sqrt(weights_vector));
  int p = x.cols();

  matrix<double> score_per_subject = matrix<double>::Zero(n_subjects, p);

  // Use map to hold these base class pointers (can also work for child class objects).
  auto derivatives_by_group = cache_obj<double, derivatives_base<double>, derivatives_sp_exp<double>, derivatives_nonspatial<double>>(theta_v, n_groups, is_spatial, cov_type, n_visits);
  matrix<double> meat = matrix<double>::Zero(p, p);
  matrix<double> xt_g_simga_inv_chol = matrix<double>::Zero(p, n_observations);
  matrix<double> ax = matrix<double>::Zero(n_observations, p);
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
    matrix<double> sigma_inv_chol = derivatives_by_group.cache[subject_group_i]->get_inverse_chol(visit_i, dist_i);
    matrix<double> Xi = x_matrix.block(start_i, 0, n_visits_i, x_matrix.cols());
    matrix<double> residual_i = residual.block(start_i, 0, n_visits_i, 1);
    matrix<double> gi_sqrt_root = G_sqrt.segment(start_i, n_visits_i).matrix().asDiagonal();
    matrix<double> gi_simga_inv_chol = gi_sqrt_root * sigma_inv_chol;
    matrix<double> xt_gi_simga_inv_chol = Xi.transpose() * gi_simga_inv_chol;
    matrix<double> ai = matrix<double>::Identity(n_visits_i, n_visits_i);
    if (type != "Empirical") {
      ai = ai - xt_gi_simga_inv_chol.transpose() * beta_vcov_matrix * xt_gi_simga_inv_chol;
    }
    if (type == "Empirical-Jackknife") {
      ai = ai.inverse();
    } else if(type == "Empirical-Bias-Reduced") {
      ai = pseudoInverseSqrt(ai);
    }
    matrix<double> xta = xt_gi_simga_inv_chol * ai;
    matrix<double> z = xta * gi_simga_inv_chol.transpose() * residual_i;
    meat = meat + z * z.transpose();
    xt_g_simga_inv_chol.block(0, start_i, p, n_visits_i) = xt_gi_simga_inv_chol;
    ax.block(start_i, 0, n_visits_i, p) = xta.transpose();

    score_per_subject.row(i) = z.transpose();
  }
  matrix<double> h = xt_g_simga_inv_chol.transpose() * beta_vcov_matrix * xt_g_simga_inv_chol;
  matrix<double> imh = matrix<double>::Identity(n_observations, n_observations) - h;
  matrix<double> ax_xtx =  ax * beta_vcov_matrix;
  matrix<double> g = matrix<double>::Zero(n_observations, p * n_subjects);
  for (int i = 0; i < n_subjects; i++) {
    int start_i = subject_zero_inds[i];
    int n_visits_i = subject_n_visits[i];
    g.block(0, i * p, n_observations, p) = imh.block(0, start_i, n_observations, n_visits_i) * ax_xtx.block(start_i, 0, n_visits_i, p);
  }

  // beta_vcov already take gi into consideration;
  matrix<double> ret = beta_vcov_matrix * meat * beta_vcov_matrix;

  return List::create(
    Named("score_per_subject") = as_num_matrix_rcpp(score_per_subject),
    Named("cov") = as_num_matrix_rcpp(ret),
    Named("g_mat") = as_num_matrix_rcpp(g)
  );
}
