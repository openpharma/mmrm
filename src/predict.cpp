#include "covariance.h"
#include "chol_cache.h"

using namespace Rcpp;
using std::string;
// Obtain the empirical given beta, beta_vcov, theta.
NumericMatrix predict(List mmrm_data, NumericVector theta, NumericVector beta, NumericMatrix beta_vcov) {
  NumericMatrix x = mmrm_data["x_matrix"];
  NumericVector y = mmrm_data["y_vector"];
  LogicalVector y_na = is_na(y);
  LogicalVector y_vd = ! y_na;
  IntegerVector subject_zero_inds = mmrm_data["subject_zero_inds"];
  IntegerVector visits_zero_inds = mmrm_data["visits_zero_inds"];
  IntegerVector subject_n_visits = mmrm_data["subject_n_visits"];
  String cov_type = mmrm_data["cov_type"];
  IntegerVector subject_groups = mmrm_data["subject_groups"];
  NumericMatrix coordinates = mmrm_data["coordinates"];

  matrix<double> x_matrix = as_matrix<matrix<double>, NumericMatrix>(x);
  matrix<double> coordinates_m = as_matrix<matrix<double>, NumericMatrix>(coordinates);
  matrix<double> beta_vcov_matrix = as_matrix<matrix<double>, NumericMatrix>(beta_vcov);
  int n_subjects = mmrm_data["n_subjects"];
  int n_observations = x_matrix.rows();
  int n_visits = mmrm_data["n_visits"];
  int is_spatial_int = mmrm_data["is_spatial_int"];
  bool is_spatial = is_spatial_int == 1;
  int n_groups = mmrm_data["n_groups"];
  vector<double> beta_v = as_vector<vector<double>, NumericVector>(beta);
  vector<double> theta_v = as_vector<vector<double>, NumericVector>(theta);
  int n_theta = theta.size();
  int theta_one_group_size = n_theta / n_groups;
  int p = x.cols();
  // Use map to hold these base class pointers (can also work for child class objects).
  std::map<int, lower_chol_base<double>*> chols_by_group;
  for (int r = 0; r < n_groups; r++) {
    // in loops using new keyword is required so that the objects stays on the heap
    // otherwise this will be destroyed and you will get unexpected result
    if (is_spatial) {
      chols_by_group[r] = new lower_chol_spatial<double>(theta_v.segment(r * theta_one_group_size, theta_one_group_size), cov_type);
    } else {
      chols_by_group[r] = new lower_chol_nonspatial<double>(theta_v.segment(r * theta_one_group_size, theta_one_group_size), n_visits, cov_type);
    }
  }
  NumericVector y_pred = clone(y); // predict value of y; observed use the same value
  NumericVector var(y.size()); // variance of y with 0 as default;
  NumericVector conf_var(y.size()); // confidence interval variance
  // Go through all subjects and calculate quantities initialized above.
  for (int i = 0; i < n_subjects; i++) {
    // Start index and number of visits for this subject.
    int start_i = subject_zero_inds(i);
    int n_visits_i = subject_n_visits(i);
    NumericVector y_i = segment(y, start_i, n_visits_i);
    LogicalVector y_na_i = segment(y_na, start_i, n_visits_i);
    LogicalVector y_valid_i = segment(y_vd, start_i, n_visits_i);
    IntegerVector visit_i(n_visits_i);
    matrix<double> dist_i(n_visits_i, n_visits_i);
    IntegerVector index_zero_i = seq(0, n_visits_i - 1);
    if (!is_spatial) {
      visit_i = segment(visits_zero_inds, start_i, n_visits_i);
    } else {
      visit_i = seq(start_i, start_i + n_visits_i - 1);
      dist_i = euclidean(matrix<double>(coordinates_m.block(start_i, 0, n_visits_i, coordinates_m.cols())));
    }
    std::vector<int> visit_std = as<std::vector<int>>(visit_i);
    IntegerVector visit_na_vec = visit_i[y_na_i];
    IntegerVector visit_valid_vec = visit_i[y_valid_i];

    IntegerVector index_zero_i_na = index_zero_i[y_na_i];
    IntegerVector index_zero_i_valid = index_zero_i[y_valid_i];

    std::vector<int> visit_na = as<std::vector<int>>(visit_na_vec);
    std::vector<int> visit_non_na = as<std::vector<int>>(visit_valid_vec);
    matrix<double> Xi = x_matrix.block(start_i, 0, n_visits_i, x_matrix.cols());
    // subject_group starts with 1.
    int subject_group_i = subject_groups(i) - 1;
    matrix<double> sigma_full = chols_by_group[subject_group_i]->get_sigma(visit_std, dist_i);
    matrix<double> na_sel_matrix = get_select_matrix<double>(as<std::vector<int>>(index_zero_i_na), n_visits_i); // select matrix based on already subsetted visits
    matrix<double> valid_sel_matrix = get_select_matrix<double>(as<std::vector<int>>(index_zero_i_valid), n_visits_i);
    matrix<double> sigma_12 = na_sel_matrix * sigma_full * valid_sel_matrix.transpose();
    matrix<double> sigma_11 = chols_by_group[subject_group_i]->get_sigma(visit_na, dist_i);
    matrix<double> x_na = na_sel_matrix * Xi;
    matrix<double> x_valid = valid_sel_matrix * Xi;
    vector<double> y_valid = as_vector<vector<double>, NumericVector>(y_i[y_valid_i]);
    IntegerVector na_index = index_zero_i_na + start_i;
    vector<double> y_hat, var_conf, var_y_on_theta;
    if (visit_valid_vec.size() == 0) {
      // no observations with valid y
      y_hat = x_na * beta_v;
      var_conf = (x_na * beta_vcov_matrix * x_na.transpose()).diagonal();
      var_y_on_theta = var_conf + vector<double>(sigma_full.diagonal());
      
    } else if (visit_na_vec.size() > 0) {
      // there are observations with invalid y
      matrix<double> sigma_22_inv;
      if (is_spatial) {
        sigma_22_inv = (valid_sel_matrix * sigma_full * valid_sel_matrix.transpose()).inverse(); // no cache available
      } else {
        sigma_22_inv = chols_by_group[subject_group_i]->get_sigma_inverse(visit_non_na, dist_i); // have the inverse in cache
      }
      matrix<double> ss = sigma_12 * sigma_22_inv;
      matrix<double> zz = x_na - ss * x_valid;
      y_hat = zz * beta_v + ss * y_valid;
      var_conf = (zz * beta_vcov_matrix * zz.transpose()).diagonal();
      var_y_on_theta = var_conf + vector<double>((sigma_11 - ss * sigma_12.transpose()).diagonal());
    }
    y_pred[na_index] = as_vector<NumericVector, vector<double>>(y_hat);
    conf_var[na_index] = as_vector<NumericVector, vector<double>>(var_conf);
    var[na_index] = as_vector<NumericVector, vector<double>>(var_y_on_theta);
  }
  return cbind(y_pred, conf_var, var);
}
