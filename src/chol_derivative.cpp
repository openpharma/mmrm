#include <Rcpp.h>
#define INCLUDE_RCPP
#include "utils.h"
#include "covariance.h"

using namespace Rcpp;
using std::string;

struct chol {
  int dim_cov_mat;
  int n_groups;
  string cov_type;
  chol(int dim, string cov, int n_groups): dim_cov_mat(dim), cov_type(cov), n_groups(n_groups) {};
  template <class T>
    vector<T> operator() (vector<T> &theta) {
      return get_cov_lower_chol_grouped(theta, this->dim_cov_mat, this->cov_type, this->n_groups, false).vec();
    }
};

struct chol_jacobian {
  int dim_cov_mat;
  string cov_type;
  chol mychol;
  chol_jacobian(int dim, string cov, int n_groups): dim_cov_mat(dim), cov_type(cov), mychol(dim, cov, n_groups) {};
  template<class T>
    vector<T> operator() (vector<T> &theta) {
      return autodiff::jacobian(this->mychol, theta).vec();
    }
};

vector<double> as_vector(NumericVector input) {
  vector<double> ret(as<std::vector<double>>(input));
  return ret;
}
vector<int> as_vector(IntegerVector input) {
  vector<int> ret(as<std::vector<int>>(input));
  return ret;
}
NumericVector as_nv(vector<double> input) {
  NumericVector ret(input.size());
  for (int i = 0; i < input.size(); i++) {
    ret[i] = input(i);
  }
  return ret;
}

NumericMatrix as_mv(matrix<double> input) {
  vector<double> input_v = input.vec();
  NumericVector input_nv = as_nv(input_v);
  NumericMatrix ret(int(input.rows()), int(input.cols()), input_nv.begin());
  return ret;
}
matrix<double> as_matrix(NumericMatrix input) {
  matrix<double> ret(input.rows(), input.cols());
  for (int i = 0; i < input.rows(); i++) {
    for (int j = 0; j < input.cols(); j++) {
      ret(i,j) = input(i,j);
    }
  }
  return ret;
}

template <class Type>
std::map<std::string, matrix<Type>> derivatives(int n_visits, std::string cov_type, vector<Type> theta, int n_groups) {
  std::map<std::string, matrix<Type>> ret;
  chol chol_obj(n_visits, cov_type, n_groups);
  chol_jacobian chol_jac_obj(n_visits, cov_type, n_groups);
  matrix<Type> l = chol_obj(theta).matrix();
  l.resize(n_visits, n_visits);
  ret["chol"] = l;
  vector<Type> chol_d1_vec = autodiff::jacobian(chol_obj, theta).vec(); // chol_d1_vec is (dim * dim * l_theta)
  vector<Type> chol_d2_vec = autodiff::jacobian(chol_jac_obj, theta).vec(); // chol_d2_vec is (dim * dim * l_theta * l_theta)
  matrix<Type> ret_d1 = matrix<Type>(n_visits * theta.size(), n_visits);
  matrix<Type> ret_d2 = matrix<Type>(n_visits * theta.size() * theta.size(), n_visits);
  int n_visits_sq = n_visits * n_visits;
  for (int i = 0; i < theta.size(); i++) {
    matrix<Type> ld1 = chol_d1_vec.segment(i * n_visits_sq, n_visits_sq).matrix();
    ld1.resize(n_visits, n_visits);
    matrix<Type> ld1_lt = ld1 * l.transpose();
    auto sigma_d1_i = ld1_lt + ld1_lt.transpose();
    ret_d1.block(i * n_visits, 0, n_visits, n_visits) = sigma_d1_i;
    for (int j = 0; j < theta.size(); j++) {
      matrix<Type> ld2 = chol_d2_vec.segment( (j * theta.size() + i) * n_visits_sq, n_visits_sq).matrix();
      matrix<Type> ld1_j = chol_d1_vec.segment(j * n_visits_sq, n_visits_sq).matrix();
      ld2.resize(n_visits, n_visits);
      ld1_j.resize(n_visits, n_visits);
      auto ld2_lt = ld2 * l.transpose();
      auto ld1_ld1j = ld1 * ld1_j.transpose();
      auto sigma_d2_ij = ld2_lt + ld2_lt.transpose() + ld1_ld1j + ld1_ld1j.transpose();
      ret_d2.block((i * theta.size() + j) * n_visits, 0, n_visits, n_visits) = sigma_d2_ij;
    }
  }
  ret["derivative1"] = ret_d1;
  ret["derivative2"] = ret_d2;
  return ret;
}

template <class Type>
struct chols {
  std::map<std::vector<int>, matrix<Type>> inverse_cache;
  std::map<std::vector<int>, matrix<Type>> sigmad1_cache;
  std::map<std::vector<int>, matrix<Type>> sigmad2_cache;
  std::map<std::vector<int>, matrix<Type>> sigma_cache;
  std::map<std::vector<int>, matrix<Type>> sigma_inverse_d1_cache;
  std::map<std::vector<int>, Eigen::SparseMatrix<Type>> sel_mat_cache;
  std::string cov_type;
  std::vector<int> full_visit;
  int n_visits;
  int n_theta; // theta.size()
  int n_groups;
  vector<Type> theta;
  matrix<Type> chol_full;
  chols(bool spatial, vector<Type> theta, int n_visits, std::string cov_type, int n_groups): n_visits(n_visits), n_groups(n_groups), cov_type(cov_type), full_visit(std::vector<int>(n_visits)) {
    this->theta = theta;
    //this->full_visit = vector<int>(n_visits);
    for (int i = 0; i < n_visits; i++) {
      this->full_visit[i] = i;
    }
    this->n_theta = theta.size();
    std::map<std::string, tmbutils::matrix<Type>> allret = derivatives<Type>(this->n_visits, this->cov_type, this->theta, this->n_groups);
    matrix<Type> sigma_d1 = allret["derivative1"];
    matrix<Type> sigma_d2 = allret["derivative2"];
    matrix<Type> l = allret["chol"];
    this->chol_full = l;
    this->sigmad1_cache[this->full_visit] = sigma_d1;
    this->sigmad2_cache[this->full_visit] = sigma_d2;
    this->sigma_cache[this->full_visit] = allret["chol"] * allret["chol"].transpose();
    this->inverse_cache[this->full_visit] = (this->sigma_cache[this->full_visit]).inverse();
    this->sel_mat_cache[this->full_visit] = get_select_matrix<Type>(full_visit, this->n_visits);
  }
  Eigen::SparseMatrix<Type> get_sel_mat(std::vector<int> visits) {
    if (this->sel_mat_cache.count(visits) > 0) {
      return this->sel_mat_cache[visits];
    } else {
      this->sel_mat_cache[visits] = get_select_matrix<Type>(visits, this->n_visits);
      return this->sel_mat_cache[visits];
    }
  }
  matrix<Type> get_sigma_derivative1(std::vector<int> visits) {
     if (this->sigmad1_cache.count(visits) > 0) {
      return this->sigmad1_cache[visits];
    } else {
      Eigen::SparseMatrix<Type> sel_mat = this->get_sel_mat(visits);
      int n_visists_i = visits.size();
      matrix<Type> ret = matrix<Type>(this->n_theta * n_visists_i, n_visists_i);
      for (int i = 0; i < this->n_theta; i++) {
        ret.block(i  * n_visists_i, 0, n_visists_i, n_visists_i) = sel_mat * this->sigmad1_cache[this->full_visit].block(i  * this->n_visits, 0, this->n_visits, this->n_visits) * sel_mat.transpose();
      }
      this->sigmad1_cache[visits] = ret;
      return ret;
    }
  }
  matrix<Type> get_sigma_derivative2(std::vector<int> visits) {
     if (this->sigmad2_cache.count(visits) > 0) {
      return this->sigmad2_cache[visits];
    } else {
      Eigen::SparseMatrix<Type> sel_mat = this->get_sel_mat(visits);
      int n_visists_i = visits.size();
      int theta_sq = this->n_theta * this->n_theta;
      matrix<Type> ret = matrix<Type>(theta_sq * n_visists_i, n_visists_i);
      for (int i = 0; i < theta_sq; i++) {
        ret.block(i  * n_visists_i, 0, n_visists_i, n_visists_i) = sel_mat * this->sigmad2_cache[this->full_visit].block(i  * this->n_visits, 0, this->n_visits, this->n_visits) * sel_mat.transpose();
      }
      this->sigmad2_cache[visits] = ret;
      return ret;
    }
  }
  matrix<Type> get_sigma(std::vector<int> visits) {
     if (this->sigma_cache.count(visits) > 0) {
      return this->sigma_cache[visits];
    } else {
      Eigen::SparseMatrix<Type> sel_mat = this->get_sel_mat(visits);
      int n_visists_i = visits.size();
      matrix<Type> ret = sel_mat * this->sigma_cache[this->full_visit] * sel_mat.transpose();
      this->sigma_cache[visits] = ret;
      return ret;
    }
  }
  matrix<Type> get_inverse(std::vector<int> visits) {
    if (this->inverse_cache.count(visits) > 0) {
      return this->inverse_cache[visits];
    } else {
      Eigen::SparseMatrix<Type> sel_mat = this->get_sel_mat(visits);
      matrix<Type> Ltildei = sel_mat * this->chol_full;
      matrix<Type> cov_i = tcrossprod(Ltildei, true);
      auto sigmainv = cov_i.inverse();
      this->inverse_cache[visits] = sigmainv;
      return sigmainv;
    }
  }
  matrix<Type> get_inverse_derivative(std::vector<int> visits) {
    if (this->sigma_inverse_d1_cache.count(visits) > 0) {
      return this->sigma_inverse_d1_cache[visits];
    } else {
      Eigen::SparseMatrix<Type> sel_mat = this->get_sel_mat(visits);
      auto sigma_d1 = this->get_sigma_derivative1(visits);
      matrix<Type> sigma_inv_d1(sigma_d1.rows(), sigma_d1.cols());
      int n_visits_i = visits.size();
      auto sigma_inv = this->get_inverse(visits);
      for (int r = 0; r < this->n_theta; r++) {
        sigma_inv_d1.block(r * n_visits_i, 0, n_visits_i, n_visits_i) = - sigma_inv * sigma_d1.block(r * n_visits_i, 0, n_visits_i, n_visits_i) *sigma_inv;
      }
      this->sigma_inverse_d1_cache[visits] = sigma_inv_d1;
      return sigma_inv_d1;
    }
  }
};

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
  NumericVector weights_vector = mmrm_fit["weights_vector"];
  if (is_spatial) {
    Rf_error("Spatial Kenward-Roger not implemented yet!");
  }
  auto theta_v = as_vector(theta);
  std::string covtype_str = string(cov_type);
  auto G_sqrt = as_vector(weights_vector).sqrt();
  auto mychol = chols<double>(is_spatial, theta_v, n_visits, cov_type, n_groups);
  int p = x.cols();
  int n_theta = theta.size();
  matrix<double> P = matrix<double>::Zero(p * n_theta, p);
  matrix<double> Q = matrix<double>::Zero(p * n_theta * n_theta, p);
  matrix<double> R = matrix<double>::Zero(p * n_theta * n_theta, p);

  for (int i = 0; i < n_subjects; i++) {
    int start_i = subject_zero_inds[i];
    int n_visits_i = subject_n_visits[i];
    std::vector<int> visit_i(n_visits_i);
    for (int i = 0; i < n_visits_i; i++) {
      visit_i[i] = visits_zero_inds[i + start_i];
    }
    auto x_matrix = as_matrix(x);
    matrix<double> Xi = x_matrix.block(start_i, 0, n_visits_i, x_matrix.cols());
    auto sigma_inv = mychol.get_inverse(visit_i);
    auto sigma_d1 = mychol.get_sigma_derivative1(visit_i);
    auto sigma_d2 = mychol.get_sigma_derivative2(visit_i);
    auto sigma = mychol.get_sigma(visit_i);
    auto sigma_inv_d1 = mychol.get_inverse_derivative(visit_i);
    auto gi_sqrt_root = G_sqrt.segment(start_i, n_visits_i).matrix().asDiagonal();
    for (int r = 0; r < n_theta; r ++) {
      auto Pi = Xi.transpose() * gi_sqrt_root * sigma_inv_d1.block(r * n_visits_i, 0, n_visits_i, n_visits_i) * gi_sqrt_root * Xi;
      P.block(r * p, 0, p, p) += Pi;
      for (int j = 0; j < n_theta; j++) {
        auto Qij = Xi.transpose() * gi_sqrt_root * sigma_inv_d1.block(r * n_visits_i, 0, n_visits_i, n_visits_i) * sigma * sigma_inv_d1.block(j * n_visits_i, 0, n_visits_i, n_visits_i) * gi_sqrt_root * Xi;
        // switch the order so that in the matrix partial(i) and partial(j) increase j first
        Q.block((r * n_theta + j) * p, 0, p, p) += Qij;
        auto Rij = Xi.transpose() * gi_sqrt_root * sigma_inv * sigma_d2.block((j * n_theta + r) * n_visits_i, 0, n_visits_i, n_visits_i) * sigma_inv * gi_sqrt_root * Xi;
        R.block((r * n_theta + j) * p, 0, p, p) += Rij;
      }
    }
  }
  return List::create(
    Named("P") = as_mv(P),
    Named("Q") = as_mv(Q),
    Named("R") = as_mv(R)
  );
}
