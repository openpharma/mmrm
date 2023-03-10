#include "covariance.h"

using namespace Rcpp;
using std::string;
// Struct chol to obtain the cholesky factor given theta.
// The reason to have it is that we need a functor that need only theta to
// obtain the derivatives from autodiff.
struct chol {
  int dim_cov_mat;
  string cov_type;
  chol(int dim, string cov): dim_cov_mat(dim), cov_type(cov) {};
  template <class T>
    vector<T> operator() (vector<T> &theta) {
      return get_cov_lower_chol_grouped(theta, this->dim_cov_mat, this->cov_type, 1, false).vec();
    }
};
// Struct chol_jacobian that has jacobian of the cholesky factor given theta.
// The reason to have it is that we need hessian so we use jacobian twice.
struct chol_jacobian {
  int dim_cov_mat;
  string cov_type;
  chol mychol;
  chol_jacobian(int dim, string cov): dim_cov_mat(dim), cov_type(cov), mychol(dim, cov) {};
  template<class T>
    vector<T> operator() (vector<T> &theta) {
      return autodiff::jacobian(this->mychol, theta).vec();
    }
};

// Template function to obtain derivatives from visits, cov_type and theta.
// Basically this is calculating the derivatives for the sigma
// from the derivatives for the cholesky factor.
template <class Type>
std::map<std::string, matrix<Type>> derivatives(int n_visits, std::string cov_type, vector<Type> theta) {
  std::map<std::string, matrix<Type>> ret;
  chol chol_obj(n_visits, cov_type);
  chol_jacobian chol_jac_obj(n_visits, cov_type);
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
// Base class of spatial and non-spatial derivatives.
template <class Type>
struct derivatives_base {
  virtual matrix<Type> get_inverse(std::vector<int> visits, matrix<Type> dist) = 0;
  virtual matrix<Type> get_inverse_chol(std::vector<int> visits, matrix<Type> dist) = 0;
  virtual matrix<Type> get_chol(std::vector<int> visits, matrix<Type> dist) = 0;
  virtual matrix<Type> get_sigma_derivative1(std::vector<int> visits, matrix<Type> dist) = 0;
  virtual matrix<Type> get_sigma_derivative2(std::vector<int> visits, matrix<Type> dist) = 0;
  virtual matrix<Type> get_sigma(std::vector<int> visits, matrix<Type> dist) = 0;
  virtual matrix<Type> get_inverse_derivative(std::vector<int> visits, matrix<Type> dist) = 0;
};

// Struct derivatives_nonspatial is created to get the derivatives with cache.
// The main reason to have it is that we nearly always have duplicated visits
// and the inverse of a matrix is calculation expensive. In addition, we can save
// the resource needed for select matrix calculations.
template <class Type>
struct derivatives_nonspatial: public derivatives_base<Type> {
  std::map<std::vector<int>, matrix<Type>> inverse_cache;
  std::map<std::vector<int>, matrix<Type>> inverse_chol_cache;
  std::map<std::vector<int>, matrix<Type>> chol_cache;
  std::map<std::vector<int>, matrix<Type>> sigmad1_cache;
  std::map<std::vector<int>, matrix<Type>> sigmad2_cache;
  std::map<std::vector<int>, matrix<Type>> sigma_cache;
  std::map<std::vector<int>, matrix<Type>> sigma_inverse_d1_cache;
  std::map<std::vector<int>, Eigen::SparseMatrix<Type>> sel_mat_cache;
  std::string cov_type;
  int n_visits;
  std::vector<int> full_visit;
  int n_theta; // theta.size()
  vector<Type> theta;
  matrix<Type> chol_full;
  derivatives_nonspatial() {
    // This default constructor is needed because the use of `[]` in map.
  }
  // Constructor from theta, n_visits and cov_type, and cache full_visits values.
  derivatives_nonspatial(vector<Type> theta, int n_visits, std::string cov_type): cov_type(cov_type), n_visits(n_visits), full_visit(std::vector<int>(n_visits)) {
    this->theta = theta;
    for (int i = 0; i < n_visits; i++) {
      this->full_visit[i] = i;
    }
    this->n_theta = theta.size();
    std::map<std::string, tmbutils::matrix<Type>> allret = derivatives<Type>(this->n_visits, this->cov_type, this->theta);
    matrix<Type> sigma_d1 = allret["derivative1"];
    matrix<Type> sigma_d2 = allret["derivative2"];
    matrix<Type> l = allret["chol"];
    this->chol_full = l;
    this->sigmad1_cache[this->full_visit] = sigma_d1;
    this->sigmad2_cache[this->full_visit] = sigma_d2;
    this->sigma_cache[this->full_visit] = allret["chol"] * allret["chol"].transpose();
    this->inverse_cache[this->full_visit] = (this->sigma_cache[this->full_visit]).inverse();
    this->sel_mat_cache[this->full_visit] = get_select_matrix<Type>(this->full_visit, this->n_visits);
  }
  // Cache and return the select matrix.
  Eigen::SparseMatrix<Type> get_sel_mat(std::vector<int> visits) {
    if (this->sel_mat_cache.count(visits) > 0) {
      return this->sel_mat_cache[visits];
    } else {
      this->sel_mat_cache[visits] = get_select_matrix<Type>(visits, this->n_visits);
      return this->sel_mat_cache[visits];
    }
  }
  // Cache and return the first order derivatives using select matrix.
  matrix<Type> get_sigma_derivative1(std::vector<int> visits, matrix<Type> dist) override {
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
  // Cache and return the second order derivatives using select matrix.
  matrix<Type> get_sigma_derivative2(std::vector<int> visits, matrix<Type> dist) override {
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
  // Cache and return the sigma using select matrix.
  matrix<Type> get_sigma(std::vector<int> visits, matrix<Type> dist) override {
     if (this->sigma_cache.count(visits) > 0) {
      return this->sigma_cache[visits];
    } else {
      Eigen::SparseMatrix<Type> sel_mat = this->get_sel_mat(visits);
      matrix<Type> ret = sel_mat * this->sigma_cache[this->full_visit] * sel_mat.transpose();
      this->sigma_cache[visits] = ret;
      return ret;
    }
  }
  // Cache and return the lower cholesky factor of inverse of sigma using select matrix.
  matrix<Type> get_inverse_chol(std::vector<int> visits, matrix<Type> dist) override {
    if (this->inverse_chol_cache.count(visits) > 0) {
      return this->inverse_chol_cache[visits];
    } else {
      auto sigmainv = this->get_inverse(visits, dist);
      Eigen::LLT<Eigen::Matrix<Type,Eigen::Dynamic,Eigen::Dynamic> > sigma_inv_chol(sigmainv);
      matrix<Type> Li = sigma_inv_chol.matrixL();
      this->inverse_chol_cache[visits] = Li;
      return Li;
    }
  }
  // Cache and return the lower cholesky factor of sigma using the select matrix.
  matrix<Type> get_chol(std::vector<int> visits, matrix<Type> dist) override {
    if (this->chol_cache.count(visits) > 0) {
      return this->chol_cache[visits];
    } else {
      Eigen::SparseMatrix<Type> sel_mat = this->get_sel_mat(visits);
      matrix<Type> Ltildei = sel_mat * this->chol_full;
      this->chol_cache[visits] = Ltildei;
      return Ltildei;
    }
  }
  // Cache and return the inverse of sigma using select matrix.
  matrix<Type> get_inverse(std::vector<int> visits, matrix<Type> dist) override {
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
  // Cache and return the first order derivatives of inverse of sigma using select matrix.
  matrix<Type> get_inverse_derivative(std::vector<int> visits, matrix<Type> dist) override {
    if (this->sigma_inverse_d1_cache.count(visits) > 0) {
      return this->sigma_inverse_d1_cache[visits];
    } else {
      Eigen::SparseMatrix<Type> sel_mat = this->get_sel_mat(visits);
      auto sigma_d1 = this->get_sigma_derivative1(visits, dist);
      matrix<Type> sigma_inv_d1(sigma_d1.rows(), sigma_d1.cols());
      int n_visits_i = visits.size();
      auto sigma_inv = this->get_inverse(visits, dist);
      for (int r = 0; r < this->n_theta; r++) {
        sigma_inv_d1.block(r * n_visits_i, 0, n_visits_i, n_visits_i) = - sigma_inv * sigma_d1.block(r * n_visits_i, 0, n_visits_i, n_visits_i) *sigma_inv;
      }
      this->sigma_inverse_d1_cache[visits] = sigma_inv_d1;
      return sigma_inv_d1;
    }
  }
};

// derivatives_sp_exp struct is created to obtain the exact derivatives of spatial exponential
// covariance structure, and its inverse.
// No caching is used because the distance can be hardly the same for spatial covariance
// structures.
template <class Type>
struct derivatives_sp_exp: public derivatives_base<Type> {
  Type const_sd;
  Type rho;
  Type logrho;
  derivatives_sp_exp() {
    // This default constructor is needed because the use of `[]` in maps.
  }
  // Initialize the theta values; the reason to have theta is that for a fit, the theta
  // is the same for all subjects, while the distance between each visits for each subject
  // can be different.
  derivatives_sp_exp(vector<Type> theta): const_sd(exp(theta(0))), rho(invlogit(theta(1))) {
    this->logrho = log(this->rho);
  }
  // Obtain first order derivatives
  matrix<Type> get_sigma_derivative1(std::vector<int> visits, matrix<Type> dist) override {
    matrix<Type> ret(2 * dist.rows(), dist.cols());
    // partial sigma / partial theta_1 = sigma.
    auto sigma = this->get_sigma(visits, dist);
    ret.block(0, 0, dist.rows(), dist.cols()) = sigma;
    ret.block(dist.rows(), 0, dist.rows(), dist.cols()) = sigma.array() * dist.array() * (1 - this->rho);
    return ret;
  }
  // Obtain second order derivatives.
  matrix<Type> get_sigma_derivative2(std::vector<int> visits, matrix<Type> dist) override {
    matrix<Type> ret(4 * dist.rows(), dist.cols());
    auto sigma = this->get_sigma(visits, dist);
    ret.block(0, 0, dist.rows(), dist.cols()) = sigma;
    Type rho_r = 1 - this->rho;
    auto dtheta1dtheta2 = sigma.array() * dist.array() * rho_r;
    ret.block(dist.rows(), 0, dist.rows(), dist.cols()) =  dtheta1dtheta2;
    ret.block(dist.rows() * 2, 0, dist.rows(), dist.cols()) = dtheta1dtheta2;
    matrix<Type> dtheta2s = dtheta1dtheta2 * (dist.array() * rho_r - this->rho);
    ret.block(dist.rows() * 3, 0, dist.rows(), dist.cols()) = dtheta2s;
    return ret;
  }
  // Obtain sigma.
  matrix<Type> get_sigma(std::vector<int> visits, matrix<Type> dist) override {
    matrix<Type> result = exp(dist.array() * this->logrho) * this->const_sd;
    return result;
  }
  // Obtain inverse of sigma.
  matrix<Type> get_inverse(std::vector<int> visits, matrix<Type> dist) override {
    return this->get_sigma(visits, dist).inverse();
  }
  // Obtain the lower cholesky factor of inverse of sigma using select matrix.
  matrix<Type> get_inverse_chol(std::vector<int> visits, matrix<Type> dist) override {
    auto sigmainv = this->get_inverse(visits, dist);
    Eigen::LLT<Eigen::Matrix<Type,Eigen::Dynamic,Eigen::Dynamic> > sigma_inv_chol(sigmainv);
    matrix<Type> Li = sigma_inv_chol.matrixL();
    return Li;
  }
  // Obtain the lower cholesky factor of sigma using select matrix.
  matrix<Type> get_chol(std::vector<int> visits, matrix<Type> dist) override {
    auto sigma = this->get_sigma(visits, dist);
    Eigen::LLT<Eigen::Matrix<Type,Eigen::Dynamic,Eigen::Dynamic> > sigma_chol(sigma);
    matrix<Type> Li = sigma_chol.matrixL();
    return Li;
  }
  // Obtain first order derivatives for inverse of sigma.
  matrix<Type> get_inverse_derivative(std::vector<int> visits, matrix<Type> dist) override {
    matrix<Type> sigma_inv_d1 = matrix<Type>::Zero(2 * dist.rows(), dist.cols());
    auto sigma_inv = this->get_inverse(visits, dist);
    auto sigma_d1 = this->get_sigma_derivative1(visits, dist);
    for (int r = 0; r < 2; r++) {
      sigma_inv_d1.block(r * dist.rows(), 0, dist.rows(), dist.cols()) = - sigma_inv * sigma_d1.block(r * dist.rows(), 0, dist.rows(), dist.cols()) *sigma_inv;
    }
    return sigma_inv_d1;
  }
};
