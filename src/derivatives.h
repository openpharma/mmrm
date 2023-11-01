#ifndef DERIVATIVE_INCLUDED_
#define DERIVATIVE_INCLUDED_

#include "chol_cache.h"

using namespace Rcpp;
using std::string;
// Struct chol to obtain the cholesky factor given theta.
// The reason to have it is that we need a functor that need only theta to
// obtain the derivatives from autodiff.
// Only non-spatial covariance structure here.
struct chol {
  int dim_cov_mat;
  string cov_type;
  chol(int dim, string cov): dim_cov_mat(dim), cov_type(cov) {};
  template <class T>
    vector<T> operator() (vector<T> &theta) {
      return get_covariance_lower_chol(theta, this->dim_cov_mat, this->cov_type).vec();
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
struct derivatives_base: virtual lower_chol_base<Type> {
  virtual matrix<Type> get_inverse_chol(std::vector<int> visits, matrix<Type> dist) = 0;
  virtual matrix<Type> get_sigma_derivative1(std::vector<int> visits, matrix<Type> dist) = 0;
  virtual matrix<Type> get_sigma_derivative2(std::vector<int> visits, matrix<Type> dist) = 0;
  virtual matrix<Type> get_inverse_derivative(std::vector<int> visits, matrix<Type> dist) = 0;
  // Create virtual destructor to avoid the default desctructor being called.
  virtual ~derivatives_base() {};
};

// Struct derivatives_nonspatial is created to get the derivatives with cache.
// The main reason to have it is that we nearly always have duplicated visits
// and the inverse of a matrix is calculation expensive. In addition, we can save
// the resource needed for select matrix calculations.
template <class Type>
struct derivatives_nonspatial: public lower_chol_nonspatial<Type>, virtual derivatives_base<Type> {
  std::map<std::vector<int>, matrix<Type>> inverse_chol_cache;
  std::map<std::vector<int>, matrix<Type>> sigmad1_cache;
  std::map<std::vector<int>, matrix<Type>> sigmad2_cache;
  std::map<std::vector<int>, matrix<Type>> sigma_inverse_d1_cache;
  derivatives_nonspatial() {
    // This default constructor is needed because the use of `[]` in map.
  }
  // Constructor from theta, n_visits and cov_type, and cache full_visits values.
  derivatives_nonspatial(vector<Type> theta, int n_visits, std::string cov_type): lower_chol_nonspatial<Type>(theta, n_visits, cov_type) {
    std::map<std::string, tmbutils::matrix<Type>> allret = derivatives<Type>(this->n_visits, this->cov_type, this->theta);
    matrix<Type> sigma_d1 = allret["derivative1"];
    matrix<Type> sigma_d2 = allret["derivative2"];
    this->sigmad1_cache[this->full_visit] = sigma_d1;
    this->sigmad2_cache[this->full_visit] = sigma_d2;
  }
  // Cache and return the first order derivatives using select matrix.
  matrix<Type> get_sigma_derivative1(std::vector<int> visits, matrix<Type> dist) override {
    auto target = this->sigmad1_cache.find(visits);
     if (target != this->sigmad1_cache.end()) {
      return target->second;
    } else {
      int n_visits_i = visits.size();
      matrix<Type> ret = matrix<Type>(this->n_theta * n_visits_i, n_visits_i);
      for (int i = 0; i < this->n_theta; i++) {
        ret.block(i  * n_visits_i, 0, n_visits_i, n_visits_i) = subset_matrix<matrix<Type>, vector<int>>(this->sigmad1_cache[this->full_visit].block(i  * this->n_visits, 0, this->n_visits, this->n_visits), visits, visits);
      }
      this->sigmad1_cache[visits] = ret;
      return ret;
    }
  }
  // Cache and return the second order derivatives using select matrix.
  matrix<Type> get_sigma_derivative2(std::vector<int> visits, matrix<Type> dist) override {
    auto target = this->sigmad2_cache.find(visits);
     if (target != this->sigmad2_cache.end()) {
      return target->second;
    } else {
      int n_visits_i = visits.size();
      int theta_sq = this->n_theta * this->n_theta;
      matrix<Type> ret = matrix<Type>(theta_sq * n_visits_i, n_visits_i);
      for (int i = 0; i < theta_sq; i++) {
        ret.block(i  * n_visits_i, 0, n_visits_i, n_visits_i) = subset_matrix<matrix<Type>, vector<int>>(this->sigmad2_cache[this->full_visit].block(i  * this->n_visits, 0, this->n_visits, this->n_visits), visits, visits);
      }
      this->sigmad2_cache[visits] = ret;
      return ret;
    }
  }
  // Cache and return the lower cholesky factor of inverse of sigma using select matrix.
  matrix<Type> get_inverse_chol(std::vector<int> visits, matrix<Type> dist) override {
    auto target = this->inverse_chol_cache.find(visits);
     if (target != this->inverse_chol_cache.end()) {
      return target->second;
    } else {
      matrix<Type> sigmainv = this->get_sigma_inverse(visits, dist);
      Eigen::LLT<Eigen::Matrix<Type,Eigen::Dynamic,Eigen::Dynamic> > sigma_inv_chol(sigmainv);
      matrix<Type> Li = sigma_inv_chol.matrixL();
      this->inverse_chol_cache[visits] = Li;
      return Li;
    }
  }
  // Cache and return the first order derivatives of inverse of sigma using select matrix.
  matrix<Type> get_inverse_derivative(std::vector<int> visits, matrix<Type> dist) override {
    auto target = this->sigma_inverse_d1_cache.find(visits);
     if (target != this->sigma_inverse_d1_cache.end()) {
      return target->second;
    } else {
      auto sigma_d1 = this->get_sigma_derivative1(visits, dist);
      matrix<Type> sigma_inv_d1(sigma_d1.rows(), sigma_d1.cols());
      int n_visits_i = visits.size();
      auto sigma_inv = this->get_sigma_inverse(visits, dist);
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
struct derivatives_sp_exp: public lower_chol_spatial<Type>, virtual derivatives_base<Type> {
  Type const_sd;
  Type rho;
  Type logrho;
  derivatives_sp_exp() {
    // This default constructor is needed because the use of `[]` in maps.
  }
  // Initialize the theta values; the reason to have theta is that for a fit, the theta
  // is the same for all subjects, while the distance between each visits for each subject
  // can be different.
  derivatives_sp_exp(vector<Type> theta, std::string cov_type): lower_chol_spatial<Type>(theta, cov_type) ,const_sd(exp(theta(0))), rho(invlogit(theta(1))) {
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
  // Obtain the lower cholesky factor of inverse of sigma using select matrix.
  matrix<Type> get_inverse_chol(std::vector<int> visits, matrix<Type> dist) override {
    auto sigmainv = this->get_sigma_inverse(visits, dist);
    Eigen::LLT<Eigen::Matrix<Type,Eigen::Dynamic,Eigen::Dynamic> > sigma_inv_chol(sigmainv);
    matrix<Type> Li = sigma_inv_chol.matrixL();
    return Li;
  }
  // Obtain first order derivatives for inverse of sigma.
  matrix<Type> get_inverse_derivative(std::vector<int> visits, matrix<Type> dist) override {
    matrix<Type> sigma_inv_d1 = matrix<Type>::Zero(2 * dist.rows(), dist.cols());
    auto sigma_inv = this->get_sigma_inverse(visits, dist);
    auto sigma_d1 = this->get_sigma_derivative1(visits, dist);
    for (int r = 0; r < 2; r++) {
      sigma_inv_d1.block(r * dist.rows(), 0, dist.rows(), dist.cols()) = - sigma_inv * sigma_d1.block(r * dist.rows(), 0, dist.rows(), dist.cols()) *sigma_inv;
    }
    return sigma_inv_d1;
  }
};

#endif
