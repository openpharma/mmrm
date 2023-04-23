#ifndef CHOL_CACHE_INCLUDED_
#define CHOL_CACHE_INCLUDED_

#include "covariance.h"
#include "utils.h"
// Base class of spatial and non-spatial Cholesky.
template <class Type>
struct lower_chol_base {
  virtual matrix<Type> get_chol(std::vector<int> visits, matrix<Type> dist) = 0;
};
// Struct to obtain Cholesky for non-spatial.
template <class Type>
struct lower_chol_nonspatial: public lower_chol_base<Type> {
  std::map<std::vector<int>, matrix<Type>> chols;
  std::string cov_type;
  int n_visits;
  std::vector<int> full_visit;
  int n_theta; // theta.size()
  vector<Type> theta;
  matrix<Type> chol_full;
  lower_chol_nonspatial() {
    // This default constructor is needed because the use of `[]` in map.
  }
  // Constructor from theta, n_visits and cov_type, and cache full_visits values.
  lower_chol_nonspatial(vector<Type> theta, int n_visits, std::string cov_type): cov_type(cov_type), n_visits(n_visits), full_visit(std::vector<int>(n_visits)) {
    this->theta = theta;
    std::iota(std::begin(this->full_visit), std::end(this->full_visit), 0);
    this->n_theta = theta.size();
    this->chol_full = get_covariance_lower_chol(this->theta, this->n_visits, this->cov_type);
    this->chols[full_visit]  = this->chol_full;
  }
  matrix<Type> get_chol(std::vector<int> visits, matrix<Type> dist) {
    if (this->chols.count(visits) > 0) {
      return this->chols[visits];
    } else {
      matrix<Type> sel_mat = get_select_matrix<Type>(visits, this->n_visits);
      matrix<Type> Ltildei = sel_mat * this->chol_full;
      matrix<Type> cov_i = tcrossprod(Ltildei);
      Eigen::LLT<Eigen::Matrix<Type,Eigen::Dynamic,Eigen::Dynamic> > cov_i_chol(cov_i);
      matrix<Type> Li = cov_i_chol.matrixL();
      this->chols[visits] = Li;
      return this->chols[visits];
    }
  }
};


// Struct to obtain Cholesky for spatial exponential.
template <class Type>
struct lower_chol_spatial: public lower_chol_base<Type> {
  vector<Type> theta;
  std::string cov_type;
  lower_chol_spatial() {
    // This default constructor is needed because the use of `[]` in map.
  }
  // Constructor from theta. For now the cholesky does not need to be cached.
  lower_chol_spatial(vector<Type> theta, std::string cov_type): theta(theta), cov_type(cov_type) {
  }
  matrix<Type> get_chol(std::vector<int> visits, matrix<Type> dist) {
    return get_spatial_covariance_lower_chol(this->theta, dist, this->cov_type);
  }
};

#endif
