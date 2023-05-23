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
  int n_theta;
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
    auto target = this->chols.find(visits);
     if (target != this->chols.end()) {
      return target->second;
    } else {
      matrix<Type> sel_mat = get_select_matrix<Type>(visits, this->n_visits);
      matrix<Type> Ltildei = sel_mat * this->chol_full;
      matrix<Type> cov_i = tcrossprod(Ltildei);
      Eigen::LLT<Eigen::Matrix<Type,Eigen::Dynamic,Eigen::Dynamic> > cov_i_chol(cov_i);
      matrix<Type> Li = cov_i_chol.matrixL();
      this->chols[visits] = Li;
      return Li;
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

// Return covariante lower Cholesky factor from lower_chol_base objects.
// For non-spatial return for full visits, for spatial return on two points that the distance is 1.
// Finally clean up the map of `chols`.
template <class Type>
matrix<Type> get_chol_and_clean(std::map<int, lower_chol_base<Type>*>& chols, bool is_spatial, int n_visits) {
  std::vector<int> visit(n_visits);
  std::iota(std::begin(visit), std::end(visit), 0);
  matrix<Type> dist(2, 2);
  dist << 0, 1, 1, 0;
  int dim = is_spatial?2:n_visits;
  matrix<Type> covariance_lower_chol = matrix<Type>::Zero(dim * chols.size(), dim);
  int n_groups = chols.size();
  for (int r = 0; r < n_groups; r++) {
    covariance_lower_chol.block(r * dim, 0, dim, dim) = chols[r]->get_chol(visit, dist);
    delete chols[r];
    chols.erase(r);
  }
  return covariance_lower_chol;
}

#endif
