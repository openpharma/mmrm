#ifndef CHOL_CACHE_INCLUDED_
#define CHOL_CACHE_INCLUDED_

#include "covariance.h"
#include "utils.h"

// Base class of spatial and non-spatial Cholesky.
template <class Type>
struct lower_chol_base {
  virtual ~lower_chol_base() {}
  virtual matrix<Type> get_chol(std::vector<int> visits, matrix<Type> dist) = 0;
  virtual matrix<Type> get_sigma(std::vector<int> visits, matrix<Type> dist) = 0;
  virtual matrix<Type> get_sigma_inverse(std::vector<int> visits, matrix<Type> dist) = 0;
};
// Struct to obtain Cholesky for non-spatial.
template <class Type>
struct lower_chol_nonspatial: virtual lower_chol_base<Type> {
  std::map<std::vector<int>, matrix<Type>> chols;
  std::map<std::vector<int>, matrix<Type>> sigmas;
  std::map<std::vector<int>, matrix<Type>> sigmas_inv;
  std::string cov_type;
  int n_visits;
  std::vector<int> full_visit;
  int n_theta;
  vector<Type> theta;
  matrix<Type> chol_full;
  matrix<Type> sigma_full;
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
    this->sigma_full = tcrossprod(this->chol_full, true);
  }
  matrix<Type> get_chol(std::vector<int> visits, matrix<Type> dist) {
    auto target = this->chols.find(visits);
     if (target != this->chols.end()) {
      return target->second;
    } else {
      matrix<Type> cov_i = this->get_sigma(visits, dist);
      Eigen::LLT<Eigen::Matrix<Type,Eigen::Dynamic,Eigen::Dynamic> > cov_i_chol(cov_i);
      matrix<Type> Li = cov_i_chol.matrixL();
      this->chols[visits] = Li;
      return Li;
    }
  }
  matrix<Type> get_sigma(std::vector<int> visits, matrix<Type> dist) {
    auto target = this->sigmas.find(visits);
    if (target != this->sigmas.end()) {
      return target->second;
    } else {
      matrix<Type> ret = subset_matrix<matrix<Type>, vector<int>>(sigma_full, visits, visits);
      this->sigmas[visits] = ret;
      return ret;
    }
  }
  matrix<Type> get_sigma_inverse(std::vector<int> visits, matrix<Type> dist) {
    auto target = this->sigmas_inv.find(visits);
    if (target != this->sigmas_inv.end()) {
      return target->second;
    } else {
      matrix<Type> ret = this->get_sigma(visits, dist).inverse();
      this->sigmas_inv[visits] = ret;
      return ret;
    }
  }
};


// Struct to obtain Cholesky for spatial exponential.
template <class Type>
struct lower_chol_spatial: virtual lower_chol_base<Type> {
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
  matrix<Type> get_sigma(std::vector<int> visits, matrix<Type> dist) {
    return tcrossprod(this->get_chol(visits, dist), true);
  }
  matrix<Type> get_sigma_inverse(std::vector<int> visits, matrix<Type> dist) {
    return this->get_sigma(visits, dist).inverse();
  }
};

template <class T, class Base, class D1, class D2>
struct cache_obj {
  std::map<int, std::shared_ptr<Base>> cache;
  int n_groups;
  bool is_spatial;
  int n_visits;
  cache_obj(vector<T> theta, int n_groups, bool is_spatial, std::string cov_type, int n_visits): n_groups(n_groups), is_spatial(is_spatial), n_visits(n_visits) {
    // Get number of variance parameters for one group.
    int theta_one_group_size = theta.size() / n_groups;
    for (int r = 0; r < n_groups; r++) {
      // Use unique pointers here to better manage resource.
      if (is_spatial) {
        this->cache[r] = std::make_shared<D1>(theta.segment(r * theta_one_group_size, theta_one_group_size), cov_type);
      } else {
        this->cache[r] = std::make_shared<D2>(theta.segment(r * theta_one_group_size, theta_one_group_size), n_visits, cov_type);
      }
    }
  }
};

template <class Type>
struct chol_cache_groups: cache_obj<Type, lower_chol_base<Type>, lower_chol_spatial<Type>, lower_chol_nonspatial<Type>> {
  chol_cache_groups(vector<Type> theta, int n_groups, bool is_spatial, std::string cov_type, int n_visits): cache_obj<Type, lower_chol_base<Type>, lower_chol_spatial<Type>, lower_chol_nonspatial<Type>>(theta, n_groups, is_spatial, cov_type, n_visits) {

  }
  // Return covariance lower Cholesky factor from lower_chol_base objects.
  // For non-spatial return for full visits, for spatial return on two points that the distance is 1.
  matrix<Type> get_default_chol() {
    std::vector<int> visit(this->n_visits);
    std::iota(std::begin(visit), std::end(visit), 0);
    matrix<Type> dist(2, 2);
    dist << 0, 1, 1, 0;
    int dim = this->is_spatial?2:this->n_visits;
    matrix<Type> covariance_lower_chol = matrix<Type>::Zero(dim * this->n_groups, dim);
    for (int r = 0; r < this->n_groups; r++) {
      covariance_lower_chol.block(r * dim, 0, dim, dim) = this->cache[r]->get_chol(visit, dist);
    }
    return covariance_lower_chol;
  }
};

#endif
