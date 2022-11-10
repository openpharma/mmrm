#include <Rcpp.h>
#define INCLUDE_RCPP
#include "utils.h"
#include "covariance.h"

using namespace Rcpp;
using std::string;

struct chol {
  int dim_cov_mat;
  string cov_type;
  chol(int dim, string cov): dim_cov_mat(dim), cov_type(cov) {};
  template <class T>
    vector<T> operator() (vector<T> &theta) {  // Evaluate function
      //return theta;
      return get_cov_lower_chol_grouped(theta, this->dim_cov_mat, this->cov_type, 1, false).vec();
    }
};

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

template<class Type>
struct return_result {
  Type value;
  return_result(Type value): value(value) {};
  Type operator() () {
    return this->value;
  }
};

template<class Type>
vector<Type> derivatives(int n_visits, std::string cov_type, vector<Type> theta) {
  chol c1(n_visits, cov_type);
  chol_jacobian c2(n_visits, cov_type);

  matrix<Type> l = c1(theta).matrix();
  l.resize(n_visits, n_visits);
  matrix<Type> sigma = tcrossprod(l, true);
  matrix<Type> sigmainv = sigma.inverse();
  matrix<Type> g = autodiff::jacobian(c1, theta); // g is (dim * dim, l_theta)
  matrix<Type> h = autodiff::jacobian(c2, theta); // h is (dim * dim * l_theta, l_theta)
  // report only works for matrix, vector, scaler or int; so even if we have that derivative we need to
  // extract it outside c++;
  // or we can use Rcpp to do that? Rcpp supports seems that it can be complicated with multiple denepdencies
  array<Type> ld1 = array<Type>(n_visits, n_visits, theta.size());
  ld1 << g.vec();
  array<Type> ld2 = array<Type>(n_visits, n_visits, theta.size(), theta.size());
  ld2 << h.vec();
  array<Type> sigma_d1 = array<Type>(n_visits, n_visits, theta.size());
  array<Type> sigma_d2 = array<Type>(n_visits, n_visits, theta.size(), theta.size());
  for (int i = 0; i < theta.size(); i++) {
    matrix<Type> d1 = ld1.col(i).matrix();
    d1.resize(n_visits, n_visits);
    matrix<Type> a = d1 * l.transpose();
    sigma_d1.col(i) = a + a.transpose();
    for (int j = 0; j < theta.size(); j++) {
      //matrix<Type> d2(n_visits, n_visits);
      //d2 << ld2.col(i).segment(j * n_visits, n_visits * n_visits);
      //d2.resize(n_visits, n_visits);
      //sigma_d2.segment(i * n_visits * n_visits + j * n_visits, n_visits * n_visits) = 2 * (d2 * l.transpose() + tcrossprod(d1, true));
    }
  }
  return theta;
}

vector<double> as_vector(NumericVector input) {
  vector<double> ret(input.size());
  for (int i = 0; i < input.size(); i++) {
    ret(i) = input[i];
  }
  return ret;
}
NumericVector as_nv(vector<double> input) {
  NumericVector ret(input.size());
  for (int i = 0; i < input.size(); i++) {
    ret[i] = input(i);
  }
  return ret;
}

template<class Type>
Type aaa(vector<Type> ii) {
  return ii(0);
}

// [[Rcpp::export]]
double test(int n_visits, std::string cov_type, NumericVector theta) {
  return return_result<double>(0.0)();
}

