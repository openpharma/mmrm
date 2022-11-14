// Check correctness of 'autdiff' namespace
#include <TMB.hpp>
using std::string;


// Producing a sparse selection matrix to select rows and columns from
// covariance matrix.
template <class Type>
Eigen::SparseMatrix<Type> get_select_matrix(const vector<int>& visits_i, const int& n_visits) {
  Eigen::SparseMatrix<Type> result(visits_i.size(), n_visits);
  for (int i = 0; i < visits_i.size(); i++) {
    result.insert(i, visits_i(i)) = (Type) 1.0;
  }
  return result;
}

// Calculate tcrossprod(lower_chol) = lower_chol * t(lower_chol).
// If complete, then adds the upper triangular part to the result as well.
// By default only the lower triangular part is populated, as this should be
// sufficient for downstream use of the result in most cases.
template <class Type>
matrix<Type> tcrossprod(const matrix<Type>& lower_chol, bool complete = false) {
  int n = lower_chol.rows();
  matrix<Type> result = matrix<Type>::Zero(n, n);
  result.template selfadjointView<Eigen::Lower>().rankUpdate(lower_chol);
  if (complete) {
    result.template triangularView<Eigen::Upper>() = result.transpose();
  }
  return result;
}

// Calculate crossprod(x) = t(x) * x.
// Only the lower triangular part is populated, as this should be
// sufficient for downstream use of the result in most cases.
// Note that x does not need to be symmetric or square.
template <class Type>
matrix<Type> crossprod(const matrix<Type>& x) {
  int n = x.cols();
  matrix<Type> result = matrix<Type>::Zero(n, n);
  result.template selfadjointView<Eigen::Lower>().rankUpdate(x.transpose());
  return result;
}

// Mapping from real values to correlation parameters in (-1, 1).
template <class T>
vector<T> map_to_cor(const vector<T>& theta) {
  return theta / sqrt(T(1.0) + theta * theta);
}

// Generic correlation function class containing and initializing correlation
// values from variance parameters theta.
template <class T>
struct generic_corr_fun {
  const vector<T> corr_values;

  generic_corr_fun(const vector<T>& theta) :
    corr_values(map_to_cor(theta)) {}
};

// Correlation function based Cholesky factor of correlation matrix.
// This is used directly for homogeneous covariance matrices.
template <class T, template<class> class F>
matrix<T> get_corr_mat_chol(int n_visits, const F<T>& corr_fun) {
  matrix<T> correlation(n_visits, n_visits);
  correlation.setIdentity();
  for(int i = 0; i < n_visits; i++) {
    for(int j = 0; j < i; j++){
      correlation(i, j) = corr_fun(i, j);
    }
  }
  Eigen::LLT<Eigen::Matrix<T,Eigen::Dynamic,Eigen::Dynamic> > correlation_chol(correlation);
  matrix<T> L = correlation_chol.matrixL();
  return L;
}

// Heterogeneous covariance matrix calculation given vector of standard deviations (sd_values)
// and a correlation function (corr_fun).
template <class T, template<class> class F>
matrix<T> get_heterogeneous_cov(const vector<T>& sd_values, const F<T>& corr_fun) {
  matrix<T> correlation_chol = get_corr_mat_chol(sd_values.size(), corr_fun);
  Eigen::DiagonalMatrix<T,Eigen::Dynamic,Eigen::Dynamic> D = sd_values.matrix().asDiagonal();
  matrix<T> result = D * correlation_chol;
  return result;
}

// Obtain the Euclidean distance
template <class T>
matrix<T> euclidean(const matrix<T>& coordinates) {
  matrix<T> result(coordinates.rows(), coordinates.rows());
  for (int i = 0; i < coordinates.rows(); i++) {
    result(i, i) = 0;
    for (int j = 0; j < i; j ++) {
      vector<T> diff = coordinates.row(i) - coordinates.row(j);
      T d = sqrt((diff * diff).sum());
      result(i, j) = d;
      result(j, i) = d;
    }
  }
  return result;
}

// Unstructured covariance:
// Cholesky factor.
template <class T>
matrix<T> get_unstructured(const vector<T>& theta, int n_visits) {
  vector<T> sd_values = exp(theta.head(n_visits));
  vector<T> lower_tri_chol_values = theta.tail(theta.size() - n_visits);
  matrix<T> covariance_lower_chol = matrix<T>::Zero(n_visits, n_visits);
  int k = 0;
  for(int i = 0; i < n_visits; i++) {
    covariance_lower_chol(i, i) = sd_values(i);
    for(int j = 0; j < i; j++){
      covariance_lower_chol(i, j) = sd_values(i) * lower_tri_chol_values(k++);
    }
  }
  return covariance_lower_chol;
}

// Ante-dependence:

// Correlation function.
template <class T>
struct corr_fun_ante_dependence : generic_corr_fun<T> {
  using generic_corr_fun<T>::generic_corr_fun;
  const T operator() (int i, int j) const {
    return this->corr_values.segment(j, i - j).prod();
  }
};
// Homogeneous Ante-dependence Cholesky factor.
template <class T>
matrix<T> get_ante_dependence(const vector<T>& theta, int n_visits) {
  T const_sd = exp(theta(0));
  corr_fun_ante_dependence<T> fun(theta.tail(n_visits - 1));
  matrix<T> ad_cor_mat_chol = get_corr_mat_chol(n_visits, fun);
  return const_sd * ad_cor_mat_chol;
}
// Heterogeneous Ante-dependence Cholesky factor.
template <class T>
matrix<T> get_ante_dependence_heterogeneous(const vector<T>& theta, int n_visits) {
  vector<T> sd_values = exp(theta.head(n_visits));
  corr_fun_ante_dependence<T> fun(theta.tail(n_visits - 1));
  return get_heterogeneous_cov(sd_values, fun);
}

// Toeplitz:

// Correlation function.
template <class T>
struct corr_fun_toeplitz : generic_corr_fun<T> {
  using generic_corr_fun<T>::generic_corr_fun;
  const T operator() (int i, int j) const {
    int index = (i - j) - 1;  // Note: We need to start at 0.
    return this->corr_values(index);
  }
};
// Homogeneous Toeplitz Cholesky factor.
template <class T>
matrix<T> get_toeplitz(const vector<T>& theta, int n_visits) {
  T const_sd = exp(theta(0));
  corr_fun_toeplitz<T> fun(theta.tail(n_visits - 1));
  matrix<T> toep_cor_mat_chol = get_corr_mat_chol(n_visits, fun);
  return const_sd * toep_cor_mat_chol;
}
// Heterogeneous Toeplitz Cholesky factor.
template <class T>
matrix<T> get_toeplitz_heterogeneous(const vector<T>& theta, int n_visits) {
  vector<T> sd_values = exp(theta.head(n_visits));
  corr_fun_toeplitz<T> fun(theta.tail(n_visits - 1));
  return get_heterogeneous_cov(sd_values, fun);
}

// Autoregressive:

// Correlation function.
template <class T>
struct corr_fun_autoregressive : generic_corr_fun<T> {
  using generic_corr_fun<T>::generic_corr_fun;
  const T operator() (int i, int j) const {
    T diff = T((i - j) * 1.0);
    return pow(this->corr_values(0), diff);  // rho^{|i-j|}
  }
};
// Homogeneous autoregressive Cholesky factor.
template <class T>
matrix<T> get_auto_regressive(const vector<T>& theta, int n_visits) {
  T const_sd = exp(theta(0));
  corr_fun_autoregressive<T> fun(theta.tail(1));
  matrix<T> ar1_cor_mat_chol = get_corr_mat_chol(n_visits, fun);
  return const_sd * ar1_cor_mat_chol;
}
// Heterogeneous autoregressive Cholesky factor.
template <class T>
matrix<T> get_auto_regressive_heterogeneous(const vector<T>& theta, int n_visits) {
  vector<T> sd_values = exp(theta.head(n_visits));
  corr_fun_autoregressive<T> fun(theta.tail(1));
  return get_heterogeneous_cov(sd_values, fun);
}

// Compound symmetry:

// Correlation function.
template <class T>
struct corr_fun_compound_symmetry : generic_corr_fun<T> {
  using generic_corr_fun<T>::generic_corr_fun;
  const T operator() (int i, int j) const {
    return this->corr_values(0);  // rho (constant)
  }
};
// Homogeneous compound symmetry Cholesky factor.
template <class T>
matrix<T> get_compound_symmetry(const vector<T>& theta, int n_visits) {
  T const_sd = exp(theta(0));
  corr_fun_compound_symmetry<T> fun(theta.tail(1));
  matrix<T> cs_cor_mat_chol = get_corr_mat_chol(n_visits, fun);
  return const_sd * cs_cor_mat_chol;
}
// Heterogeneous compound symmetry Cholesky factor.
template <class T>
matrix<T> get_compound_symmetry_heterogeneous(const vector<T>& theta, int n_visits) {
  vector<T> sd_values = exp(theta.head(n_visits));
  corr_fun_compound_symmetry<T> fun(theta.tail(1));
  return get_heterogeneous_cov(sd_values, fun);
}

// Spatial Exponential Cholesky factor.
template <class T>
matrix<T> get_spatial_exponential(const vector<T>& theta, const matrix<T>& distance) {
  T const_sd = exp(theta(0));
  T rho = invlogit(theta(1));
  matrix<T> expdist = exp(distance.array() * log(rho));
  matrix<T> result = expdist * const_sd;
  Eigen::LLT<Eigen::Matrix<T,Eigen::Dynamic,Eigen::Dynamic> > cov_i_chol(result);
  return cov_i_chol.matrixL();
}

// Creates a new correlation object dynamically.
template <class T>
matrix<T> get_covariance_lower_chol(const vector<T>& theta, int n_visits, std::string cov_type) {
  matrix<T> result;

  if (cov_type == "us") {
    result = get_unstructured<T>(theta, n_visits);
  } else if (cov_type == "toep") {
    result = get_toeplitz<T>(theta, n_visits);
  } else if (cov_type == "toeph") {
    result = get_toeplitz_heterogeneous<T>(theta, n_visits);
  } else if (cov_type == "ar1") {
    result = get_auto_regressive<T>(theta, n_visits);
  } else if (cov_type == "ar1h") {
    result = get_auto_regressive_heterogeneous<T>(theta, n_visits);
  } else if (cov_type == "ad") {
    result = get_ante_dependence<T>(theta, n_visits);
  } else if (cov_type == "adh") {
    result = get_ante_dependence_heterogeneous<T>(theta, n_visits);
  } else if (cov_type == "cs") {
    result = get_compound_symmetry<T>(theta, n_visits);
  } else if (cov_type == "csh") {
    result = get_compound_symmetry_heterogeneous<T>(theta, n_visits);
  } else {
    Rf_error(("Unknown covariance type '" + cov_type + "'.").c_str());
  }

  return result;
}

// Creates a new spatial covariance cholesky.
template <class T>
matrix<T> get_spatial_covariance_lower_chol(const vector<T>& theta, const matrix<T>& distance, std::string cov_type) {
  matrix<T> result;
  if (cov_type == "sp_exp") {
    result = get_spatial_exponential<T>(theta, distance);
  } else {
    Rf_error(("Unknown spatial covariance type '" + cov_type + "'.").c_str());
  }
  return result;
}

// Creates a grouped correlation object dynamically.
template <class T>
matrix<T> get_cov_lower_chol_grouped(const vector<T>& theta, int dim_cov_mat, std::string cov_type, int n_groups, bool is_spatial) {
  matrix<T> result(dim_cov_mat * n_groups, dim_cov_mat);
  int covariance_size = theta.size() / n_groups;
  if (is_spatial) {
    matrix<T> standard_dist(2, 2);
    standard_dist << 0, 1, 1, 0;
    for (int i = 0; i < n_groups; i++) {
      matrix<T> lower_chol = get_spatial_covariance_lower_chol(vector<T>(theta.segment(2 * i, covariance_size)), standard_dist, cov_type);
      result << result.block(0, 0, dim_cov_mat * i, dim_cov_mat), lower_chol;
    }
  } else {
    for (int i = 0; i < n_groups; i++) {
      matrix<T> lower_chol = get_covariance_lower_chol<T>(theta.segment(i * covariance_size, covariance_size), dim_cov_mat, cov_type);
      result << result.block(0, 0, dim_cov_mat * i, dim_cov_mat), lower_chol;
    }
  }

  return result;
}


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

struct func1 {
  template <class T>
    T operator() (vector<T> x) {  // Evaluate function
      return x.prod();
    }
};


template<class Type>
Type objective_function<Type>::operator() ()
{
  DATA_INTEGER(n_visits);
  DATA_STRING(cov_type);
  PARAMETER_VECTOR(theta);
  chol c1(n_visits, cov_type);
  chol_jacobian c2(n_visits, cov_type);
  func1 f;
  matrix<Type> l = c1(theta).matrix();
  l.resize(n_visits, n_visits);
  std::map<std::vector<int>,matrix<Type>> aa;
  auto a1 = std::vector<int>(2);
  a1 = {1, 2};
  matrix<Type> ddd = matrix<Type>::Zero(n_visits, n_visits);
  aa[a1] = ddd;
  if (aa.count(a1) > 0) {
    std::cout << "found a1" << ' ';
  }
  
  auto a3 = std::vector<int>(2);
  a3[0] = 1;
  a3[1] = 2;
  if (aa.count(a3) > 0) {
    std::cout << "found a3" << ' ';
  }
  auto zxd = a1 == a3;
  REPORT(zxd);
  matrix<Type> sigma = tcrossprod(l, true);
  matrix<Type> sigmainv = sigma.inverse();
  matrix<Type> g = autodiff::jacobian(c1, theta); // g is (dim * dim, l_theta)
  matrix<Type> h = autodiff::jacobian(c2, theta); // h is (dim * dim * l_theta, l_theta)
  g.block(0, 0, n_visits, theta.size()) = matrix<Type>::Zero(n_visits, theta.size());
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
  //auto zzz = aa[a1];
  auto zzz = aa[a3];
  REPORT(zzz);
  REPORT(g);
  REPORT(ld1);
  REPORT(h);
  REPORT(ld2);
  REPORT(sigmainv);
  REPORT(l);
  REPORT(sigma_d1);
  REPORT(sigma_d2);
  ADREPORT(f(theta));
  return 0;
}
