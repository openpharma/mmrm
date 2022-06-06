// Author: Daniel Sabanes based on Ben Bolker example
// v2 with explicit covariance matrix calculation

// Idea: Prototype a specific MMRM with TMB.
//
// Definition:
//
// Y_i = X_i * beta + epsilon_i, i = 1, ..., n_subjects
// where Y_i = (Y_i1, ..., Y_im) are the observations of subject i
// over the m timepoints,
//
// and for the epsilon_i's :
// epsilon_i ~iid N(0, Sigma) where Sigma is an unstructured m x m covariance matrix
// parametrized by a vector theta.
//
// Note: This is a special generalized least squares model
// Y = X * beta + epsilon,
// where we have a block structure for the covariance matrix of the epsilon
// vector.

#include <TMB.hpp>

// Function class to get the visits covariance matrix.
struct covariance_fun {
  template <class T>
  matrix<T> operator() (vector<T> theta) {  // Evaluate function
    int n_theta = theta.size();
    int n_visits = int(floor(sqrt(2.0 * n_theta)));
    vector<T> sd_values = exp(theta.head(n_visits));
    vector<T> lower_tri_chol_values = theta.tail(n_theta - n_visits);
    density::UNSTRUCTURED_CORR_t<T> unscaled_neg_log_dmvnorm(lower_tri_chol_values);
    matrix<T> corr_mat = unscaled_neg_log_dmvnorm.cov();
    matrix<T> sd_mat = Eigen::DiagonalMatrix<T, Eigen::Dynamic>(n_visits);
    sd_mat.diagonal() = sd_values;
    matrix<T> result = sd_mat * corr_mat * sd_mat;
    return result;
  }
};

template<class Type>
Type objective_function<Type>::operator() ()
{
  using namespace density;
  using namespace Eigen;

  // Read data from R.
  DATA_INTEGER(select);         // 1: Calculate negative log likelihood,
                                // 2: Calculate covariance matrix.
  DATA_MATRIX(X);               // Model matrix (dimension n x p).
  DATA_VECTOR(Y);               // Response vector (length n).
  DATA_IVECTOR(visits_zero);    // Zero-based Visits vector (length n).
  DATA_INTEGER(n_visits);       // Number of visits, which is the dimension of the covariance matrix.
  DATA_INTEGER(n_subjects);     // Number of subjects.
  DATA_IVECTOR(subject_inds);   // Starting indices for each subject (0-based) (length n_subjects).
  DATA_IVECTOR(subject_n_visits); // Number of observed visits for each subject (length n_subjects).

  // Read parameters from R.
  PARAMETER_VECTOR(beta);       // Coefficient vector (length p).
  PARAMETER_VECTOR(theta);      // Covariance parameters (length k). Starts with log standard deviations
                                // and continues with entries of lower triangular Cholesky factor.

  // Initialize negative log-likelihood value.
  Type neg_log_lik = 0.0;

  if (select == 1) {
    // 1: Calculate negative log likelihood.

    // Construct negative log density for multivariate N(0, Sigma) where
    // Sigma is an unstructured covariance matrix parametrized by theta.
    // Note: For now we use the unscaled version and do the scaling manually.
    vector<Type> log_sd_values = theta.head(n_visits);
    vector<Type> lower_tri_chol_values = theta.tail(theta.size() - n_visits);
    UNSTRUCTURED_CORR_t<Type> unscaled_neg_log_dmvnorm(lower_tri_chol_values);
    // VECSCALE_t<UNSTRUCTURED_CORR_t<Type> > neg_log_dmvnorm = VECSCALE(unscaled_neg_log_dmvnorm, exp(log_sd_values));

    // Calculate residual vector epsilon = Y - X * beta.
    vector<Type> residuals = Y - X * beta;

    // Add negative log density epsilon_i ~ N(0, Sigma) for each subject i,
    // taking into account the different visits they might have.
    for (int i = 0; i < n_subjects; i++) {
      // Obtain the residuals for this subject.
      int start_i = subject_inds(i);
      int n_visits_i = subject_n_visits(i);
      vector<Type> residual_i = residuals.segment(start_i, n_visits_i);
      // Obtain the zero-based visits for this subject.
      vector<int> visits_i = visits_zero.segment(start_i, n_visits_i);
      // Create a possible enlarged vector x of dimension n_visits for this subject,
      // where the residual values are put in the right positions.
      // See https://eigen.tuxfamily.org/dox/group__TutorialAdvancedInitialization.html for zero constructor.
      vector<Type> x_i = vector<Type>::Zero(n_visits);
      // The keep vector says which ones are the real observations to keep (value 1, otherwise value 0).
      vector<Type> keep_i = vector<Type>::Zero(n_visits);
      for (int j = 0; j < n_visits_i; j++) {
        int this_visit = visits_i(j);
        x_i(this_visit) = residual_i(j);
        keep_i(this_visit) = 1.0;
      }
      // Here we scale with the standard deviations.
      x_i = x_i / exp(log_sd_values);
      // Add the negative log density. Something goes wrong here with indexing.
      neg_log_lik += unscaled_neg_log_dmvnorm(x_i, keep_i);
      // Add the scaling contribution.
      neg_log_lik += (log_sd_values(visits_i)).sum();
    }

  } else if (select == 2)  {
    // 2: Calculate covariance matrix.

    // The visits covariance matrix.
    covariance_fun f;
    matrix<Type> cov_mat = f(theta);
    REPORT(cov_mat);

    // Now calculate the covariance matrix of beta.
    int dim_beta = X.cols();
    matrix<Type> beta_cov_mat = matrix<Type>::Zero(dim_beta, dim_beta);

    for (int i = 0; i < n_subjects; i++) {
      // Going through all subjects, we add X_i^T * Sigma * X_i to beta_cov_mat.
      int start_i = subject_inds(i);
      int n_visits_i = subject_n_visits(i);
      vector<int> visits_i = visits_zero.segment(start_i, n_visits_i);
      matrix<Type> this_cov_mat = cov_mat(visits_i, visits_i);
      matrix<Type> X_i = X(Eigen::seqN(start_i, n_visits_i), all);
      matrix<Type> this_contribution = X_i.transpose() * this_cov_mat * X_i;
      beta_cov_mat += this_contribution;
    }

    REPORT(beta_cov_mat);

  } else {
    // Else: Error.

    std::abort();

  }

  // Return negative log-likelihood.
  return neg_log_lik;
}
