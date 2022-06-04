// Author: Daniel Sabanes based on Ben Bolker example

// Idea: Prototype a specific MMRM with TMB.
//
// Definition:
//
// Y_i = X_i * beta + epsilon_i, i = 1, ..., n_subjects
// where Y_i = (Y_i1, ..., Y_im) are the observations of subject i
// over the m timepoints,
//
// and for the epsilon_i's:
// epsilon_i ~iid N(0, Sigma) where Sigma is an unstructured m x m covariance matrix
// parametrized by a vector theta.
//
// Note: This is a special generalized least squares model
// Y = X * beta + epsilon,
// where we have a block structure for the covariance matrix of the epsilon
// vector.

// Questions:
// - How can we elegantly deal with missing time points for patients?
//   -> Not a problem for beta, but for the epsilon_i since its Sigma
//      is then a subset of the m x m overall Sigma.
//   -> Should be supported already by the keep argument, see
//      https://kaskr.github.io/adcomp/density_8hpp_source.html#l00155
//   -> So we could define a length m vector keep_i for each patient that indicates
//      to which timepoints the observations belong to.

#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() ()
{
  using namespace density;
  using namespace Eigen;

  // Read data from R.
  DATA_MATRIX(X);               // Model matrix (dimension n x p).
  DATA_VECTOR(Y);               // Response vector (length n).
  DATA_IVECTOR(visits);         // Visits vector (length n).
  DATA_INTEGER(n_visits);       // Number of visits, which is the dimension of the covariance matrix.
  DATA_INTEGER(n_subjects);     // Number of subjects.
  DATA_IVECTOR(subject_inds);   // Starting indices for each subject (0-based) (length n_subjects).
  DATA_IVECTOR(subject_n_visits); // Number of observed visits for each visit (length n_subjects).

  // Read parameters from R.
  PARAMETER_VECTOR(beta);       // Coefficient vector (length p).
  PARAMETER_VECTOR(theta);      // Covariance parameters (length k). Starts with log standard deviations
                                // and continues with entries of lower triangular Cholesky factor.

  // Initialize negative log-likelihood value.
  Type neg_log_lik = 0;

  // Construct negative log density for multivariate N(0, Sigma) where
  // Sigma is an unstructured covariance matrix parametrized by theta.
  vector<Type> log_sd_values = theta.head(n_visits);
  vector<Type> lower_tri_chol_values = theta.tail(theta.size() - n_visits);
  UNSTRUCTURED_CORR_t<Type> unscaled_neg_log_dmvnorm(theta);
  VECSCALE_t<UNSTRUCTURED_CORR_t<Type> > neg_log_dmvnorm = VECSCALE(unscaled_neg_log_dmvnorm, exp(log_sd_values));

  // Calculate residual vector epsilon = Y - X * beta.
  vector<Type> residuals = Y - X * beta;

  // Add negative log density epsilon_i ~ N(0, Sigma) for each subject i,
  // taking into account the different visits they might have.
  for (int i = 0; i < n_subjects; i++) {
    // Obtain the residuals for this subject.
    Type start_i = subject_inds(i);
    Type n_visits_i = subject_n_visits(i);
    vector<Type> residual_i = residuals.segment(start_i, n_visits_i);
    // Obtain the visits for this subject.
    vector<Type> visits_i = visits.segment(start_i, n_visits_i);
    // Create a possible enlarged vector x of dimension n_visits for this subject,
    // where the residual values are put in the right positions.
    vector<Type> x_i(0, n_visits);
    x_i[visits_i - 1] = residual_i;
    // The keep vector says which ones are the real observations to keep (value 1, otherwise value 0).
    vector<Type> keep_i(0, n_visits);
    keep_i[visits_i - 1] = 1;
    // Add the negative log density.
    neg_log_lik += neg_log_dmvnorm(x_i, keep_i);
  }

  // Return negative log-likelihood.
  return neg_log_lik;
}
