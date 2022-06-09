#ifndef TMB_INCLUDED_
#define TMB_INCLUDED_
#include <TMB.hpp>
#endif

#include "correlation.hpp"
#include "leastsquares.hpp"

// Definition:
//
// Y_i = X_i * beta + epsilon_i, i = 1, ..., n_subjects
// where Y_i = (Y_i1, ..., Y_im) are the observations of subject i
// over the m timepoints,
//
// and for the epsilon_i's :
// epsilon_i ~iid N(0, Sigma) where Sigma is an covariance matrix
// parametrized by a vector theta.
//
// Note: This is a special generalized least squares model
// Y = X * beta + epsilon,
// where we have a block structure for the covariance matrix of the epsilon
// vector.
//
// Given theta and therefore Sigma, and writing W = Sigma^-1, we can determine
// beta via the weighted least squares equation
// (X^T W X) beta = X^T W Y.
// Therefore beta itself is not a parameter for TMB here.

template<class Type>
Type objective_function<Type>::operator() ()
{
  // Read data from R.
  DATA_MATRIX(x_matrix);           // Model matrix (dimension n x p).
  DATA_VECTOR(y_vector);           // Response vector (length n).
  DATA_IVECTOR(visits_zero_inds);  // Zero-based Visits vector (length n).
  DATA_INTEGER(n_visits);          // Number of visits, which is the dimension of the covariance matrix.
  DATA_INTEGER(n_subjects);        // Number of subjects.
  DATA_IVECTOR(subject_zero_inds); // Starting indices for each subject (0-based) (length n_subjects).
  DATA_IVECTOR(subject_n_visits);  // Number of observed visits for each subject (length n_subjects).
  DATA_INTEGER(corr_type);         // Correlation type.

  // Read parameters from R.
  PARAMETER_VECTOR(theta);         // Covariance parameters (length k). Contents depend on correlation type.

  // X^T W X will be calculated incrementally into here.
  matrix<Type> XtWX = matrix<Type>::Zero(x_matrix.cols(), x_matrix.cols());
  // X^T W Y will be calculated incrementally into here.
  vector<Type> XtWY = vector<Type>::Zero(x_matrix.cols());
  // W^T/2 X will be saved into here.
  matrix<Type> x_mat_tilde = matrix<Type>::Zero(x_matrix.rows(), x_matrix.cols());
  // W^T/2 Y will be saved into here.
  vector<Type> y_vec_tilde = vector<Type>::Zero(y_vector.rows());
  // Sum of the log determinant will be incrementally calculated here.
  Type sum_log_det = 0.0;

  // Dynamically create the correlation object.
  Correlation<Type>* corr = get_correlation<Type>(theta, corr_type, n_visits);

  // Go through all subjects and calculate quantities initialized above.
  for (int i = 0; i < n_subjects; i++) {
    // Obtain the residuals for this subject.
    int start_i = subject_zero_inds(i);
    int n_visits_i = subject_n_visits(i);

    // Define lower triangular covariance factor for this subject.
    matrix<Type> Li;
    if (n_visits_i < n_visits) {
      // Obtain the zero-based visits for this subject.
      vector<int> visits_i = visits_zero_inds.segment(start_i, n_visits_i);
      matrix<Type> sel_mat = get_select_matrix<Type>(visits_i, n_visits);
      matrix<Type> Ltildei = sel_mat.transpose() * corr->covariance_lower_chol;
      matrix<Type> cov_i = Ltildei * Ltildei.transpose();
      Eigen::LLT<Eigen::Matrix<Type,Eigen::Dynamic,Eigen::Dynamic> > cov_i_chol(cov_i);
      Li = cov_i_chol.matrixL();
    } else {
      Li = corr->covariance_lower_chol;
    }
    matrix<Type> LiInverse = Li.inverse();  // Todo: improve this to use Cholesky etc.
    // Ensure we delete the correlation object.

    // Calculate scaled design matrix and response vector for this subject.
    matrix<Type> Xi = x_matrix.block(start_i, 0, n_visits_i, x_matrix.cols());
    matrix<Type> XiTilde = LiInverse * Xi;
    vector<Type> Yi = y_vector.segment(start_i, n_visits_i);
    vector<Type> YiTilde = LiInverse * Yi;

    // Increment quantities.
    matrix<Type> XiTildeT = XiTilde.transpose();
    XtWX += XiTildeT * XiTilde;
    XtWY += XiTildeT * YiTilde;
    vector<Type> LiDiag = Li.diagonal();
    sum_log_det += sum(log(LiDiag));

    // Save stuff.
    x_mat_tilde.block(start_i, 0, n_visits_i, x_matrix.cols()) = XiTilde;
    y_vec_tilde.segment(start_i, n_visits_i) = YiTilde;
  }

  // Solve for beta.
  matrix<Type> XtWXinv = XtWX.inverse();
  vector<Type> beta = XtWXinv * XtWY;

  // Define scaled residuals.
  vector<Type> epsilonTilde = y_vec_tilde - x_mat_tilde * beta;

  // Calculate negative log-likelihood.
  Type neg_log_lik = epsilonTilde.size() / 2.0 * log(2.0 * M_PI) +
    sum_log_det + 0.5 * (epsilonTilde * epsilonTilde).sum();

  // Report quantities to R.
  REPORT(beta);
  REPORT(XtWX);
  REPORT(XtWXinv);
  matrix<Type> cov_report = corr->covariance_matrix;
  REPORT(cov_report);

  // Ensure corr object deleted.
  delete corr;

  return neg_log_lik;
}
