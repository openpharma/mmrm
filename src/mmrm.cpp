#include "covariance.h"

// Definition:
//
// Y_i = X_i * beta + epsilon_i, i = 1, ..., n_subjects
// where Y_i = (Y_i1, ..., Y_im) are the observations of subject i over the m
// timepoints,
//
// and for the epsilon_i's :
// epsilon_i ~iid N(0, Sigma) where Sigma is a covariance matrix
// parameterized by a vector theta.
//
// Note: This is a special generalized least squares model
// Y = X * beta + epsilon,
// where we have a block structure for the covariance matrix of the epsilon
// vector.
//
// beta itself is not a parameter for TMB here:
// - For maximum likelihood estimation:
//   Given theta and therefore Sigma, and writing W = Sigma^-1, we can determine
//   the beta optimizing the likelihood via the weighted least squares equation
//   (X^T W X) beta = X^T W Y.
// - For restricted maximum likelihood estimation:
//   Given theta, beta is integrated out from the likelihood. Weighted least
//   squares results are used to calculate integrated log likelihood.

template<class Type>
Type objective_function<Type>::operator() ()
{
  // Read data from R.
  DATA_MATRIX(x_matrix);           // Model matrix (dimension n x p).
  DATA_VECTOR(y_vector);           // Response vector (length n).
  DATA_VECTOR(weights_vector);     // Weights vector (length n).
  DATA_IVECTOR(visits_zero_inds);  // Zero-based Visits vector (length n).
  DATA_MATRIX(coordinates);        // Coordinates matrix.
  DATA_INTEGER(n_visits);          // Number of visits, which is the dimension of the covariance matrix.
  DATA_INTEGER(n_subjects);        // Number of subjects.
  DATA_IVECTOR(subject_zero_inds); // Starting indices for each subject (0-based) (length n_subjects).
  DATA_IVECTOR(subject_n_visits);  // Number of observed visits for each subject (length n_subjects).
  DATA_STRING(cov_type);           // Covariance type name.
  DATA_INTEGER(is_spatial_int);    // Spatial covariance (1)? Otherwise non-spatial covariance.
  DATA_INTEGER(reml);              // REML (1)? Otherwise ML (0).
  DATA_FACTOR(subject_groups);     // subject groups vector(0-based) (length n_subjects).
  DATA_INTEGER(n_groups);          // number of total groups.
  // Read parameters from R.
  PARAMETER_VECTOR(theta);         // Covariance parameters (length k). Contents depend on covariance type.

  // X^T W X will be calculated incrementally into here.
  matrix<Type> XtWX = matrix<Type>::Zero(x_matrix.cols(), x_matrix.cols());
  // X^T W Y will be calculated incrementally into here.
  matrix<Type> XtWY = matrix<Type>::Zero(x_matrix.cols(), 1);
  // W^T/2 X will be saved into here.
  matrix<Type> x_mat_tilde = matrix<Type>::Zero(x_matrix.rows(), x_matrix.cols());
  // W^T/2 Y will be saved into here.
  vector<Type> y_vec_tilde = vector<Type>::Zero(y_vector.rows());
  // Sum of the log determinant will be incrementally calculated here.
  Type sum_log_det = 0.0;
  // Get number of variance parameters for one group.
  int theta_one_group_size = theta.size() / n_groups;
  // Convert is_spatial_int to bool.
  bool is_spatial = (is_spatial_int == 1);
  // Create the lower triangular Cholesky factor of the visit x visit covariance matrix.
  int dim_cov_mat;
  if (is_spatial) {
    dim_cov_mat = 2;
  } else {
    dim_cov_mat = n_visits;
  }
  matrix<Type> covariance_lower_chol = get_cov_lower_chol_grouped(theta, dim_cov_mat, cov_type, n_groups, is_spatial);
  // Go through all subjects and calculate quantities initialized above.
  for (int i = 0; i < n_subjects; i++) {
    // Start index and number of visits for this subject.
    int start_i = subject_zero_inds(i);
    int n_visits_i = subject_n_visits(i);
    // Obtain Cholesky factor Li.
    matrix<Type> Li;
    if (!is_spatial) {
      matrix<Type> lower_chol = covariance_lower_chol.block(subject_groups(i) * n_visits, 0,  n_visits, n_visits);
      if (n_visits_i < n_visits) {
        // This subject has less visits, therefore we need to recalculate the Cholesky factor.
        vector<int> visits_i = visits_zero_inds.segment(start_i, n_visits_i);
        Eigen::SparseMatrix<Type> sel_mat = get_select_matrix<Type>(visits_i, n_visits);
        // Note: We cannot use triangular view for covariance_lower_chol here because sel_mat is sparse.
        matrix<Type> Ltildei = sel_mat * lower_chol;
        matrix<Type> cov_i = tcrossprod(Ltildei);
        Eigen::LLT<Eigen::Matrix<Type,Eigen::Dynamic,Eigen::Dynamic> > cov_i_chol(cov_i);
        Li = cov_i_chol.matrixL();
      } else {
        // This subject has full number of visits, therefore we can just take the original factor.
        Li = lower_chol;
      }
    } else {
      matrix<Type> distance_i = euclidean(matrix<Type>(coordinates.block(start_i, 0, n_visits_i, coordinates.cols())));
      // Obtain Cholesky factor Li.
      vector<Type> theta_i = theta.segment(subject_groups(i) * theta_one_group_size, theta_one_group_size);
      Li = get_spatial_covariance_lower_chol(theta_i, distance_i, cov_type);
    }


    // Calculate weighted Cholesky factor for this subject.
    Eigen::DiagonalMatrix<Type,Eigen::Dynamic,Eigen::Dynamic> Gi_inv_sqrt = weights_vector.segment(start_i, n_visits_i).cwiseInverse().sqrt().matrix().asDiagonal();
    Li = Gi_inv_sqrt * Li;

    // Calculate scaled design matrix and response vector for this subject.
    matrix<Type> Xi = x_matrix.block(start_i, 0, n_visits_i, x_matrix.cols());
    matrix<Type> XiTilde = Li.template triangularView<Eigen::Lower>().solve(Xi);
    matrix<Type> Yi = y_vector.segment(start_i, n_visits_i).matrix();
    matrix<Type> YiTilde = Li.template triangularView<Eigen::Lower>().solve(Yi);

    // Increment quantities.
    matrix<Type> XiTildeCrossprod = crossprod(XiTilde);
    XtWX += XiTildeCrossprod.template triangularView<Eigen::Lower>();
    XtWY += XiTilde.transpose() * YiTilde;
    vector<Type> LiDiag = Li.diagonal();
    sum_log_det += sum(log(LiDiag));

    // Save stuff.
    x_mat_tilde.block(start_i, 0, n_visits_i, x_matrix.cols()) = XiTilde;
    y_vec_tilde.segment(start_i, n_visits_i) = YiTilde.col(0);
  }

  // Solve for beta.
  Eigen::LDLT<Eigen::Matrix<Type,Eigen::Dynamic,Eigen::Dynamic> > XtWX_decomposition(XtWX);
  matrix<Type> beta_mat = XtWX_decomposition.solve(XtWY);
  vector<Type> beta = beta_mat.col(0);

  // Define scaled residuals.
  vector<Type> x_mat_tilde_beta = x_mat_tilde * beta;
  vector<Type> epsilonTilde = y_vec_tilde - x_mat_tilde_beta;

  // Calculate negative log-likelihood.
  Type neg_log_lik;

  if (reml == 1) {
    // Use restricted maximum likelihood.
    vector<Type> XtWX_D = XtWX_decomposition.vectorD();
    Type XtWX_log_det = XtWX_D.log().sum();
    neg_log_lik = (x_matrix.rows() - x_matrix.cols()) / 2.0 * log(2.0 * M_PI) +
      sum_log_det +
      XtWX_log_det / 2.0 +
      0.5 * (y_vec_tilde * y_vec_tilde).sum() - 0.5 * (x_mat_tilde_beta * x_mat_tilde_beta).sum();
  } else {
    // Use maximum likelihood.
    neg_log_lik = x_matrix.rows() / 2.0 * log(2.0 * M_PI) +
      sum_log_det +
      0.5 * (epsilonTilde * epsilonTilde).sum();
  }

  // Report quantities to R.
  REPORT(beta);

  // We already compute the inverse of XtWX here because we already did the
  // matrix decomposition above.
  matrix<Type> Identity(XtWX.rows(), XtWX.cols());
  Identity.setIdentity();
  matrix<Type> beta_vcov = XtWX_decomposition.solve(Identity);
  REPORT(beta_vcov);

  REPORT(covariance_lower_chol);

  return neg_log_lik;
}
