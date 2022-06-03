// Author: Ben Bolker

#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() ()
{

  // vanilla TMB code for a linear mixed model with a single, vector-valued random effect
  DATA_MATRIX(X);          // fixed-effect model matrix
  DATA_SPARSE_MATRIX(Z);   // random-effect model matrix - because we get the dgCMatrix
  DATA_VECTOR(yobs);       // response
  DATA_INTEGER(n_re);      // number of RE/dim of RE covariance matrix

  PARAMETER_VECTOR(beta);  // FE parameters
  PARAMETER_VECTOR(b);     // BLUPs
  PARAMETER_VECTOR(theta); // RE parameters (log-sd followed by Cholesky factor elements)
  PARAMETER(logsd);        // residual sd

  Type jnll=0; // initialize joint neg log likelihood value

  // unpack RE parameter vector
  vector<Type> re_logsd = theta.head(n_re);
  // scaled Cholesky factor: see
  // (1) https://kaskr.github.io/adcomp/classUNSTRUCTURED__CORR__t.html for details
  vector<Type> corr_transf = theta.tail(theta.size() - n_re);
  // convert corr parameter vector into corr matrix
  density::UNSTRUCTURED_CORR_t<Type> nldens(corr_transf);
  // construct density function for MVN likelihood [scale corr density by sd vector]
  density::VECSCALE_t<density::UNSTRUCTURED_CORR_t<Type> > scnldens = density::VECSCALE(nldens, exp(re_logsd));

  // pack RE vector into a matrix; this isn't strictly necessary (we could step through
  //  the RE vector n_re elements at a time and pass each sub-vector to scnldens()),
  // but I'm copying the way glmmTMB does it (glmmTMB has an outer loop that iterates
  // through segments of the b and theta vectors corresponding to each separate random
  // effects term in the model)
  int ngrp = Z.cols()/n_re;  // number of groups (subjects)
  vector<int> dim(2);        // dimensions
  dim << n_re, ngrp;         // number of random effects, number of groups
  array<Type> bseg( &b(0), dim);  // bseg is a TMB array with these dimensions and reads the data from b
  // see the constructor in https://kaskr.github.io/adcomp/array_8hpp_source.html

  // add conditional density -log(L(b|theta))
  // blockwise, for each RE vector
  // n.b. scnldens is already a NEGATIVE log-likelihood density (so use +=)
  for (int i = 0; i<ngrp; i++) {
    jnll += scnldens(bseg.col(i));
  }

  // compute conditional mean values
  vector<Type> mu = Z * b + X * beta;

  // subtract residual log-likelihood (-log(L(y|mu, b)))
  jnll -= sum(dnorm(yobs, mu, exp(logsd), true));

  return jnll;

}
