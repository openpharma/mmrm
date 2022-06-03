// Author: Daniel Sabanes based on Ben Bolker example

// Idea: Prototype an MMRM with TMB.
//
// Definition:
// Y_i = X_i * beta + epsilon_i, i = 1, ..., n_subjects
// where Y_i = (Y_i1, ..., Y_im) are the observations of subject i
// over the m timepoints, and for the epsilon_i's:
// epsilon_i ~iid N(0, Sigma) where Sigma is a m x m covariance matrix.
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

}
