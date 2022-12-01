#ifndef TMB_INCLUDES_H
#define TMB_INCLUDES_H
#ifdef INCLUDE_RCPP
/* Rcpp defines `R_NO_REMAP` which causes errors when using TMB. Thus, we
* define a few macros. */
#ifndef findVar
#define findVar	Rf_findVar
#endif
#ifndef install
#define install	Rf_install
#endif
#ifndef error
#define error	Rf_error
#endif
/* TMB redefines a few macro. Thus, we undefine them first */
#ifdef R_D_val
#undef R_D_val
#endif
#ifdef R_D_qIv
#undef R_D_qIv
#endif
#ifdef R_D_exp
#undef R_D_exp
#endif
#ifdef R_D_log
#undef R_D_log
#endif
#ifdef R_D_Clog
#undef R_D_Clog
#endif
#ifdef R_D_LExp
#undef R_D_LExp
#endif
#ifdef R_DT_qIv
#undef R_DT_qIv
#endif
#ifdef R_DT_CIv
#undef R_DT_CIv
#endif
#ifdef R_Q_P01_check
#undef R_Q_P01_check
#endif
#ifdef R_Q_P01_boundaries
#undef R_Q_P01_boundaries
#endif
#ifdef R_D_negInonint
#undef R_D_negInonint
#endif
#ifdef R_D_nonint_check
#undef R_D_nonint_check
#endif
#endif // ifdef INCLUDE_RCPP

/* TMB */
#define WITH_LIBTMB

#include <TMB.hpp>

#endif
