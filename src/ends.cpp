#include <Rcpp.h>
using namespace Rcpp;

namespace impl {

template <int RTYPE>
Vector<RTYPE> ends(const Vector<RTYPE>& x, int n)
{
    n = std::min((R_xlen_t)n, x.size() / 2);
    Vector<RTYPE> res(2 * n);

    std::copy(x.begin(), x.begin() + n, res.begin());
    std::copy(x.end() - n, x.end(), res.begin() + n);

    return res;
}

} // impl
//' @export
// [[Rcpp::export]]
SEXP ends(SEXP x, int n = 6) {
    switch (TYPEOF(x)) {
        case INTSXP: {
            return impl::ends(as<IntegerVector>(x), n);
        }
        case REALSXP: {
            return impl::ends(as<NumericVector>(x), n);
        }
        case STRSXP: {
            return impl::ends(as<CharacterVector>(x), n);
        }
        case LGLSXP: {
            return impl::ends(as<LogicalVector>(x), n);
        }
        case CPLXSXP: {
            return impl::ends(as<ComplexVector>(x), n);
        }
        default: {
            warning(
                "Invalid SEXPTYPE %d (%s).\n",
                TYPEOF(x), type2name(x)
            );
            return R_NilValue;
        }
    }
}