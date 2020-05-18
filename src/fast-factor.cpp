#include <Rcpp.h>
using namespace Rcpp;

/* Copied from article:
 * https://gallery.rcpp.org/articles/fast-factor-generation/
 */

template <int RTYPE>
  IntegerVector fast_factor_template( const Vector<RTYPE>& x ) {
    Vector<RTYPE> levs = sort_unique(x);
    IntegerVector out = match(x, levs);
    out.attr("levels") = as<CharacterVector>(levs);
    out.attr("class") = "factor";
    return out;
  }

// [[Rcpp::export]]
SEXP fast_factor( SEXP x ) {
  switch( TYPEOF(x) ) {
    case INTSXP: return fast_factor_template<INTSXP>(x);
    case REALSXP: return fast_factor_template<REALSXP>(x);
    case STRSXP: return fast_factor_template<STRSXP>(x);
  }
  return R_NilValue;
}
