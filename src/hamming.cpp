#include <Rcpp.h>
#include <Rinternals.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector bit_count(Rcpp::IntegerVector a, Rcpp::IntegerVector b) {

  Rcpp::IntegerVector res(a.size());

  for (int i = 0; i < a.size(); i++) {
    res[i] = __builtin_popcount(a[i] ^ b[0]);
  }

  return res;

}
