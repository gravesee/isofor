#include <Rcpp.h>
#include <Rinternals.h>
using namespace Rcpp;

// TODO: add comments to code

enum Tree { Type, Size, Left, Right, SplitAtt, SplitValue, AttType };
enum vType { Numeric = 1, Factor = 2};

// [[Rcpp::export]]
IntegerVector which_eq_one(int d) {
  std::bitset<32> bs(d);
  IntegerVector res(bs.count());
  int idx = 0;
  for (int i = 0; i < bs.size(); i++){
    if (bs[i] == 1) {
      res[idx] = i;
      idx++;
    }
  }
  return res;
}

// [[Rcpp::export]]
LogicalVector iTreeFilter_numeric (NumericVector x, int ni, NumericMatrix Tree, List Forest) {
  return x < Tree(ni, SplitValue);
}

// [[Rcpp::export]]
LogicalVector iTreeFilter_factor (IntegerVector x, int ni, NumericMatrix Tree, List Forest) {
  double v = Tree(ni, SplitValue);

  Rcpp::IntegerVector f = which_eq_one(v) + 1;
  Rcpp::LogicalVector res(x.size(), FALSE);

  for (int i = 0; i < res.size(); i++) {
    for (int j = 0; j < f.size(); j++) {
      if (x[i] == f[j]) {
        res[i] = TRUE;
      }
    }
  }

  return res;
}

// [[Rcpp::export]]
NumericVector pathLength_cpp(DataFrame x, NumericMatrix Tree, List Forest, double e, int ni, int len) {

  if (Tree(ni, Type) == -1) {
    NumericVector res(len, e + Tree(ni, Size));
    return(res);
  }

  int i = Tree(ni, SplitAtt) - 1;
  int type = Tree(ni, AttType);

  LogicalVector f = (type == Factor) ?
    iTreeFilter_factor(Rcpp::as<IntegerVector>(x[i]), ni, Tree, Forest) :
    iTreeFilter_numeric(Rcpp::as<NumericVector>(x[i]), ni, Tree, Forest);

  return Rcpp::wrap(ifelse(f,
          pathLength_cpp(x, Tree, Forest, e + 1, Tree(ni, Left) - 1, len),
          pathLength_cpp(x, Tree, Forest, e + 1, Tree(ni, Right) - 1, len)));
}
