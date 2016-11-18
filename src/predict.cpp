#include <Rcpp.h>
#include <Rinternals.h>
#include <stdlib.h>
#include <bitset>
#include <math.h>
using namespace Rcpp;

enum Tree { Type, Size, Left, Right, SplitAtt, SplitValue, AttType };
enum vType { Numeric = 1, Factor = 2};
enum ForestSlots { FOREST=0, PHI=1, NTREES=3};


// This function takes the 32-bit int storing the factor pattern and converts it
// to a bitset. The positions of the 1-bits indicate which factor levels go left

IntegerVector which_eq_one(int d) {

  std::bitset<32> bs(d);
  IntegerVector res(bs.count());
  int idx = 0;
  for (size_t i = 0; i < bs.size(); i++){
    if (bs[i] == 1) {
      res[idx] = i;
      idx++;
    }
  }
  return res;
}

LogicalVector iTreeFilter_numeric (NumericVector x, int ni, NumericMatrix Tree) {
  return x < Tree(ni, SplitValue);
}

// Compares the current factor (cast as integer) to the bitset and returns a
// logical vector indicating left/right node membership

LogicalVector iTreeFilter_factor (IntegerVector x, int ni, NumericMatrix Tree) {

  Rcpp::IntegerVector f = which_eq_one(Tree(ni, SplitValue)) + 1;
  Rcpp::LogicalVector res(x.size(), 0);

  for (int j = 0; j < f.size(); j++) {
    res = res | (x == f[j]);
  }

  return res;
}

double cn(double n) {
  if (n == 2) {
    return 1;
  } else if (n < 2) {
    return 0;
  } else {
    double H = log(n - 1) + 0.5772156649;
    return 2 * H - (2*(n - 1)/n);
  }
}


// [[Rcpp::export]]
NumericVector pathLength_cpp(DataFrame x, NumericMatrix Tree, double e, int ni) {

  if (Tree(ni, Type) == -1) {
    double val = e + cn(double(Tree(ni, Size)));
    NumericVector res(x.nrows(), val);
    return(res);
  }

  int i = Tree(ni, SplitAtt) - 1;

  LogicalVector f = (int(Tree(ni, AttType)) == Factor) ?
    iTreeFilter_factor(Rcpp::as<IntegerVector>(x[i]), ni, Tree) :
    iTreeFilter_numeric(Rcpp::as<NumericVector>(x[i]), ni, Tree);

  return Rcpp::wrap(ifelse(f,
          pathLength_cpp(x, Tree, e + 1, Tree(ni, Left) - 1),
          pathLength_cpp(x, Tree, e + 1, Tree(ni, Right) - 1)));
}

// [[Rcpp::export]]
IntegerVector nodes_cpp(DataFrame x, NumericMatrix Tree, double e, int ni) {

  if (Tree(ni, Type) == -1) {
    IntegerVector res(x.nrows(), ni);
    return(res);
  }

  int i = Tree(ni, SplitAtt) - 1;

  LogicalVector f = (int(Tree(ni, AttType)) == Factor) ?
  iTreeFilter_factor(Rcpp::as<IntegerVector>(x[i]), ni, Tree) :
    iTreeFilter_numeric(Rcpp::as<NumericVector>(x[i]), ni, Tree);

  return Rcpp::wrap(ifelse(f,
    nodes_cpp(x, Tree, e + 1, Tree(ni, Left) - 1),
    nodes_cpp(x, Tree, e + 1, Tree(ni, Right) - 1)));
}

// [[Rcpp::export]]
NumericVector predict_iForest_pathLength_cpp(DataFrame x, List Model) {

  NumericVector res(x.nrows());

  // extract pieces from list
  int N = Rcpp::as<int>(Model[NTREES]);
  double phi = Rcpp::as<double>(Model[PHI]);
  List forest = Rcpp::as<List>(Model[FOREST]);

  double avg = cn(phi);

  // loop over forest matrices and calculate the path length
  NumericMatrix pls(x.nrows(), N);

  for (int i = 0; i < N; i++) {
    //Rcpp::checkUserInterrupt();
    pls(_, i) = pathLength_cpp(x, Rcpp::as<NumericMatrix>(forest[i]), 0, 0);
  }

  // Calculate the rowMeans of the matrix
  for( int i=0; i < pls.nrow(); i++ ) {
    double tmp = mean( pls(i, _) );
    res[i] = pow(2,  -1 * tmp / avg);
  }

  return res;

}

// [[Rcpp::export]]
IntegerMatrix predict_iForest_nodes_cpp(DataFrame x, List Model) {

  // extract pieces from list
  int N = Rcpp::as<int>(Model[NTREES]);
  List forest = Rcpp::as<List>(Model[FOREST]);

  // loop over forest matrices and calculate the path length
  IntegerMatrix nodes(x.nrows(), N);

  for (int i = 0; i < N; i++) {
    //Rcpp::checkUserInterrupt();
    nodes(_, i) = nodes_cpp(x, Rcpp::as<NumericMatrix>(forest[i]), 0, 0);
  }

  return nodes;
}
