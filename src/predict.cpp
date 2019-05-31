#include <Rcpp.h>
#include <Rinternals.h>
#include <stdlib.h>
#include <bitset>
#include <math.h>
using namespace Rcpp;

enum Tree { TerminalID, Type, Size, Left, Right, SplitAtt, SplitValue, AttType };
enum vType { Numeric = 1, Factor = 2};
enum ForestSlots { FOREST=0, PHI=1, NTREES=3, NTERM=5};


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
IntegerVector nodes_cpp(DataFrame x, NumericMatrix Tree, double e, int ni) {

  if (Tree(ni, Type) == -1) {
    IntegerVector res(x.nrows(), Tree(ni, TerminalID));
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
IntegerMatrix predict_iForest_nodes_cpp(DataFrame x, List Model) {

  // extract pieces from list
  int N = Rcpp::as<int>(Model[NTREES]);
  List forest = Rcpp::as<List>(Model[FOREST]);

  // loop over forest matrices and calculate the path length
  IntegerMatrix nodes(x.nrows(), N);

  for (int i = 0; i < N; i++) {
    Rcpp::checkUserInterrupt();
    nodes(_, i) = nodes_cpp(x, Rcpp::as<NumericMatrix>(forest[i]), 0, 0);
  }

  return nodes;
}

// [[Rcpp::export]]
SEXP predict_iForest_sparse_nodes(DataFrame x, List Model) {

  // extract pieces from list
  int num_trees = Rcpp::as<int>(Model[NTREES]);
  IntegerVector nterm = Rcpp::as<IntegerVector>(Model[NTERM]);
  List forest = Rcpp::as<List>(Model[FOREST]);

  int n_entries = x.nrows() * num_trees;

  // loop over forest matrices and calculate the path length
  IntegerVector s4_i = IntegerVector(n_entries);
  IntegerVector s4_j = IntegerVector(n_entries);

  // create matrix position offset based on # terminal nodes per tree
  IntegerVector offset = IntegerVector(nterm.size(), 0);
  for (int i = 0; i < offset.size(); i++) {
    if (i == 0) continue;
    offset[i] = offset[i-1] + nterm[i-1];
  }

  IntegerVector nodes(x.nrows()); // temp storagage for terminal node vector
  for (int j = 0; j < num_trees; j++) {
    nodes = nodes_cpp(x, Rcpp::as<NumericMatrix>(forest[j]), 0, 0);

    // loop over nodes and update slots
    for (int i = 0; i < nodes.size(); i++) {
      s4_i[j * x.nrows() + i] = i ;
      s4_j[j * x.nrows() + i] = offset[j] + nodes[i] - 1;
    }
  }

  S4 sparse_nodes("dgTMatrix");
  sparse_nodes.slot("i")   = s4_i;
  sparse_nodes.slot("j")   = s4_j;
  sparse_nodes.slot("Dim") = IntegerVector::create(x.nrows(), sum(nterm));
  sparse_nodes.slot("x")   = NumericVector(n_entries, 1.0);

  return(sparse_nodes);
}


