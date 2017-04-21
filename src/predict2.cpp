#include <Rcpp.h>
#include <Rinternals.h>
#include <stdlib.h>
#include <bitset>
#include <math.h>
#include "predict.h"
#include <omp.h>
using namespace Rcpp;

enum Tree { TerminalID, Type, Size, Left, Right, SplitAtt, SplitValue, AttType };
enum vType { Numeric = 1, Factor = 2};
enum ForestSlots { FOREST=0, PHI=1, NTREES=3, NTERM=5};


// macros for accessing parts of Tree matrix

#define GET_TREE_ATTR(Tree, Attr, row, nrows) REAL(Tree)[row + (nrows * Attr)]
#define GET_DF_VAL_N(df, s_attr, i) REAL(VECTOR_ELT(df, s_attr))[i]
#define GET_DF_VAL_I(df, s_attr, i) INTEGER(VECTOR_ELT(df, s_attr))[i]

// Iterative implementation of numeric attribute check
// Returns tree index for left or right column of tree matrix
int filter_numeric(double x, double val) {
  return x < val ? Left : Right;
}


int filter_factor(int x, int d) {

  // turn value into a bitset
  std::bitset<32> bs(d);
  int * res = (int *) calloc(sizeof(int *), bs.count());

    // return array of which factor levels are store in `d`
  bool matches_any = FALSE;
  for (size_t i = 0; i < bs.size(); i++){
    if (bs[i] == 1 && x == (i + 1)) {
      matches_any = TRUE;
      break;
    }
  }

  free(res);

  return matches_any ? Left : Right;
}


// out_mat is passed in from calling function. it will store the calculations
// start wrtiting into out mat at pos
void pathlength_iterative(SEXP df, SEXP Tree, double * out_mat, int offset, int * current_node, int * depth)  {

  // start with the root node and a vector of the root node position
  int df_nrows = Rf_length(VECTOR_ELT(df, 0));

  // create vector to keep track of current child node
  //int * current_node = (int *) calloc(sizeof(int *), df_nrows);
  //int * depth = (int *) calloc(sizeof(int *), df_nrows);

  for (int i = 0; i < df_nrows; i++) {
    current_node[i] = 0;
    depth[i] = 0;
  }

  // get pointer to tree matrix as well as dimensions
  SEXP dims = Rf_getAttrib(Tree, R_DimSymbol);
  int nrows = INTEGER(dims)[0];

  // loop over the current node vector and use it to index the tree
  bool all_terminal = TRUE;
  do {

    for (int i = 0; i < df_nrows; ++i) {

      int row = current_node[i]; // current tree node
      int type = GET_TREE_ATTR(Tree, Type, row, nrows); // terminal or not?

      if (type == 1) {
        all_terminal = FALSE;
        depth[i] += 1;

        // split characteristics
        int s_attr = GET_TREE_ATTR(Tree, SplitAtt, row, nrows) - 1;
        double s_value = GET_TREE_ATTR(Tree, SplitValue, row, nrows);
        int s_type = GET_TREE_ATTR(Tree, AttType, row, nrows);

        // now do the test
        int lr;
        switch(s_type) {
        case Numeric:

          // check if numeric field is integer or real
          if (TYPEOF(VECTOR_ELT(df, s_attr)) == INTSXP) {
            lr = filter_numeric(GET_DF_VAL_I(df, s_attr, i), s_value);
          } else {
            lr = filter_numeric(GET_DF_VAL_N(df, s_attr, i), s_value);
          }

          current_node[i] = GET_TREE_ATTR(Tree, lr, row, nrows) - 1;
          break;

        case Factor:

          lr = filter_factor(GET_DF_VAL_I(df, s_attr, i), (int) s_value);
          current_node[i] = GET_TREE_ATTR(Tree, lr, row, nrows) - 1;
          break;

        };

      }

    }
  }  while(!all_terminal);

  for (int i = 0; i < df_nrows; i++) {
    int row = current_node[i];
    double size = GET_TREE_ATTR(Tree, Size, row, nrows);


    out_mat[i + offset * df_nrows] = depth[i] + cn(size);
    Rprintf("(%3d, %3d, %2.2f)", i, offset, i + offset * df_nrows);
  }

}

// [[Rcpp::export]]
SEXP predict_iterative(SEXP df, List Model) {

  int df_nrows = LENGTH(VECTOR_ELT(df, 0)); // length of first column


  // extract pieces from list
  int n_trees = Rcpp::as<int>(Model[NTREES]);
  double phi = Rcpp::as<double>(Model[PHI]);
  List forest = Rcpp::as<List>(Model[FOREST]);

  double avg = cn(phi);

  //Rprintf("Allocating memory for pathlengths matrix\n");
  double * pls = (double *) calloc(sizeof(double *), df_nrows * n_trees);

  int * current_node;
  int * depth;

  #pragma omp parallel private(current_node, depth)
  {

    current_node = (int *) calloc(sizeof(int *), df_nrows);
    depth = (int *) calloc(sizeof(int *), df_nrows);

    #pragma omp for
    for (int i = 0; i < n_trees; i++) {
      Rprintf("Starting column\n");
      pathlength_iterative(df, forest[i], pls, i, current_node, depth); // forest is a matrix
    }

    free(current_node);
    free(depth);

  }

  // fill output vector with average path length
  SEXP res;
  PROTECT(res = Rf_allocVector(REALSXP, df_nrows));

  Rprintf("Filling return vector\n");
  // Calculate the rowMeans of the matrix
  for( int i = 0; i < df_nrows; i++ ) {


    double tmp = 0;
    for (int j = 0; j < n_trees; j++) {
      tmp += pls[i + df_nrows * j];
    }

    // Rprintf("Sum: %f, Avg %f, Mean: %f\n", tmp, avg, tmp/avg);
    tmp = tmp / n_trees;

    REAL(res)[i] = pow(2,  -1 * tmp / avg);
  }

  free(pls);
  UNPROTECT(1);
  return res;

}