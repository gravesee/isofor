#include <Rcpp.h>
#include <Rinternals.h>
#include <stdlib.h>
#include <bitset>
#include <math.h>
#include "predict.h"

#ifdef _OPENMP
#include <omp.h>
#endif

using namespace Rcpp;

// Enumeration helpers corresponding to matrix columns or list indices
enum Tree { TerminalID, Type, Size, Left, Right, SplitAtt, SplitValue, AttType };
enum vType { Numeric = 1, Factor = 2};
enum ForestSlots { FOREST=0, PHI=1, NTREES=3, NTERM=5};

// These are helper macros to extract parts of Isolation Tree matrices
#define GET_TREE_ATTR(Tree, Attr, row, nrows) REAL(Tree)[row + (nrows * Attr)]
#define GET_DF_VAL_N(df, s_attr, i) REAL(VECTOR_ELT(df, s_attr))[i] // for numeric data
#define GET_DF_VAL_I(df, s_attr, i) INTEGER(VECTOR_ELT(df, s_attr))[i] // for factors and integer data

/* Test numeric observations
 * @param x a numeric (real) value from a data.frame
 * @param val the value to compare to
 * @return the index position corresponding to a left of right split
 */
int filter_numeric(double x, double val) {
  return x < val ? Left : Right;
}

/* Test Factor observations
 *
 * The compare value, d, is converted to a bitset. If any of the set bit
 * positions are equal to x, the function returns Left.
 *
 * @param x an integer value from a data.frame
 * @param d the value to compare to
 * @return the index position corresponding to a left of right split
 */
int filter_factor(int x, int d) {

  std::bitset<32> bs(d);
  int * res = (int *) calloc(bs.count(), sizeof res);

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


/* Calculate path length for data.frame of observations and a given isolation Tree
 *
 * @param df the data.frame of observations
 * @param Tree a matrix representation of the isolation tree
 * @param out pointer to array storing calculated path lengths
 * @param current_node pointer to array keeping track of current node for each observation
 * @param depth pointer to array keeping track of current observation depth
 * @param n_trees number of trees in the isolation forest
 * @return modifies `out` in-place and calculates the path lenght for each observation
 */
void predict_pathlength_cpp(SEXP df, SEXP Tree, double * out, int * current_node, int * depth, int n_trees)  {

  int df_nrows = Rf_length(VECTOR_ELT(df, 0));

  // clear temporary arrays
  memset(current_node, 0, df_nrows*sizeof(current_node));
  memset(depth, 0, df_nrows*sizeof(depth));

  int nrows = INTEGER(Rf_getAttrib(Tree, R_DimSymbol))[0]; // number of rows in the tree matrix

  // loop over the current node vector and use it to index the tree
  bool all_terminal;
  do {

    all_terminal = TRUE;

    for (int i = 0; i < df_nrows; ++i) {
      int row = current_node[i]; // current tree node

      if (GET_TREE_ATTR(Tree, Type, row, nrows) == 1) { // check if terminal
        all_terminal = FALSE;
        depth[i] += 1;

        // split characteristics
        int    s_attr  = GET_TREE_ATTR(Tree, SplitAtt, row, nrows) - 1;
        double s_value = GET_TREE_ATTR(Tree, SplitValue, row, nrows);
        int    s_type  = GET_TREE_ATTR(Tree, AttType, row, nrows);

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

  #pragma omp critical
  for (int i = 0; i < df_nrows; i++) {
    double size = GET_TREE_ATTR(Tree, Size, current_node[i], nrows);
    out[i] += (depth[i] + cn(size)) / n_trees;
  }
}



/* Predict path length for isolation forest
 *
 * @param df data.frame of observations on which to predict anomaly score
 * @param Model isolation forest model object
 * @param n_cores number of cores to use in parallel
 * @value A numeric vector with the anomaly score
 */

// [[Rcpp::export]]
SEXP predict_iForest_pathlength_cpp(SEXP df, List Model, SEXP n_cores) {

  int df_nrows = LENGTH(VECTOR_ELT(df, 0));
  int n_trees = INTEGER(Model[NTREES])[0];

  double * pl = (double *) calloc(df_nrows, sizeof pl);

  #pragma omp parallel num_threads(INTEGER(n_cores)[0])
  {

    int * current_node = (int *) calloc(df_nrows,  sizeof current_node);
    int * depth = (int *) calloc(df_nrows, sizeof depth);

    #pragma omp for
    for (int i = 0; i < n_trees; i++) {
      predict_pathlength_cpp(df, VECTOR_ELT(Model[FOREST], i), pl, current_node, depth, n_trees);
    }

    #pragma omp critical
    free(current_node);
    free(depth);
  }

  SEXP res;
  PROTECT(res = Rf_allocVector(REALSXP, df_nrows));

  double avg = cn(INTEGER(Model[PHI])[0]);

  #pragma omp parallel for
  for( int i = 0; i < df_nrows; i++ ) {
    REAL(res)[i] = pow(2,  -1 * pl[i] / avg);
  }

  free(pl);
  UNPROTECT(1);
  return res;
}
