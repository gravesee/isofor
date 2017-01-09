#include <Rinternals.h>
#include <R.h>

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// vector of node memberships in posiiions
// list of vectors of indices belonging to each position


// do this in C and make it a lot faster

// make the positions a list as well

// do the global sort indices trick from binnr
static int* global_array = NULL;

// compare function for sorting index
int compare(const void* a, const void* b) {
  int aa = *(int*)a;
  int bb = *(int*)b;

  return
    (global_array[aa] < global_array[bb]) ?  1
   :(global_array[aa] > global_array[bb]) ? -1
   : 0;
}

// [[Rcpp::export]]
SEXP top_k_distances(SEXP pos, SEXP li, SEXP n, SEXP k) {

  int N = INTEGER(n)[0];
  int K = INTEGER(k)[0];

  // fill the indices with sequences #s
  int * indices = malloc(sizeof(int) * N);
  for (int i = 0; i < N; i ++) {
    indices[i] = i;
  }

  int num_nodes = LENGTH(pos);
  //Rprintf("Numer of nodes: %d\n", K);

  int * dists = (int*) calloc(N, sizeof(int));
  //memset(INTEGER(result), 0, N * sizeof(int));

  int * cur;
  //int * res = INTEGER(result);

  for (int i = 0; i < num_nodes; i++) {

    int idx = INTEGER(pos)[i] - 1;

    cur = INTEGER(VECTOR_ELT(li, idx));
    int M = LENGTH(VECTOR_ELT(li, idx));

    for (int j = 0; j < M; j++) {
      dists[cur[j] - 1]++;
    }
  }

  // fix this when you get back. needs the number of items.
  global_array = dists;
  qsort(indices, N, sizeof(int), compare);


  SEXP out = PROTECT(allocVector(INTSXP, K));

  for (int i = 0; i < K; i++){
    INTEGER(out)[i] = indices[i] + 1;
  }

  free(indices);
  free(dists);
  UNPROTECT(1);

  return out;
}

// [[Rcpp::export]]
// IntegerVector top_k_distances(NumericVector positions, List li, IntegerVector n, IntegerVector k) {
//
//   // store distances of each index in this vector which will be incremented
//   IntegerVector res(n[0]);
//   //IntegerVector out(k[0]);
//
//   // each element of the list contains a numeric vector indices
//
//   for (int i = 0; i < positions.size(); i++) {
//
//     NumericVector ids = as<NumericVector>(li[positions[i] - 1]);
//
//     for (int j = 0; j < ids.size(); j++) {
//       res[ids[j] - 1]++;
//     }
//   }
//
//   // sort and return the top K
//   //res.sort(TRUE);
//   //IntegerVector sorted = clone(res).sort(TRUE);
//   //IntegerVector tmp = match(sorted, res);
//   //
//   // for (int i = 0; i < k[0]; i++) {
//   //   out[i] = tmp[i];
//   // }
//
//   return res;
// }
//
// // [[Rcpp::export]]
// NumericVector timesTwo(NumericVector x) {
//   return x * 2;
// }
//
//
// // You can include R code blocks in C++ files processed with sourceCpp
// // (useful for testing and development). The R code will be automatically
// // run after the compilation.
// //
//
// /*** R
// timesTwo(42)
// */
