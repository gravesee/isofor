#include <Rinternals.h>
#include <R.h>

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
  int * indices = (int *) malloc(sizeof(int) * N);
  for (int i = 0; i < N; i ++) {
    indices[i] = i;
  }

  int num_nodes = LENGTH(pos);
  //Rprintf("Numer of nodes: %d\n", K);

  int * dists = (int*) calloc(N, sizeof(int));
  int * cur;

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