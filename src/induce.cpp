#include <Rmath.h>
#include <Rcpp.h>
#include <R.h>
#include <Rinternals.h>
#include <stdlib.h>
#include <bitset>
#include <math.h>
#include <algorithm>
#include <random>
#include <vector>
#include <string.h>
#include <bitset>
#include "column.h"
#include "dataframe.h"
#include "isolationforest.h"

//using namespace Rcpp;
using namespace std;

enum Tree { Type, Size, Left, Right, SplitAtt, SplitValue, AttType };
enum vType { R_NUMERIC = 1, R_FACTOR = 2, R_INTEGER = 3};
//enum ForestSlots { FOREST=0, PHI=1, NTREES=3};

/*
class iForest {

  //std::vector<double**> forest;

  DataFrame df;
  // pointer to the data
  //bool * mask; // reused boolean mask for growing the tree

  public:
    vector<vector<int>> in_bag;
    int max_depth; // max depth
    int num_trees; // number of trees
    int num_samples; // number of sample used to calculate max depth
    int max_nodes;

    // constructor
    iForest(int num_trees, int num_samples, SEXP data);

    // Destructor
    ~iForest();

    int num_cols() { return df.num_cols; }
    int num_rows() { return df.num_rows; }
    Column get_column(int i)  { return df.get_column(i);}

    // grow multiple trees in succession
    void grow_forest(void);

    void recurse(int e, int node_id, double* m);

    double * grow_tree(void); // grow a single tree

    void clear_boolean_mask();

    void subsample();

};

// constructor
iForest::iForest(int num_trees, int num_samples, SEXP data) {
  df = DataFrame(data);
  this->num_trees = num_trees;
  this->num_samples = num_samples > df.num_rows ? df.num_rows : num_samples;
  in_bag[1] = vector<int>(num_samples);
}

iForest::~iForest() {
  free(mask);
}

void iForest::clear_boolean_mask() {
  // set first column to all true
  for (int i = 0; i < num_samples; i++) {
    mask[i] = true;
  }

}

// function that clears the in_bag vector and then randomly fills it with
// indices totally the number of requested samples

void iForest::subsample() {
  // randomly shuffle the indices and fill in_bag
  std::random_shuffle(df.indices.begin(), df.indices.end());

  for (int i = 0; i < num_samples; i++) {
    in_bag[i] = df.indices[i];
  }
}

// create boolean masks, intial one is all true, rest are false

// t

*/
// THis is the function that passes stuff from R to C and back
RcppExport SEXP test_func (SEXP data) {

  DataFrame* df = new DataFrame(data);

  iForest mod = iForest(10, 256, df);
  // loop to produce trees

  Rprintf("First Value of first column");
  //(*df[0]).print(0);



  /*
  iForest mod = iForest(100, 16, data);

  // create new mask
  mod.clear_boolean_mask();

  // randomly sample data points
  mod.subsample();


  // get a random column
  int i = unif_rand() * mod.num_cols();
  Column col = mod.get_column(i);


  // get a random split
  // double split = col.random_split(mod.in_bag);

  // create the boolean mask using the column and the split value

  //col.compare(split, mod.mask, 0, mod.num_samples);

  Rprintf("Random Split Value: %f\n", split);

  if (col.type == R_FACTOR) {
    std::set<int> tmp = create_set_from_double(split);
    for (auto const& val : tmp) {
      Rprintf("Selected Factor Levels: %d\n", val);
    }
  }
  */

  return (R_NilValue);
}
