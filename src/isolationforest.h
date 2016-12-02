#ifndef IFOREST_H
#define IFOREST_H

#include "dataframe.h"
#include <vector>
#include <random> // rand

class iForest {

  //std::vector<double**> forest;
  DataFrame* df;
  std::vector<int> in_bag;
  //std::default_random_engine generator;
  //std::uniform_int_distribution<int> distribution;

  public:
    int max_depth; // max depth
    int num_trees; // number of trees
    int num_samples; // number of sample used to calculate max depth
    int max_nodes;

    // constructor
    iForest(int num_trees, int num_samples, DataFrame* df);
    ~iForest();

    //Column* random_column(size_t i)  { return df[i];}

    //void subsample();

  // grow multiple trees in succession
  //void grow_forest(void);

  //void recurse(int e, int node_id, double* m);

  //double * grow_tree(void); // grow a single tree

  //void clear_boolean_mask();

};

// constructor
iForest::iForest(int num_trees, int num_samples, DataFrame* df) {
  Rprintf("Making isolation forest object\n");
  this->num_trees = num_trees;
  this->num_samples = num_samples;
  this->df = df;
  //distribution = std::uniform_int_distribution<int>(0, df.num_cols - 1);
}

iForest::~iForest() {
  delete df;
}


#endif
