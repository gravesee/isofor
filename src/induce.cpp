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

//using namespace Rcpp;
using namespace std;

enum Tree { Type, Size, Left, Right, SplitAtt, SplitValue, AttType };
enum vType { R_NUMERIC = 1, R_FACTOR = 2, R_INTEGER = 3};
//enum ForestSlots { FOREST=0, PHI=1, NTREES=3};

// class to store column and column info/operations range, etc...
class Column {
public:
  SEXP data;
  int size;
  int type;
  Column(SEXP data);
  double random_split(vector<int> in_bag);
  bool compare(int index, double value);
  bool compare(int index, std::set<int> value);
};

Column::Column(SEXP data) {
  this->data = data;
  size = LENGTH(data);
  if (Rf_isFactor(data)) {
    type = R_FACTOR;
  } else if (Rf_isReal(data)) {
    type = R_NUMERIC;
  } else if (Rf_isInteger(data)) {
    type = R_INTEGER;
  }
}

bool Column::compare(int index, double value) {
  if (type == R_NUMERIC) {
    return(REAL(data)[index] < value);
  } else {
    return(INTEGER(data)[index] < value);
  }
}

bool Column::compare(int index, std::set<int> value) {
  return(value.count(INTEGER(data)[index]));
}

class DataFrame {
  std::vector<Column> Columns;

public:
  std::vector<int> indices;
  int num_rows, num_cols;

  DataFrame() {indices = vector<int>(0); num_rows = 0; num_cols=0;}
  DataFrame(SEXP data);
  void print_var_types();
  void fill_indices();
  Column get_column(int i);


};

Column DataFrame::get_column(int i) {
  return Columns[i];
}

double random_split_numeric(Column c, vector<int> in_bag) {
  double min = R_PosInf;
  double max = R_NegInf;
  double * d = REAL(c.data);
  for(std::vector<int>::iterator it = in_bag.begin(); it != in_bag.end(); ++it) {
    if (d[*it] < min) min = d[*it];
    if (d[*it] > max) max = d[*it];
  }
  return unif_rand() * (max - min) + min;
}

double random_split_integer(Column c, vector<int> in_bag) {
  double min = R_PosInf;
  double max = R_NegInf;
  int * d = INTEGER(c.data);
  for(std::vector<int>::iterator it = in_bag.begin(); it != in_bag.end(); ++it) {
    if (d[*it] < min) min = d[*it];
    if (d[*it] > max) max = d[*it];
  }
  return unif_rand() * (max - min) + min;
}

double random_split_factor(Column c, vector<int> in_bag) {
  int * d = INTEGER(c.data);

  std::set<int> uniq;

  // load the vector
  for(std::vector<int>::iterator it = in_bag.begin(); it != in_bag.end(); ++it) {
    uniq.insert(d[*it]);
  }

  // randomly sample from range of unique values
  int s = unif_rand() * pow(uniq.size(), 2);

  std::bitset<32> sampled_levels(s); // to ones and zeros depending on sampled stats
  std::bitset<32> out(0); // initialized to zero

  // copy set to vector
  std::vector<int> res(uniq.begin(), uniq.end());

  for(int i = 0; i < sampled_levels.size(); i++) {
    if (sampled_levels[i] == 1) {
      out[res[i] - 1] = 1; // R factors are 1-indexed
    }
  }

  // convert back to double
  return(out.to_ullong());
}

// function to take a double value and return the set
std::set<int> create_set_from_double(double val) {
  std::set<int> out;
  std::bitset<32> bitset(val);

  for (int i = 0; i < bitset.size(); i++) {
    if (bitset[i] == 1) {
      out.insert(i + 1); // add one because R Factors are 1-indexed
    }
  }
  return(out);
}


double Column::random_split(vector<int> in_bag) {
  if (type == R_NUMERIC) {
    return(random_split_numeric(*this, in_bag));
  } else if (type == R_INTEGER) {
    return(random_split_integer(*this, in_bag));
  } else if (type == R_FACTOR) {
    return(random_split_factor(*this, in_bag));
  } else {
    return(-1.0);
  }
}

DataFrame::DataFrame(SEXP data) {
  num_cols = LENGTH(data);
  num_rows = LENGTH(VECTOR_ELT(data, 0)); // length of first data.frame column
  indices  = vector<int>(num_rows);
  std::iota(indices.begin(), indices.end(), 0);

  for (int i = 0; i < num_cols; i++) {
    Columns.push_back(Column(VECTOR_ELT(data, i)));
  }
}

void DataFrame::print_var_types() {
  for(std::vector<Column>::iterator it = Columns.begin(); it != Columns.end(); ++it) {
    Rprintf("%d ", (*it).type);
  }
  Rprintf("\n");
}


class iForest {

  //std::vector<double**> forest;

  DataFrame df;
  // pointer to the data
  //bool * mask; // reused boolean mask for growing the tree

  public:
    vector<int> in_bag;
    int max_depth; // max depth
    int num_trees; // number of trees
    int num_samples; // number of sample used to calculate max depth
    int max_nodes;
    bool * mask;

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
  in_bag = vector<int>(num_samples);
}

iForest::~iForest() {
  free(mask);
}

void iForest::clear_boolean_mask() {
  mask = (bool*) calloc(num_samples * max_nodes, sizeof(bool));

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



// THis is the function that passes stuff from R to C and back
RcppExport SEXP test_func (SEXP data) {

  iForest mod = iForest(100, 16, data);

  // create new mask
  mod.clear_boolean_mask();

  // randomly sample data points
  mod.subsample();


  // get a random column
  int i = unif_rand() * mod.num_cols();
  Column col = mod.get_column(i);

  // get a random split
  double split = col.random_split(mod.in_bag);

  // create the boolean mask using the column and the split value
  int l_node_id = 1;
  int r_node_id = 2;

  if (col.type == R_FACTOR) {
    std::set<int> comp = create_set_from_double(split);

    for (int i = 0; i < mod.num_samples; i++) {
      bool test = col.compare(i, comp);
      mod.mask[mod.num_samples * l_node_id + i] &= test;
      mod.mask[mod.num_samples * r_node_id + i] &= !test;
    }
  } else {
    for (int i = 0; i < mod.num_samples; i++) {
      bool test = col.compare(i, split);
      mod.mask[mod.num_samples * l_node_id + i] &= test;
      mod.mask[mod.num_samples * r_node_id + i] &= !test;
    }
  }

  Rprintf("Random Split Value: %f\n", split);

  if (col.type == R_FACTOR) {
    std::set<int> tmp = create_set_from_double(split);
    for (auto const& val : tmp) {
      Rprintf("Selected Factor Levels: %d\n", val);
    }
  }

  return (R_NilValue);
}
