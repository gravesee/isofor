#include <Rcpp.h>
#include <Rinternals.h>
#include <stdlib.h>
#include <bitset>
#include <math.h>
using namespace Rcpp;
using namespace std;

//enum Tree { Type, Size, Left, Right, SplitAtt, SplitValue, AttType };
//enum vType { Numeric = 1, Factor = 2};
//enum ForestSlots { FOREST=0, PHI=1, NTREES=3};

class iForest {

  std::vector<double**> forest;

  public:
    double l; // max depth
    int nt; // number of trees
    int phi; // number of sample used to calculate max depth
    int max_nodes;

    // also need pointer to list of data

    // also need the variable types (factor and numeric only)

    // constructor
    iForest(int num_trees, int num_samples);

    // grow multiple trees in succession
    void grow_forest(void);

    // grow a single tree
    double * grow_tree(void);

};


// implement the class
// Member functions definitions including constructor
iForest::iForest(int num_trees, int num_samples) {
  nt = num_trees;
  phi = num_samples;
  l = ceil(log2(phi));
  max_nodes = (pow(l, 2) - 1) * pow(l, 2);
}



double * iForest::grow_tree() {
  // allocate memory for the output matrix

  double * m = (double *) calloc(max_nodes * 7, sizeof(double));

  return(m);

}

  //return(m);
//}

//}
//env$mat = matrix(0,
//                 nrow = max_nodes(l),
//                 ncol = 7,
//                 dimnames = list(NULL,
//                                 c("Type","Size","Left","Right","SplitAtt","SplitValue","AttType")))


