#include <Rcpp.h>
#include <vector>
#include <set>
#include <R.h>
using namespace Rcpp;

int count_if(LogicalVector x) {
  int counter = 0;
  for(int i = 0; i < x.size(); i++) {
    if(x[i] == TRUE) {
      counter++;
    }
  }
  return counter;
}


// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//


enum NodeType {NUMERIC=1, DISCRETE=2};

// node class
class Node {

  public:
    Node() {left = NULL; right = NULL;};
    Node(bool is_leaf_, int size_, int split_attribute, NodeType node_type, double split_value_);
    Node* left;
    Node* right;
    void set_leaf();

  protected:
    bool is_leaf;
    int size;
    int split_attribute;
    NodeType node_type; //enum: NUMERIC or DISCRETE
    double split_value;

};

void Node::set_leaf()  {
  is_leaf = true;
}

Node::Node(bool is_leaf_, int size_, int split_attribute_, NodeType node_type_, double split_value_) {
  left = NULL;
  right = NULL;

  is_leaf = is_leaf_;
  size = size_;
  split_attribute = split_attribute_;
  node_type = node_type_; //enum: NUMERIC or DISCRETE
  split_value = split_value_;
}


class Tree {

  public:
    //Tree();
    void DestroyRecursive(Node* node);
    void induce(Node* node, NumericMatrix* data, IntegerVector column_types, LogicalVector f, int current_depth, int max_depth);
    ~Tree();
    Node* root;


};


void Tree::DestroyRecursive(Node* node) {
  if (node) {
    DestroyRecursive(node->left);
    DestroyRecursive(node->right);
    delete node;
  }
}

Tree::~Tree() {
  DestroyRecursive(root);
}

LogicalVector check_for_dups(NumericMatrix* m, LogicalVector f) {

  Rprintf("Checking for dups\n");

  LogicalVector dups(m->ncol());

  for (int i = 0; i < m->ncol(); i++) {
    Rprintf("%d ", i);
    NumericVector tmp = (*m)(_,i); // extract the column
    tmp = tmp[f]; // subset it
    dups[i] = is_true(all(duplicated(tmp)));
  }
  Rprintf("\nDone checking for dups\n");

  return dups;

}


std::pair<double, double> get_range(NumericVector x) {

  double min_ = DBL_MAX;
  double max_ = DBL_MIN;

  for (int i = 0; i < x.size() - 1; ++i) {
    if (x.at(i) < min_) min_ = x.at(i);
    if (x.at(i) > max_) max_ = x.at(i);
  }

  return std::pair<double, double>(min_, max_);

}

void Tree::induce(Node* node, NumericMatrix* data, IntegerVector column_types, LogicalVector f, int current_depth, int max_depth) {

  Rprintf("Depth: %d\n", current_depth);

  LogicalVector dups = check_for_dups(data, f);

  // evaluate base case
  if (current_depth > max_depth | sum(f) <= 1 | is_true(all(dups))) {
    Rprintf("Should not be in here at all...\n");
    Rprintf("Current Depth: %d\n", current_depth);
    Rprintf("Max Depth: %d\n", max_depth);
    Rprintf("Sum(f): %d\n", (int) sum(f));
    Rprintf("Dup check: %d\n", is_true(all(dups)));

    Rprintf("Setting leaf to true\n");
    node->set_leaf();
    Rprintf("Returning\n");
    return;
  }
  Rprintf("Passed the base case check\n");

  // Select random variable from non-dups
  std::vector<int> valid_cols;

  // check attribute type
  Rprintf("Filling valid columns vector\n");
  for (int i = 0; i < dups.size(); ++i) {
    if (!dups[i]) {
      valid_cols.push_back(i);
    }
  }

  Rprintf("Randomly selecting a valid column\n");
  int sample_index = floor(R::runif(0, valid_cols.size() - 1));
  Rprintf("Sample Index: %d\n", sample_index);

  int selected_col = valid_cols.at(sample_index);

  NodeType selected_type = static_cast<NodeType>(column_types[selected_col]);

  Rprintf("Selected column %d\n", selected_col);

  // create left/right nodes and add them

  LogicalVector predicate;
  double split_value = 0;

  if (column_types[selected_col] == NUMERIC) {

    NumericVector col = (*data)(_, selected_col);
    //col = ;

    // sample randomly between min and max
    std::pair<double, double> rng = get_range(col[f]);
    double split_value = R::runif(rng.first, rng.second);

    predicate = col <= split_value;

  } else {

    Rcpp::stop("Discrete types not Supported\n");

  }

  // combine predict
  node->left = new Node(false, sum(predicate & f), selected_col, selected_type, split_value);
  node->right = new Node(false, sum(!predicate & f), selected_col, selected_type, split_value);

  // Recurse
  induce(node->left, data, column_types, predicate & f, current_depth + 1, max_depth);
  induce(node->right, data, column_types, predicate & !f, current_depth + 1, max_depth);

}




class IsolationForest {

public:
  IsolationForest(NumericMatrix data_, IntegerVector column_types_, IntegerVector nt_, IntegerVector phi_) {
    data = data_;
    column_types = column_types_;
    nt = nt_[0];
    phi = phi_[0];

    init();
  }

  void init();

  ~IsolationForest();

  std::vector<Tree>* forest;

private:
  NumericMatrix data;
  IntegerVector column_types;
  int nt;
  int phi;

};

void IsolationForest::init() {
  forest = new std::vector<Tree>(nt);
}


// recursively build the tree

// [[Rcpp::export]]
Rcpp::NumericVector isofor(NumericMatrix data, IntegerVector column_types, IntegerVector nt, IntegerVector phi ) {

  Rprintf("Entering cpp\n");

  int max_depth = log2(phi[0]);
  Rprintf("Max Depth: %d\n", max_depth);

  Rprintf("Creating IsolationForest\n");
  IsolationForest* mod = new IsolationForest(data, column_types, nt, phi);

  Rprintf("Initializing it\n");
  mod->init();

  // loop over trees and induce them
  LogicalVector f(data.nrow(), true);
  Rprintf("Num recs: %d\n", f.size());

  Rprintf("Iterating over mod->forest\n");
  for (std::vector<Tree>::iterator it = mod->forest->begin(); it != mod->forest->end(); ++it) {
    it->induce( it->root, &data, column_types, f, 1, max_depth);
  }


  return Rcpp::wrap(1.0);
}





