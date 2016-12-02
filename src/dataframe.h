#ifndef DATAFRAME_H
#define DATAFRAME_H

#include "column.h"
#include <vector>
#include <R.h>

class DataFrame {
  std::vector<Column*> columns;

public:
  std::vector<int> indices;
  int num_cols;
  DataFrame() { columns = std::vector<Column*>(0); }
  DataFrame(SEXP data); // make this more general...
  ~DataFrame();
  Column* operator[](std::size_t i) { return columns[i]; }
};

DataFrame::DataFrame(SEXP data) {
  Rprintf("Making isolation forest object\n");
  this->num_cols = LENGTH(data);
  int num_rows = LENGTH(VECTOR_ELT(data, 0)); // length of first data.frame column

  indices = std::vector<int>(num_rows);
  std::iota(indices.begin(), indices.end(), 0);

  for (int i = 0; i < num_cols; i++) {
    SEXP col = VECTOR_ELT(data, i);
    // load colmns with correct type of column based on R type
    if (Rf_isFactor(col)) {
      columns.push_back(new FactorColumn(INTEGER(col), num_rows));
    } else if (Rf_isInteger(col)) {
      columns.push_back(new IntegerColumn(INTEGER(col), num_rows));
    } else if (Rf_isNumeric(col)) {
      columns.push_back(new NumericColumn(REAL(col), num_rows));
    }
  }
}

DataFrame::~DataFrame() {
  for (std::vector<Column*>::iterator it = columns.begin(); it != columns.end(); ++it) {
    free(*it);
  }
}

/*
Column DataFrame::get_column(int i) {
  return Columns[i];
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

*/
#endif
