#ifndef COLUMN_H
#define COLUMN_H


//#include <R.h>
//#include <Rinternals.h>
#include <vector>
#include <bitset>

// class to store column and column info/operations range, etc...

class Column {
  //protected:
  public:
    int size;
    virtual double random_split(std::vector<int> in_bag) = 0;
    virtual std::vector<int> compare(std::vector<int> in_bag, double v) = 0;
    virtual void print(size_t i) = 0;
};

/* Numeric Column */
class NumericColumn : public Column {
  double * data;
  public:
    NumericColumn(double * data, int size);
    virtual void print(size_t i) { Rprintf("%f", data[i]); }
    virtual double random_split(std::vector<int> in_bag);
    virtual std::vector<int> compare(std::vector<int> in_bag, double v);
};

NumericColumn::NumericColumn(double * data, int size) {
  this->data = data;
  this->size = size;
}

double NumericColumn::random_split(std::vector<int> in_bag) {
  double min = R_PosInf;
  double max = R_NegInf;
  for(std::vector<int>::iterator it = in_bag.begin(); it != in_bag.end(); ++it) {
    if (data[*it] < min) min = data[*it];
    if (data[*it] > max) max = data[*it];
  }
  return unif_rand() * (max - min) + min;
}

std::vector<int> NumericColumn::compare(std::vector<int> in_bag, double v) {
  std::vector<int> res;
  for(std::vector<int>::iterator it = in_bag.begin(); it != in_bag.end(); ++it) {
    if (data[*it] < v) res.push_back(*it);
  }
  return(res);
}

/* Factor Column */
class FactorColumn : public Column {
  int * data;
  std::set<int> create_set_from_double(double v);
public:
  FactorColumn(int * data, int size) ;
  virtual void print(size_t i) { Rprintf("%f", data[i]); }
  virtual double random_split(std::vector<int> in_bag);
  virtual std::vector<int> compare(std::vector<int> in_bag, double v);
};

FactorColumn::FactorColumn(int * data, int size) {
  this->data = data;
  this->size = size;
}

double FactorColumn::random_split(std::vector<int> in_bag) {
  std::set<int> uniq;

  for(std::vector<int>::iterator it = in_bag.begin(); it != in_bag.end(); ++it) {
    uniq.insert(data[*it]);
  }

  int s = unif_rand() * pow(uniq.size(), 2); // randomly sample from unique values

  std::bitset<32> sampled_levels(s); // to ones and zeros depending on sampled stats
  std::bitset<32> out(0); // initialized to zero
  std::vector<int> res(uniq.begin(), uniq.end()); // copy set to vector

  for(int i = 0; i < sampled_levels.size(); i++) {
    if (sampled_levels[i] == 1) {
      out[res[i] - 1] = 1; // R factors are 1-indexed
    }
  }
  return(out.to_ullong()); // convert back to double
}

std::set<int> FactorColumn::create_set_from_double(double val) {
  std::set<int> out;
  std::bitset<32> bitset(val);

  for (int i = 0; i < bitset.size(); i++) {
    if (bitset[i] == 1) {
      out.insert(i + 1); // add one because R Factors are 1-indexed
    }
  }
  return(out);
}

std::vector<int> FactorColumn::compare(std::vector<int> in_bag, double v) {
  std::set<int> set = create_set_from_double(v);
  std::vector<int> res;
  for(std::vector<int>::iterator it = in_bag.begin(); it != in_bag.end(); ++it) {
    if (set.count(data[*it]) > 0) res.push_back(*it);
  }
  return(res);
}

/* Integer Column */

class IntegerColumn : public Column {
  int * data;
public:
  IntegerColumn(int * data, int size) ;
  virtual void print(size_t i) { Rprintf("%f", data[i]); }
  virtual double random_split(std::vector<int> in_bag);
  virtual std::vector<int> compare(std::vector<int> in_bag, double v);
};

IntegerColumn::IntegerColumn(int * data, int size) {
  this->data = data;
  this->size = size;
}

double IntegerColumn::random_split(std::vector<int> in_bag) {
  double min = R_PosInf;
  double max = R_NegInf;
  for(std::vector<int>::iterator it = in_bag.begin(); it != in_bag.end(); ++it) {
    if (data[*it] < min) min = data[*it];
    if (data[*it] > max) max = data[*it];
  }
  return unif_rand() * (max - min) + min;
}

std::vector<int> IntegerColumn::compare(std::vector<int> in_bag, double v) {
  std::vector<int> res;
  for(std::vector<int>::iterator it = in_bag.begin(); it != in_bag.end(); ++it) {
    if (data[*it] < v) res.push_back(*it);
  }
  return(res);
}

#endif
