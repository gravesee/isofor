// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// nodes_cpp
IntegerVector nodes_cpp(DataFrame x, NumericMatrix Tree, double e, int ni);
RcppExport SEXP _isofor_nodes_cpp(SEXP xSEXP, SEXP TreeSEXP, SEXP eSEXP, SEXP niSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< DataFrame >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type Tree(TreeSEXP);
    Rcpp::traits::input_parameter< double >::type e(eSEXP);
    Rcpp::traits::input_parameter< int >::type ni(niSEXP);
    rcpp_result_gen = Rcpp::wrap(nodes_cpp(x, Tree, e, ni));
    return rcpp_result_gen;
END_RCPP
}
// predict_iForest_nodes_cpp
IntegerMatrix predict_iForest_nodes_cpp(DataFrame x, List Model);
RcppExport SEXP _isofor_predict_iForest_nodes_cpp(SEXP xSEXP, SEXP ModelSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< DataFrame >::type x(xSEXP);
    Rcpp::traits::input_parameter< List >::type Model(ModelSEXP);
    rcpp_result_gen = Rcpp::wrap(predict_iForest_nodes_cpp(x, Model));
    return rcpp_result_gen;
END_RCPP
}
// predict_iForest_sparse_nodes
SEXP predict_iForest_sparse_nodes(DataFrame x, List Model);
RcppExport SEXP _isofor_predict_iForest_sparse_nodes(SEXP xSEXP, SEXP ModelSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< DataFrame >::type x(xSEXP);
    Rcpp::traits::input_parameter< List >::type Model(ModelSEXP);
    rcpp_result_gen = Rcpp::wrap(predict_iForest_sparse_nodes(x, Model));
    return rcpp_result_gen;
END_RCPP
}
// predict_iForest_pathlength_cpp
SEXP predict_iForest_pathlength_cpp(SEXP df, List Model, SEXP n_cores);
RcppExport SEXP _isofor_predict_iForest_pathlength_cpp(SEXP dfSEXP, SEXP ModelSEXP, SEXP n_coresSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type df(dfSEXP);
    Rcpp::traits::input_parameter< List >::type Model(ModelSEXP);
    Rcpp::traits::input_parameter< SEXP >::type n_cores(n_coresSEXP);
    rcpp_result_gen = Rcpp::wrap(predict_iForest_pathlength_cpp(df, Model, n_cores));
    return rcpp_result_gen;
END_RCPP
}
<<<<<<< HEAD
=======
// top_k_distances
SEXP top_k_distances(SEXP pos, SEXP li, SEXP n, SEXP k);
RcppExport SEXP _isofor_top_k_distances(SEXP posSEXP, SEXP liSEXP, SEXP nSEXP, SEXP kSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type pos(posSEXP);
    Rcpp::traits::input_parameter< SEXP >::type li(liSEXP);
    Rcpp::traits::input_parameter< SEXP >::type n(nSEXP);
    Rcpp::traits::input_parameter< SEXP >::type k(kSEXP);
    rcpp_result_gen = Rcpp::wrap(top_k_distances(pos, li, n, k));
    return rcpp_result_gen;
END_RCPP
}
>>>>>>> 05649892b210a31e5578dba0d9d3352d82066858

static const R_CallMethodDef CallEntries[] = {
    {"_isofor_nodes_cpp", (DL_FUNC) &_isofor_nodes_cpp, 4},
    {"_isofor_predict_iForest_nodes_cpp", (DL_FUNC) &_isofor_predict_iForest_nodes_cpp, 2},
    {"_isofor_predict_iForest_sparse_nodes", (DL_FUNC) &_isofor_predict_iForest_sparse_nodes, 2},
    {"_isofor_predict_iForest_pathlength_cpp", (DL_FUNC) &_isofor_predict_iForest_pathlength_cpp, 3},
<<<<<<< HEAD
=======
    {"_isofor_top_k_distances", (DL_FUNC) &_isofor_top_k_distances, 4},
>>>>>>> 05649892b210a31e5578dba0d9d3352d82066858
    {NULL, NULL, 0}
};

RcppExport void R_init_isofor(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
