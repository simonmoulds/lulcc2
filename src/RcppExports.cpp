// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// allowfun
IntegerVector allowfun(IntegerVector lu, IntegerVector hist, IntegerVector codes, IntegerVector changedir, IntegerVector rules);
RcppExport SEXP _lulcc2_allowfun(SEXP luSEXP, SEXP histSEXP, SEXP codesSEXP, SEXP changedirSEXP, SEXP rulesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type lu(luSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type hist(histSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type codes(codesSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type changedir(changedirSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type rules(rulesSEXP);
    rcpp_result_gen = Rcpp::wrap(allowfun(lu, hist, codes, changedir, rules));
    return rcpp_result_gen;
END_RCPP
}
// autoconvert
IntegerVector autoconvert(IntegerVector lu, IntegerVector mask, NumericVector prob, IntegerVector codes);
RcppExport SEXP _lulcc2_autoconvert(SEXP luSEXP, SEXP maskSEXP, SEXP probSEXP, SEXP codesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type lu(luSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type mask(maskSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type prob(probSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type codes(codesSEXP);
    rcpp_result_gen = Rcpp::wrap(autoconvert(lu, mask, prob, codes));
    return rcpp_result_gen;
END_RCPP
}
// inC
LogicalVector inC(IntegerVector x, IntegerVector table);
RcppExport SEXP _lulcc2_inC(SEXP xSEXP, SEXP tableSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type table(tableSEXP);
    rcpp_result_gen = Rcpp::wrap(inC(x, table));
    return rcpp_result_gen;
END_RCPP
}
// orderC
Rcpp::IntegerVector orderC(NumericVector x, bool decreasing);
RcppExport SEXP _lulcc2_orderC(SEXP xSEXP, SEXP decreasingSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< bool >::type decreasing(decreasingSEXP);
    rcpp_result_gen = Rcpp::wrap(orderC(x, decreasing));
    return rcpp_result_gen;
END_RCPP
}
// ordered
IntegerVector ordered(IntegerVector lu0, NumericMatrix tprob, IntegerVector dmd, IntegerVector alloc_order, IntegerVector categories, bool stochastic, int maxiter);
RcppExport SEXP _lulcc2_ordered(SEXP lu0SEXP, SEXP tprobSEXP, SEXP dmdSEXP, SEXP alloc_orderSEXP, SEXP categoriesSEXP, SEXP stochasticSEXP, SEXP maxiterSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type lu0(lu0SEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type tprob(tprobSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type dmd(dmdSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type alloc_order(alloc_orderSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type categories(categoriesSEXP);
    Rcpp::traits::input_parameter< bool >::type stochastic(stochasticSEXP);
    Rcpp::traits::input_parameter< int >::type maxiter(maxiterSEXP);
    rcpp_result_gen = Rcpp::wrap(ordered(lu0, tprob, dmd, alloc_order, categories, stochastic, maxiter));
    return rcpp_result_gen;
END_RCPP
}
// updatehist
IntegerVector updatehist(IntegerVector lu0, IntegerVector lu1, IntegerVector hist);
RcppExport SEXP _lulcc2_updatehist(SEXP lu0SEXP, SEXP lu1SEXP, SEXP histSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type lu0(lu0SEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type lu1(lu1SEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type hist(histSEXP);
    rcpp_result_gen = Rcpp::wrap(updatehist(lu0, lu1, hist));
    return rcpp_result_gen;
END_RCPP
}

RcppExport SEXP allocateclue(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
RcppExport SEXP allocateclues(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
RcppExport SEXP total(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_lulcc2_allowfun", (DL_FUNC) &_lulcc2_allowfun, 5},
    {"_lulcc2_autoconvert", (DL_FUNC) &_lulcc2_autoconvert, 4},
    {"_lulcc2_inC", (DL_FUNC) &_lulcc2_inC, 2},
    {"_lulcc2_orderC", (DL_FUNC) &_lulcc2_orderC, 2},
    {"_lulcc2_ordered", (DL_FUNC) &_lulcc2_ordered, 7},
    {"_lulcc2_updatehist", (DL_FUNC) &_lulcc2_updatehist, 3},
    {"allocateclue",       (DL_FUNC) &allocateclue,       16},
    {"allocateclues",      (DL_FUNC) &allocateclues,       8},
    {"total",              (DL_FUNC) &total,               2},
    {NULL, NULL, 0}
};

RcppExport void R_init_lulcc2(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
