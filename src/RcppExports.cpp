// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// lcpa_cpp
Rcpp::List lcpa_cpp(arma::mat x, arma::vec y, int R_max);
RcppExport SEXP _riskslimr_lcpa_cpp(SEXP xSEXP, SEXP ySEXP, SEXP R_maxSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type y(ySEXP);
    Rcpp::traits::input_parameter< int >::type R_max(R_maxSEXP);
    rcpp_result_gen = Rcpp::wrap(lcpa_cpp(x, y, R_max));
    return rcpp_result_gen;
END_RCPP
}
// compute_loss
double compute_loss(arma::vec lambda, arma::mat x, arma::vec y);
RcppExport SEXP _riskslimr_compute_loss(SEXP lambdaSEXP, SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type lambda(lambdaSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(compute_loss(lambda, x, y));
    return rcpp_result_gen;
END_RCPP
}
// compute_loss_grad
arma::vec compute_loss_grad(arma::vec lambda, arma::mat x, arma::vec y);
RcppExport SEXP _riskslimr_compute_loss_grad(SEXP lambdaSEXP, SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type lambda(lambdaSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(compute_loss_grad(lambda, x, y));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_riskslimr_lcpa_cpp", (DL_FUNC) &_riskslimr_lcpa_cpp, 3},
    {"_riskslimr_compute_loss", (DL_FUNC) &_riskslimr_compute_loss, 3},
    {"_riskslimr_compute_loss_grad", (DL_FUNC) &_riskslimr_compute_loss_grad, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_riskslimr(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
