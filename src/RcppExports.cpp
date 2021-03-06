// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// predict_prob
arma::mat predict_prob(const arma::mat& X, const arma::mat& B);
RcppExport SEXP _emlogit_predict_prob(SEXP XSEXP, SEXP BSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type X(XSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type B(BSEXP);
    rcpp_result_gen = Rcpp::wrap(predict_prob(X, B));
    return rcpp_result_gen;
END_RCPP
}
// log_likelihood
double log_likelihood(const arma::mat& Y, const arma::mat& X, const arma::mat& B, const arma::vec& mu0, const arma::mat& Z0);
RcppExport SEXP _emlogit_log_likelihood(SEXP YSEXP, SEXP XSEXP, SEXP BSEXP, SEXP mu0SEXP, SEXP Z0SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type Y(YSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type X(XSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type B(BSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type mu0(mu0SEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type Z0(Z0SEXP);
    rcpp_result_gen = Rcpp::wrap(log_likelihood(Y, X, B, mu0, Z0));
    return rcpp_result_gen;
END_RCPP
}
// emlogit_run
arma::mat emlogit_run(const arma::mat& Y, const arma::mat& X, arma::mat& B, const double& tol, const int& max_iter, const arma::vec& mu0, const arma::mat& Z0, const bool& verbose);
RcppExport SEXP _emlogit_emlogit_run(SEXP YSEXP, SEXP XSEXP, SEXP BSEXP, SEXP tolSEXP, SEXP max_iterSEXP, SEXP mu0SEXP, SEXP Z0SEXP, SEXP verboseSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type Y(YSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type X(XSEXP);
    Rcpp::traits::input_parameter< arma::mat& >::type B(BSEXP);
    Rcpp::traits::input_parameter< const double& >::type tol(tolSEXP);
    Rcpp::traits::input_parameter< const int& >::type max_iter(max_iterSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type mu0(mu0SEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type Z0(Z0SEXP);
    Rcpp::traits::input_parameter< const bool& >::type verbose(verboseSEXP);
    rcpp_result_gen = Rcpp::wrap(emlogit_run(Y, X, B, tol, max_iter, mu0, Z0, verbose));
    return rcpp_result_gen;
END_RCPP
}
// emlogit_var
arma::mat emlogit_var(const arma::mat& Y, const arma::mat& X, const arma::mat& B, const arma::vec& mu0, const arma::mat& Z0, const bool& robust);
RcppExport SEXP _emlogit_emlogit_var(SEXP YSEXP, SEXP XSEXP, SEXP BSEXP, SEXP mu0SEXP, SEXP Z0SEXP, SEXP robustSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type Y(YSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type X(XSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type B(BSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type mu0(mu0SEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type Z0(Z0SEXP);
    Rcpp::traits::input_parameter< const bool& >::type robust(robustSEXP);
    rcpp_result_gen = Rcpp::wrap(emlogit_var(Y, X, B, mu0, Z0, robust));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_emlogit_predict_prob", (DL_FUNC) &_emlogit_predict_prob, 2},
    {"_emlogit_log_likelihood", (DL_FUNC) &_emlogit_log_likelihood, 5},
    {"_emlogit_emlogit_run", (DL_FUNC) &_emlogit_emlogit_run, 8},
    {"_emlogit_emlogit_var", (DL_FUNC) &_emlogit_emlogit_var, 6},
    {NULL, NULL, 0}
};

RcppExport void R_init_emlogit(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
