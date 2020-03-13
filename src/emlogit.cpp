// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>



// ------------------------------------------------------------- //
//                                                               //
//        Helper functions: log-sum-exp, log-likelihood          //
//                                                               //
// ------------------------------------------------------------- //

// log-sum-exp for a arma::rowvec input 
// @param in_vec A reference pass of arma::rowvec.
// @returns A double of log(sum(exp(in_vec))).
double lse_rv (
  const arma::rowvec &in_vec
) {
  double a_max = arma::max(in_vec);
  double out = a_max + log(arma::sum(exp(in_vec - a_max)));
  return out;
}

arma::vec log_sum_exp_beta(const arma::mat &XB) {

  // compute the common term
  arma::vec denom(XB.n_rows);
  for (int i = 0; i < XB.n_rows; i++) {
    denom(i) = lse_rv(XB.row(i));
  }

  return denom;
}


arma::vec sum_exp_beta(const arma::mat &XB) {

  // compute the common term
  arma::vec denom = arma::sum(exp(XB), 1);
  // arma::vec denom(XB.n_rows);
  // for (int i = 0; i < XB.n_rows; i++) {
  //   denom(i) = sum(exp(XB.row(i)));
  // }

  return denom;
}



// log density of multivariate normal 
// @param Y A matrix of outcome variables. Rows are observations and cols are variable dimensions.
// @param mu A vector of means. 
// @param Z Variance covariace matrix. 
// @return A arma::rowvec of evaluated densities.
arma::rowvec log_normalpdf(
  const arma::mat &Y,
  const arma::rowvec &mu,
  const arma::mat &Z
) {

  // compute det(Z)
  double dZ = arma::det(Z);
  int k = Y.n_cols;
  arma::rowvec density(Y.n_rows);
  for (int i = 0; i < Y.n_rows; i++) {
    double A = -log(2.0 * arma::datum::pi) * static_cast<double>(k);
    double B = -log(dZ);
    double C = arma::accu(
      -(Y.row(i) - mu) * arma::solve(Z, arma::trans(Y.row(i) - mu))
    );

    density(i) = (A + B + C) / 2.0;
  }

  return density;
}



//' observed log-likelihood
//' @param Y A matrix of multinomial outcomes.
//' @param X A matrix of covariate matrix. 
//' @param B A matrix of coefficeints. Cols correspond to choices and rows correspond to variables.
//' @param mu0 A vector of prior means. 
//' @param Z0 A matrix of prior variance covariance matrix.
//' @return A double of log-likelihood evaluated at \code{B}.
//' @keyword internal
// [[Rcpp::export]]
double log_likelihood (
  const arma::mat &Y,
  const arma::mat &X,
  const arma::mat &B,
  const arma::vec &mu0,
  const arma::mat &Z0
) {

  arma::mat XB = X * B;
  arma::vec denom = log_sum_exp_beta(XB);

  double ll = 0.0;
  for (int j = 1; j < XB.n_cols; j++) {
    ll += sum( (XB.col(j) - denom) % Y.col(j) );
  }

  // add prior contribution
  ll += arma::accu(log_normalpdf(B, mu0.t(), Z0));

  return ll;
}

// ------------------------------------------------------------- //
//                                                               //
//          M-step: Update regression coefficeints               //
//                                                               //
//       Beta is fixed to zero for J = 0 (first category).       //
//                                                               //
// ------------------------------------------------------------- //
void emlogit_mstep(
  const arma::mat &Y,
  const arma::mat &X,
        arma::mat &B,
  const arma::mat &omega,
  const arma::mat &mu0,
  const arma::mat &Z0
) {

  // iterate over each category (j = 1,...J)
  for (int j = 1; j < B.n_cols; j++) {
    arma::mat XB  = X * B;
    arma::vec eXB = sum_exp_beta(XB);
    arma::mat SS  = X.t() * arma::diagmat(omega.col(j-1)) * X;
    arma::vec di  = log(eXB - exp(XB.col(j))) % omega.col(j-1) +
                      (Y.col(j) - 0.5);
    // update beta[j] | beta[-j]
    B.col(j) = arma::solve(SS + arma::inv_sympd(Z0),
                           X.t() * di + arma::solve(Z0, mu0));
  }
}

// ------------------------------------------------------------- //
//                                                               //
//    E-step: Update expectation of augmented PG variable        //
//                                                               //
// ------------------------------------------------------------- //

// Compute: X * beta[j] - log(sum(exp(X * beta(h)): h \= j))
arma::vec compute_psi(
  const arma::mat &XB,
  const arma::vec &eXB,
  const int       &j
) {

  arma::vec xbj = XB.col(j);
  arma::vec di = log(eXB - exp(xbj));
  arma::vec out = xbj - di;
  return out;
}

// Compute expected 
void emlogit_estep(
  const arma::mat &X,
  const arma::mat &B,
  arma::mat &omega
) {

  arma::mat XB = X * B;
  arma::vec eXB = sum_exp_beta(XB);
  for (int j = 1; j < B.n_cols; j++) {
    arma::vec psi  = compute_psi(XB, eXB, j);
    omega.col(j-1) =  tanh(psi / 2.0) / (2.0 * psi);
  }
}


// ------------------------------------------------------------- //
//                                                               //
//      EM algorithm for the multinomial logit regression        //
//        Based on Polya-Gamma data augmentation scheme          //
//                                                               //
// ------------------------------------------------------------- //

//' EM implementation
//' @keyword internal
// [[Rcpp::export]]
arma::mat emlogit_run(
  const arma::mat &Y,
  const arma::mat &X,
        arma::mat B,
  const double    &tol,
  const int       &max_iter,
  const arma::vec &mu0,
  const arma::mat &Z0,
  const bool      &verbose
) {


  double eval     = 1.0;
  int    iter     = 0;
  arma::mat omega = arma::zeros(Y.n_rows, Y.n_cols);
  double ll_old   = log_likelihood(Y, X, B, mu0, Z0);
  
  // EM updates
  while((iter < max_iter) & (eval > tol)) {
    // E-step -------------------------------------- // 
    emlogit_estep(X, B, omega);

    // M-step -------------------------------------- //
    emlogit_mstep(Y, X, B, omega, mu0, Z0);

    // evaluate log-likelihood --------------------- //
    if (verbose) {
      Rcpp::Rcout << "ll = " << ll_old << std::endl;
    }

    double ll_new = log_likelihood(Y, X, B, mu0, Z0);
    eval = abs(ll_new - ll_old) / abs(ll_old);
    ll_old = ll_new;

    // update iterator ----------------------------- //
    ++iter;
  }

  return B;
}
