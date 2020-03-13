// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>



double lse_rv (
  const arma::rowvec &in_vec
) {
  double a_max = arma::max(in_vec);
  double out = a_max + log(arma::sum(exp(in_vec - a_max)));
  return out;
}

arma::rowvec log_normalpdf(
  const arma::mat &Y,
  const arma::rowvec &mu,
  const arma::mat &Z
) {


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
  arma::vec denom(XB.n_rows);
  for (int i = 0; i < XB.n_rows; i++) {
    denom(i) = sum(exp(XB.row(i)));
  }

  return denom;

}

//' observed log-likelihood
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

  // add prior
  // arma::mat Z0(X.n_cols, X.n_cols);
  // Z0.eye();
  // Z0.diag() *= 0.1;
  // arma::rowvec mu = arma::zeros<arma::rowvec>(B.n_rows);
  ll += arma::accu(log_normalpdf(B, mu0.t(), Z0));

  return ll;
}

// ------------------------- //
// M-step
// ------------------------- //
void emlogit_mstep(
  const arma::mat &Y,
  const arma::mat &X,
  arma::mat &B,
  const arma::mat &omega,
  const arma::mat &mu0,
  const arma::mat &Z0
) {


  // prior variance
  // arma::mat Z0(X.n_cols, X.n_cols);  Z0.eye();
  // Z0.diag() *= 0.1;

  // iterate over each category
  // arma::mat B = B;
  for (int j = 1; j < B.n_cols; j++) {
    arma::mat XB  = X * B;
    arma::vec eXB = sum_exp_beta(XB);
    arma::mat SS  = X.t() * arma::diagmat(omega.col(j-1)) * X;
    arma::vec di  = log(eXB - exp(XB.col(j))) % omega.col(j-1) +
                      (Y.col(j) - 0.5);
    B.col(j) = arma::solve(SS + arma::inv_sympd(Z0),
                           X.t() * di + arma::solve(Z0, mu0));
  }

}

// ------------------------ //
// E-step
// ------------------------ //
arma::vec compute_psi(
  const arma::mat &XB,
  const arma::vec &eXB,
  const int &j
) {

  arma::vec xbj = XB.col(j);
  arma::vec di = log(eXB - exp(xbj));
  arma::vec out = xbj - di;
  return out;
}

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




// EM implementation
//
// [[Rcpp::export]]
arma::mat emlogit_run(
  const arma::mat &Y,
  const arma::mat &X,
  arma::mat B,
  const double &tol,
  const int &max_iter,
  const arma::vec &mu0,
  const arma::mat &Z0,
  const bool &verbose
) {


  double eval = 1.0;
  int    iter = 0;
  arma::mat omega = arma::zeros(Y.n_rows, Y.n_cols);
  double ll_old = log_likelihood(Y, X, B, mu0, Z0);
  while((iter < max_iter) & (eval > tol)) {
    // E-step
    emlogit_estep(X, B, omega);

    // M-step: Update coefficeints
    emlogit_mstep(Y, X, B, omega, mu0, Z0);

    // evaluate log-likelihood
    if (verbose) {
      Rcpp::Rcout << "ll = " << ll_old << std::endl;
      double ll_new   = log_likelihood(Y, X, B, mu0, Z0);
      eval = abs(ll_new - ll_old) / abs(ll_old);
      ll_old = ll_new;      
    }


    // update
    ++iter;
  }

  return B;
}