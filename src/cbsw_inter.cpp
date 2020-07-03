// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

// [[Rcpp::plugins("cpp11")]]

// [[Rcpp::export]]
double cbsw_inter_loss_cpp(
  const arma::vec &par, 
  const arma::mat &Xmat,
  const arma::vec &pop_vec,
  const arma::sp_mat &Mmat, 
  const std::vector<arma::sp_mat> &constMat,
  const std::vector<arma::vec> &uvec,
  const std::vector<arma::vec> &phi,
  const double &rho
) {
  
  // main objective term 
  double A = arma::accu(pop_vec.t() * par);
  double B = log(arma::accu(exp(Xmat * par)));
  
  // penalty term 
  double q2 = arma::accu((par.t() * Mmat * par) * rho / 2.0); 
  arma::rowvec q1 = arma::zeros<arma::rowvec>(par.n_elem);
  for (int i = 0; i < constMat.size(); i++) {
    arma::vec unew = uvec[i] - phi[i]; 
    q1 += unew.t() * constMat[i];  
  }
  
  double CC = arma::accu(q1 * par) * rho;
  return B - A + q2 + CC;  
}



// [[Rcpp::export]]
arma::vec cbsw_inter_loss_gradient_cpp(
  const arma::vec &par, 
  const arma::mat &Xmat,
  const arma::vec &pop_vec,
  const arma::sp_mat &Mmat, 
  const std::vector<arma::sp_mat> &constMat,
  const std::vector<arma::vec> &uvec,
  const std::vector<arma::vec> &phi,
  const double &rho
) {

  arma::rowvec q1 = arma::zeros<arma::rowvec>(par.n_elem);
  for (int i = 0; i < constMat.size(); i++) {
    arma::vec unew = uvec[i] - phi[i]; 
    q1 += unew.t() * constMat[i];
  }
  
  arma::vec expXb = exp(Xmat * par);
  arma::rowvec A = expXb.t() * Xmat / arma::accu(expXb);
  arma::vec grad = (A.t() - pop_vec) + rho * (Mmat.t() * par + q1.t());
  return grad;
}
