//#include <Rcpp.h>
#include <RcppArmadillo.h>
//using namespace Rcpp;


// [[Rcpp::export]]
double compute_loss(arma::vec lambda, arma::mat x, arma::vec y) {
  //mean(log(1 + exp(-z %*% lambda)))
  arma::mat z = x.each_col() % y;
  double loss = arma::mean(arma::log(1 + arma::exp(-z * lambda)));
  return loss;
}

// [[Rcpp::export]]
arma::vec compute_loss_grad(arma::vec lambda, arma::mat x, arma::vec y) {
  arma::mat z = x.each_col() % y; // {n x d} matrix
  arma::vec b = 1 + arma::exp(z * lambda); // {n x 1} vector
  arma::mat a = z.each_col() / b;
  arma::mat loss_grad = arma::mean(a, 0); // column means
  return loss_grad.as_col();
}
