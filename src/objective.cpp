#include <RcppArmadillo.h>

//' Rcpp-based objective function for L1- or L2-regularized logistic regression
//'
//' @param beta0 intercept
//' @param beta a feature matrix
//' @param x observations
//' @param y response
//' @param lambda penalty
//' @param alpha elasticnet mixing parameter
//'
//' @return Objective function value.
//' @export
//'
//' @examples
//' if (requireNamespace("glmnet")) {
//'   x <- matrix(rnorm(100*20), 100, 20)
//'   y <- sample(1:2, 100, replace = TRUE)
//'   fit <- cv.glmnet(x, y, family = "binomial")
//'   lambda <- fit$lambda.1se
//'   beta0 <- coef(fit, lambda)[1]
//'   beta <- coef(fit, lambda)[-1]
//'   objective_cpp(beta0, beta, t(x), y, lambda, alpha = 1)
//' }
// [[Rcpp::export]]
double objective_cpp(double beta0,
                     arma::vec beta,
                     arma::mat x,
                     arma::uvec y,
                     double lambda,
                     double alpha = 0) {
  int n = y.n_elem;
  arma::mat z = beta0 + x.t()*beta;
  double loglik = arma::accu(y%z - arma::log(1 + arma::exp(z)));
  double penalty = 0.5*(1 - alpha)*arma::accu(arma::square(beta)) +
    alpha*arma::accu(arma::abs(beta));
  return -loglik/n + lambda*penalty;
}
