#' R-based objective function for L1- or L2-regularized logistic regression
#'
#' @param beta0 intercept
#' @param beta a feature matrix
#' @param x observations
#' @param y response
#' @param lambda penalty
#' @param alpha elasticnet mixing parameter
#'
#' @return Objective function value.
#' @export
#'
#' @examples
#' if (requireNamespace("glmnet")) {
#'   x <- matrix(rnorm(100*20), 100, 20)
#'   y <- sample(1:2, 100, replace = TRUE)
#'   fit <- glmnet::cv.glmnet(x, y, family = "binomial")
#'   lambda <- fit$lambda.1se
#'   beta0 <- coef(fit, lambda)[1]
#'   beta <- coef(fit, lambda)[-1]
#'   objective_r(beta0, beta, t(x), y, lambda, alpha = 1)
#' }
objective_r <- function(beta0, beta, x, y, lambda, alpha = 0) {
  n <- length(y)
  # binomial loglikelihood
  z <- beta0 + crossprod(x, beta)
  loglik <- sum(y*z - log(1 + exp(z)))

  # compute penalty
  penalty <- 0.5*(1 - alpha)*sum(beta^2) + alpha*sum(abs(beta))
  -loglik/n + lambda*penalty
}
