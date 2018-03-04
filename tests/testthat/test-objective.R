context("test-objective.R")

test_that("R and Cpp objective functions return similar output", {
  x <- matrix(rnorm(100*20), 100, 20)
  y <- sample(1:2, 100, replace = TRUE)
  fit <- glmnet::cv.glmnet(x, y, family = "binomial")
  lambda <- fit$lambda.1se
  beta0 <- coef(fit, lambda)[1]
  beta <- coef(fit, lambda)[-1]
  obj_r <- objective_r(beta0, beta, t(x), y, lambda, alpha = 1)
  obj_cpp <- objective_cpp(beta0, beta, t(x), y, lambda, alpha = 1)
  expect_equivalent(obj_r, obj_cpp)
})
