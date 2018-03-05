
# Simulations for medium and medium-hard tests ----------------------------

library(microbenchmark)
library(glmnet)
library(bigoptim)

# setup glmnet-like objective function
logloss <- function(beta0, beta, x, y, lambda, alpha = 0, n) {
   # binomial loglikelihood
  loglik <- sum(y*(beta0 + crossprod(x, beta)) -
                  log(1 + exp(beta0 + crossprod(x, beta))))

  # compute penalty
  penalty <- 0.5*(1 - alpha)*sum(beta^2) + alpha*sum(abs(beta))
  -loglik/n + lambda*penalyt
}

# setup loss wrapper for glmnet, bigoptim, and sklearn
get_loss <- function(fit,
                     x,
                     y,
                     alpha = 0,
                     lambda = NULL,
                     intercept_in_x = TRUE) {
  if (intercept_in_x)
    x <- x[, -1, drop = FALSE]

  # tidy up data
  n <- NROW(x)
  x <- t(x)
  y <- as.numeric(y)
  y[y == min(y)] <- 0
  y[y > min(y)] <- 1

  # retrieve lambda, beta0
  if (inherits(fit, "SAG_fit")) {
    beta <- as.matrix(fit$w[-1])
    beta0 <- fit$w[1]
    lambda <- fit$input$lambda
  } else if (inherits(fit, "lognet")) {
    if (is.null(lambda))
      lambda <- fit$lambda
    if (intercept_in_x) {
      beta <- as.matrix(coef(fit, s = lambda))[-c(1, 2), , drop = FALSE]
      beta0 <- as.matrix(coef(fit, s = lambda))[2]
    } else {
      beta <- as.matrix(coef(fit, s = lambda))[-1, , drop = FALSE]
      beta0 <- as.matrix(coef(fit, s = lambda))[1]
    }
  } else if (inherits(fit, "sklearn.linear_model.logistic.LogisticRegression")) {
    C <- fit$C
    lambda <- 1/(n*C)
    beta <- t(fit$coef_)
    beta0 <- as.vector(fit$intercept_)
  }
  logloss(beta0, beta, x, y, lambda, alpha, n)
}

# Medium ------------------------------------------------------------------

set.seed(1)

data("covtype.libsvm")
d <- covtype.libsvm
d$X <- scale(d$X)

nrows <- floor(seq(100, 50000, length.out = 20))
ncols <- floor(seq(2, 50, length.out = 20))
data_medium <- data.frame(rows = integer(),
                          cols = integer(),
                          time = double(),
                          loss = double(),
                          package = character())

for (i in nrows) {
  for (j in ncols) {
    # add intercept and sample
    si <- sample.int(i)
    sj <- sample.int(j)
    xx <- cbind(rep.int(1, i), d$X[si, sj])
    yy <- d$y[si, , drop = FALSE]
    lambda <- 1/(1 + j)

    # compute timings
    z <- microbenchmark(
      glmnet = {glmnet_fit <- glmnet(xx,
                                     yy,
                                     alpha = 0,
                                     family = "binomial",
                                     intercept = FALSE,
                                     standardize = FALSE)},
      sag = {sag_fit <- sag_fit(xx,
                                yy,
                                lambda = lambda,
                                standardize = FALSE)},
      times = 10
    )

    sag_cost <- get_loss(sag_fit, xx, yy, alpha = 0, lambda = lambda)
    glmnet_cost <- get_loss(glmnet_fit, xx, yy, alpha = 0, lambda = lambda)

    # summarize findings
    timings <- tapply(z$time, z$expr, median, na.rm = TRUE)
    glmnet_time <- timings[names(timings) == "glmnet"]
    sag_time <- timings[names(timings) == "sag"]

    data_medium <- rbind(data_medium,
                         data.frame(
                           rows = i,
                           cols = j,
                           time = c(glmnet_time, sag_time),
                           loss = c(glmnet_cost, sag_cost),
                           package = c("glmnet", "sag")
                         ))
  }
}

# convert time to microseconds
data_medium$time <- data_medium$time/1000

# export raw data
devtools::use_data(data_medium, overwrite = TRUE)

# Medium-Hard -------------------------------------------------------------

library(reticulate)
sklearn <- import("sklearn")
numpy <- import("numpy")
py_set_seed(1)
set.seed(1)

data_mediumhard <- data.frame(rows = integer(),
                              cols = integer(),
                              time = double(),
                              loss = double(),
                              package = character())

d$y <- as.factor(d$y)

for (i in nrows) {
  for (j in ncols) {
    # add intercept and sample
    si <- sample.int(i)
    sj <- sample.int(j)
    xx <- d$X[si, sj]
    yy <- d$y[si]
    lambda <- 1/j
    C <- 1/(i*lambda)

    xx_py <- r_to_py(xx)
    yy_py <- r_to_py(yy)

    # setup model
    saga_model <- sklearn$linear_model$LogisticRegression(solver = "saga",
                                                          C = C,
                                                          warm_start = TRUE,
                                                          penalty = "l1",
                                                          fit_intercept = TRUE)

    # compute timings
    z <- microbenchmark(
      glmnet = {glmnet_fit <- glmnet(xx,
                                     yy,
                                     family = "binomial",
                                     standardize = FALSE)},
      saga = {saga_fit <- saga_model$fit(xx_py, yy_py)},
      times = 10
    )

    saga_cost <- get_loss(saga_fit,
                          xx,
                          yy,
                          alpha = 1,
                          lambda = lambda,
                          intercept_in_x = FALSE)
    glmnet_cost <- get_loss(glmnet_fit,
                            xx,
                            yy,
                            alpha = 1,
                            lambda = lambda,
                            intercept_in_x = FALSE)

    # summarize findings
    timings <- tapply(z$time, z$expr, median, na.rm = TRUE)
    glmnet_time <- timings[names(timings) == "glmnet"]
    saga_time <- timings[names(timings) == "saga"]

    data_mediumhard <- rbind(data_mediumhard,
                             data.frame(
                               rows = i,
                               cols = j,
                               time = c(glmnet_time, saga_time),
                               loss = c(glmnet_cost, saga_cost),
                               package = c("glmnet", "saga")
                             ))
  }
}

# convert time to microseconds
data_mediumhard$time <- data_mediumhard$time/1000

# export data
devtools::use_data(data_mediumhard, overwrite = TRUE)
