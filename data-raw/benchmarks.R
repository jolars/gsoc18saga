
# Simulations for medium and medium-hard tests ----------------------------

library(microbenchmark)
library(glmnet)
library(bigoptim)

# setup glmnet-like objective function
logloss <- function(beta0, beta, x, y, lambda, alpha = 0) {
  n <- length(y)
  # binomial loglikelihood
  loglik <- sum(y*(beta0 + crossprod(x, beta)) -
                  log(1 + exp(beta0 + crossprod(x, beta))))

  # compute penalty
  penalty <- 0.5*(1 - alpha)*sum(beta^2) + alpha*sum(abs(beta))
  -loglik/n + lambda*penalty
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
    beta0 <- fit$intercept_
  }
  logloss(beta0, beta, x, y, lambda, alpha)
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
                                     lambda = lambda,
                                     family = "binomial",
                                     intercept = FALSE,
                                     standardize = FALSE)},
      sag = {sag_fit <- sag_fit(xx,
                                yy,
                                lambda = lambda,
                                standardize = FALSE)},
      times = 5
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

saga_path <- function(X, y, C, maxit = 10) {
  model <- sklearn$linear_model$LogisticRegression(solver = "saga",
                                                   max_iter = maxit,
                                                   penalty = "l1",
                                                   warm_start = TRUE,
                                                   fit_intercept = TRUE)
  n <- length(C)
  beta <- matrix(NA, ncol = n, nrow = ncol(X))
  a0 <- double(n)
  X_py <- reticulate::r_to_py(X)
  y_py <- reticulate::r_to_py(y)
  for (i in seq_along(C)) {
    model$set_params(C = C[i])
    model$fit(X_py, y_py)
    beta[, i] <- model$coef_
    a0[i] <- model$intercept_
  }
  list(a0 = a0, beta = beta)
}

library(reticulate)
sklearn <- import("sklearn")
numpy <- import("numpy")
py_set_seed(1)
set.seed(1)

# load datasets
data("covtype.libsvm", package = "bigoptim")
data("rcv1_train", package = "bigoptim")
covtype.libsvm$X <- scale(covtype.libsvm$X)
data("spam", package = "ElemStatLearn")
data(LetterRecognition, package = "mlbench")


datasets <- list(covertype = covtype.libsvm,
                 spam = list(X = spam[, -ncol(spam)],
                             y = spam$spam))

maxit_seq <- floor(seq(1, 20, length.out = 10))

# setup model
data_mediumhard <- data.frame(dataset = character(),
                              package = character(),
                              time = double(),
                              loss = double())

# compute timings
for (i in seq_along(datasets)) {
  cat("dataset:", names(datasets)[i], "\n")
  X <- as.matrix(datasets[[i]]$X)
  y <- as.numeric(datasets[[i]]$y)
  n_obs <- nrow(X)

  lambda <- 1/n_obs
  C <- 1/(n_obs*lambda)

  for (maxit in maxit_seq) {
    cat("maxit:", maxit, "\n")
    # glmnet_time <- system.time({
    #   glmnet_fit <- glmnet(X,
    #                        y,
    #                        maxit = maxit,
    #                        family = "binomial",
    #                        standardize = FALSE)
    #
    # })
    # lambda <- glmnet_fit$lambda
    # n_lambda <- length(lambda)
    # saga_time <- system.time({
    #   saga_fit <- saga_path(X, y, C, maxit = maxit)
    # })
    # glmnet_losses <- double(n_lambda)
    # for (j in seq_len(n_lambda)) {
    #   glmnet_losses[j] <- logloss(
    #     beta0 = glmnet_fit$a0[j],
    #     beta = glmnet_fit$beta[, j],
    #     x = t(X),
    #     y = y,
    #     lambda = lambda[j]
    #   )
    # }
    #
    # saga_losses <- double(n_lambda)
    # for (j in seq_len(n_lambda)) {
    #   saga_losses[j] <- logloss(
    #     beta0 = saga_fit$a0[j],
    #     beta = saga_fit$beta[, j],
    #     x = t(X),
    #     y = y,
    #     lambda = lambda[j]
    #   )
    # }

    glmnet_time <- system.time({
      glmnet_fit <- glmnet(X,
                           y,
                           maxit = maxit*10,
                           lambda = lambda,
                           family = "binomial",
                           standardize = FALSE)

    })

    glmnet_loss <- get_loss(
      glmnet_fit,
      X,
      y,
      alpha = 1,
      lambda = lambda,
      intercept_in_x = FALSE
    )

    model <- sklearn$linear_model$LogisticRegression(solver = "saga",
                                                     max_iter = maxit,
                                                     penalty = "l1",
                                                     C = C,
                                                     warm_start = TRUE,
                                                     fit_intercept = TRUE)
    X_py <- reticulate::r_to_py(X)
    y_py <- reticulate::r_to_py(y)
    saga_time <- system.time({
      model$fit(X_py, y_py)
    })

    saga_loss <- get_loss(
      model,
      X,
      y,
      alpha = 1,
      lambda = lambda,
      intercept_in_x = FALSE
    )

    data_mediumhard <- rbind(data_mediumhard,
                             data.frame(
                               dataset = names(datasets)[i],
                               package = c("glmnet", "saga"),
                               time = c(glmnet_time[3], saga_time[3]),
                               loss = c(glmnet_loss, saga_loss)
                             ))
  }
}

data_mediumhard <- data_mediumhard[order(data_mediumhard$time), ]
lattice::xyplot(loss ~ time | dataset, type = "b",
                data_mediumhard,
                groups = package,
                scales = list(relation = "free"),
                auto.key = list(lines = TRUE, points = TRUE))


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

# convert time to microseconds
data_mediumhard$time <- data_mediumhard$time/1000

# export data
devtools::use_data(data_mediumhard, overwrite = TRUE)
