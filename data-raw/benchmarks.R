
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
                     lambda,
                     intercept_in_x = TRUE) {
  if (intercept_in_x)
    x <- x[, -1, drop = FALSE]

  # tidy up data
  n <- NROW(x)
  x <- t(x)
  y <- as.vector(as.numeric(y))
  y[y == min(y)] <- 0
  y[y > min(y)] <- 1

  # retrieve lambda, beta0
  if (inherits(fit, "SAG_fit")) {
    beta <- fit$w[-1]
    beta0 <- fit$w[1]
  } else if (inherits(fit, "lognet")) {
    if (intercept_in_x) {
      beta <- as.matrix(fit$beta[-1, , drop = FALSE])
      beta0 <- fit$beta[1]
    } else {
      beta <- fit$beta
      beta0 <- fit$a0
    }
  } else if (inherits(fit, "sklearn.linear_model.logistic.LogisticRegression")) {
    beta <- t(fit$coef_)
    beta0 <- as.vector(fit$intercept_)
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

# saga_path <- function(X, y, C, maxit = 10) {
#   model <- sklearn$linear_model$LogisticRegression(solver = "saga",
#                                                    max_iter = maxit,
#                                                    penalty = "l1",
#                                                    warm_start = TRUE,
#                                                    fit_intercept = TRUE)
#   n <- length(C)
#   beta <- matrix(NA, ncol = n, nrow = ncol(X))
#   a0 <- double(n)
#   X_py <- reticulate::r_to_py(X)
#   y_py <- reticulate::r_to_py(y)
#   for (i in seq_along(C)) {
#     model$set_params(C = C[i])
#     model$fit(X_py, y_py)
#     beta[, i] <- model$coef_
#     a0[i] <- model$intercept_
#   }
#   list(a0 = a0, beta = beta)
# }

library(reticulate)
sklearn <- import("sklearn")
numpy <- import("numpy")

# load datasets
data("covtype.libsvm", package = "bigoptim")
covtype.libsvm$X <- scale(covtype.libsvm$X)
data("spam", package = "ElemStatLearn")
spam$spam <- as.numeric(spam$spam)
spam$spam[spam$spam == min(spam$spam)] <- -1
spam$spam[spam$spam > min(spam$spam)] <- 1
spam[, -ncol(spam)] <- scale(spam[, -ncol(spam)])

datasets <- list(
  covertype = covtype.libsvm,
  a9a = a9a,
  phishing = phishing,
  ijcnn1 = ijcnn1,
  spam = list(X = spam[, -ncol(spam)],
              y = spam$spam),
  w8a = w8a
)

maxit_seq <- floor(c(1, 2, 3, 4, c(seq(5, 200, by = 5))))
tol_seq <- cumprod(seq(0.9, 0.1, length.out = 20))

# setup model
data_mediumhard <- data.frame(dataset = character(),
                              package = character(),
                              time = double(),
                              loss = double())

# compute timings
for (i in seq_along(datasets)) {
  cat("dataset:", names(datasets)[i], "\n")
  X <- as.matrix(datasets[[i]]$X)
  y <- as.vector(datasets[[i]]$y)
  n_obs <- nrow(X)

  lambda <- 1/n_obs
  C <- 1/(n_obs*lambda)

  for (j in seq_along(tol_seq)) {
    set.seed(j*i)
    reticulate::py_set_seed(j*i)
    #cat("maxit:", maxit_seq[j], "\n")
    cat("tolerance:", tol_seq[j], "\n")
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
      glmnet_fit <- glmnet::glmnet(X,
                                   y,
                                   thresh = tol_seq[j],
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

    saga_fit <- sklearn$linear_model$LogisticRegression(solver = "saga",
                                                        penalty = "l1",
                                                        tol = tol_seq[j],
                                                        C = C,
                                                        warm_start = TRUE,
                                                        fit_intercept = TRUE)
    X_py <- reticulate::r_to_py(as.matrix(X))
    y_py <- reticulate::r_to_py(y)
    saga_time <- system.time({
      saga_fit$fit(X_py, y_py)
    })

    saga_loss <- get_loss(
      saga_fit,
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
