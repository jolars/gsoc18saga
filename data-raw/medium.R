library(microbenchmark)
library(glmnet)
library(bigoptim)

# setup loss wrapper
get_loss <- function(fit,
                     X,
                     y,
                     lambda,
                     alpha = 0) {
  X <- X[, -1, drop = FALSE]

  # tidy up data
  n <- NROW(X)
  X <- t(X)
  y <- as.vector(as.numeric(y))
  y[y == min(y)] <- 0
  y[y > min(y)] <- 1

  # retrieve lambda, beta0
  if (inherits(fit, "SAG_fit")) {
    beta <- fit$w[-1]
    beta0 <- fit$w[1]
  } else if (inherits(fit, "lognet")) {
    beta <- as.matrix(fit$beta[-1, , drop = FALSE])
    beta0 <- fit$beta[1]
  }
  # binomial loglikelihood
  cXb <- crossprod(X, beta)
  loglik <- sum(y*(beta0 + cXb) - log(1 + exp(beta0 + cXb)))

  # compute penalty
  penalty <- 0.5*(1 - alpha)*sum(beta^2) + alpha*sum(abs(beta))
  -loglik/n + lambda*penalty
}

set.seed(1)

nrows <- floor(seq(100, 50000, length.out = 20))
ncols <- floor(seq(2, 50, length.out = 20))
data_medium <- data.frame(rows = integer(),
                          cols = integer(),
                          time = double(),
                          loss = double(),
                          package = character())

X <- SparseM::as.matrix(covtype$x)
y <- covtype$y

for (i in nrows) {
  for (j in ncols) {
    # add intercept and sample
    si <- sample.int(i)
    sj <- sample.int(j)
    xx <- cbind(rep.int(1, i), X[si, sj])
    yy <- as.matrix(y[si])
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

    sag_cost <- get_loss(sag_fit, xx, yy, lambda = lambda)
    glmnet_cost <- get_loss(glmnet_fit, xx, yy, lambda = lambda)

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

devtools::use_data(data_medium, overwrite = TRUE)
