# setup glmnet-like objective function
logloss <- function(beta0, beta, x, y, lambda, alpha = 1) {
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
                     lambda,
                     alpha = 1) {
  # tidy up data
  n <- NROW(x)
  x <- t(x)
  y <- as.vector(as.numeric(y))
  y[y == min(y)] <- 0
  y[y > min(y)] <- 1

  # retrieve lambda, beta0
  if (inherits(fit, "lognet")) {
    beta <- as.matrix(fit$beta)
    beta0 <- as.vector(fit$a0)
  } else if (inherits(fit, "sklearn.linear_model.logistic.LogisticRegression")) {
    beta <- t(fit$coef_)
    beta0 <- as.vector(fit$intercept_)
  }
  logloss(beta0, beta, x, y, lambda, alpha)
}


library(reticulate)
sklearn <- import("sklearn")
numpy <- import("numpy")

library(SparseM)

# load datasets
datasets <- list(
  covtype = covtype,
  a9a = a9a,
  phishing = phishing,
  ijcnn1 = ijcnn1,
  susy = susy,
  w8a = w8a,
  skin = skin
)

# setup tolerance sequence to iterate over
tol_seq <- cumprod(seq(0.9, 0.1, length.out = 20))

# setup result data.frame
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
    cat("tolerance:", tol_seq[j], "\n")

    glmnet_time <- system.time({
      glmnet_fit <- glmnet::glmnet(X,
                                   y,
                                   thresh = tol_seq[j],
                                   lambda = lambda,
                                   family = "binomial",
                                   standardize = FALSE)

    })


    saga_fit <- sklearn$linear_model$LogisticRegression(solver = "saga",
                                                        penalty = "l1",
                                                        tol = tol_seq[j],
                                                        C = C,
                                                        #warm_start = TRUE,
                                                        fit_intercept = TRUE)
    X_py <- reticulate::r_to_py(as.matrix(X))
    y_py <- reticulate::r_to_py(y)
    saga_time <- system.time({
      saga_fit$fit(X_py, y_py)
    })

    # retrieve loss
    glmnet_loss <- get_loss(glmnet_fit, X, y, lambda = lambda)
    saga_loss <- get_loss(saga_fit, X, y, lambda = lambda)

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
