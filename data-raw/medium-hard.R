# setup loss wrapper
get_loss <- function(fit,
                     X,
                     y,
                     lambda,
                     alpha = 1) {
  # tidy up data
  n <- NROW(X)
  X <- t(X)
  y <- as.vector(as.numeric(y))
  y[y == min(y)] <- 0
  y[y > min(y)] <- 1
  beta <- t(fit$coef_)
  beta0 <- as.vector(fit$intercept_)

  n <- length(y)
  # binomial loglikelihood
  cXb <- crossprod(X, beta)
  loglik <- sum(y*(beta0 + cXb) - log(1 + exp(beta0 + cXb)))

  # compute penalty
  penalty <- 0.5*(1 - alpha)*sum(beta^2) + alpha*sum(abs(beta))
  -loglik/n + lambda*penalty
}

library(SparseM)
library(reticulate)
use_python("/opt/anaconda3/bin/python3")

sklearn <- import("sklearn")
numpy <- import("numpy")
glmnet_py <- import("glmnet")

# load datasets
datasets <- list(
  mushrooms = mushrooms,
  covtype = covtype,
  a9a = a9a,
  phishing = phishing,
  ijcnn1 = ijcnn1,
  skin_nonskin = skin_nonskin
)

# setup tolerance sequence to iterate over
n_tol <- 50
saga_tol <- signif(dexp(seq(0, 8, length.out = n_tol), rate = 0.9), 2)
glmnet_tol <- signif(dexp(seq(0, 13, length.out = n_tol), rate = 0.9), 2)

# setup result data.frame
data_mediumhard <- data.frame(dataset = character(),
                              package = character(),
                              time = double(),
                              loss = double())

# compute timings
for (i in seq_along(datasets)) {
  cat(names(datasets)[i], "\n")
  X <- as.matrix(datasets[[i]]$x)
  y <- datasets[[i]]$y
  n_obs <- nrow(X)

  lambda <- 1/n_obs
  C <- 1/(n_obs*lambda)

  X_py <- reticulate::r_to_py(X)
  y_py <- reticulate::r_to_py(y)

  for (j in seq_len(n_tol)) {
    set.seed(j*i)
    reticulate::py_set_seed(j*i)
    cat("\r", j, "/", n_tol)
    flush.console()
    if (j == n_tol)
      cat("\n")

    glmnet_fit <- glmnet_py$LogitNet(lambda_path = as.matrix(lambda),
                                     tol = glmnet_tol[j],
                                     standardize = FALSE,
                                     fit_intercept = TRUE)

    glmnet_time <- system.time({
      glmnet_fit$fit(X_py, y_py)
    })

    saga_fit <- sklearn$linear_model$LogisticRegression(solver = "saga",
                                                        penalty = "l1",
                                                        tol = saga_tol[j],
                                                        C = C,
                                                        fit_intercept = TRUE)
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
# library(lattice)
# library(latticeExtra)
# lattice::xyplot(loss ~ time | dataset,
#                 data_mediumhard,
#                 xlab = "Time (s)",
#                 ylanb = "Objective loss",
#                 groups = package,
#                 scales = list(relation = "free"),
#                 auto.key = list(lines = TRUE, points = TRUE)) +
#   glayer(panel.smoother(..., span = 0.5))

devtools::use_data(data_mediumhard, overwrite = TRUE)
