#' Fit a Mixture of Linear Regressions using the EM Algorithm
#'
#' This function fits a mixture of linear regression models using the Expectation-Maximization (EM) algorithm.
#'
#' @param X A numeric matrix of predictors (with intercept column if desired)
#' @param y A numeric vector of response values
#' @param n_components Number of mixture components to fit
#' @param max_iter Maximum number of EM iterations
#' @param tol Convergence tolerance for the log-likelihood
#' @param seed Optional random seed
#'
#' @return A list containing fitted weights, coefficients, standard deviations, log-likelihoods, and responsibilities
#' @export
#' @importFrom stats rnorm sd dnorm
fit_mixture_regression <- function(X, y, n_components, max_iter = 100, tol = 1e-4, seed = NULL) {
  n <- nrow(X)
  p <- ncol(X)

  set.seed(seed)

  # Initialize parameters
  weights <- rep(1 / n_components, n_components)
  betas <- lapply(1:n_components, function(i) rnorm(p))
  sigmas <- rep(sd(y), n_components)
  log_likelihoods <- numeric()

  for (iter in 1:max_iter) {
    # E-step: compute responsibilities
    resp <- matrix(0, n, n_components)
    for (k in 1:n_components) {
      mu_k <- X %*% betas[[k]]
      resp[, k] <- weights[k] * dnorm(y, mean = mu_k, sd = sigmas[k])
    }
    row_sums <- rowSums(resp)
    resp <- resp / row_sums

    # M-step
    for (k in 1:n_components) {
      r_k <- resp[, k]
      R <- diag(r_k)
      X_weighted <- t(X) %*% R %*% X
      y_weighted <- t(X) %*% R %*% y
      betas[[k]] <- solve(X_weighted, y_weighted)
      y_pred <- X %*% betas[[k]]
      sigmas[k] <- sqrt(sum(r_k * (y - y_pred)^2) / sum(r_k))
      weights[k] <- mean(r_k)
    }

    # Log-likelihood
    ll <- sum(log(rowSums(sapply(1:n_components, function(k) {
      weights[k] * dnorm(y, mean = X %*% betas[[k]], sd = sigmas[k])
    }))))
    log_likelihoods <- c(log_likelihoods, ll)

    if (iter > 1 && abs(log_likelihoods[iter] - log_likelihoods[iter - 1]) < tol) {
      break
    }
  }

  list(
    weights = weights,
    betas = betas,
    sigmas = sigmas,
    log_likelihood = log_likelihoods[length(log_likelihoods)],
    resp = resp
  )
}
