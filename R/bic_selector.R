#' Select Best Number of Components using BIC
#'
#' Iteratively fits mixture regression models with increasing components and selects the model with the lowest BIC.
#'
#' @param X A numeric matrix of predictors (including intercept)
#' @param y A numeric vector of response values
#' @param k_max Maximum number of components to consider
#' @param seed Optional random seed
#'
#' @return A list containing the best model, best_k, and best BIC
#'
#' @examples
#' custom_betas <- list(
#'   c(0.5, 1.5, 2, 3),   # Cluster 1
#'   c(1, 2, 3, 4),       # Cluster 2
#'   c(2, 3, 4, 6)        # Cluster 3
#' )
#' df <- generate_mixture_data(
#'   n_samples = 1000,
#'   n_features = 3,
#'   betas = custom_betas,
#'   cluster_probs = c(0.4, 0.3, 0.3),
#'   noise_std = 1,
#'   seed = 42
#' )
#' response_col <- "y"
#' feature_cols <- setdiff(names(df), c(response_col, "cluster"))
#' X_raw <- as.matrix(df[, feature_cols])
#' y <- df[[response_col]]
#' X <- cbind(1, X_raw)  # Add intercept
#'
#' bic_result <- select_best_k_bic(X, y, seed = 42)
#' best_k <- bic_result$best_k
#'
#' @export
#' @importFrom stats rnorm sd dnorm
select_best_k_bic <- function(X, y, k_max = 10, seed = NULL) {
  bic_scores <- numeric()
  models <- list()

  best_bic <- Inf
  best_k <- NULL
  best_model <- NULL

  for (k in 1:k_max) {
    model_k <- fit_mixture_regression(X, y, n_components = k, seed = seed)
    bic_k <- compute_bic(model_k, X, y)

    bic_scores[k] <- bic_k
    models[[k]] <- model_k

    cat(sprintf("Components: %d, BIC: %.2f\n", k, bic_k))

    if (bic_k < best_bic) {
      best_bic <- bic_k
      best_k <- k
      best_model <- model_k
    } else if (k > best_k + 1) {
      break
    }
  }

  return(list(
    best_model = best_model,
    best_k = best_k,
    best_bic = best_bic,
    all_bic = bic_scores,
    all_models = models
  ))
}

#' Compute BIC for Fitted Mixture Model
#'
#' @param model A fitted mixture model (from fit_mixture_regression)
#' @param X Predictor matrix
#' @param y Response vector
#'
#' @return Numeric BIC value
#'
#' @export
compute_bic <- function(model, X, y) {
  n <- nrow(X)
  p <- ncol(X)
  k <- length(model$weights)
  n_params <- k * (p + 2) - 1  # k betas, k sigmas, k-1 weights
  log_likelihood <- model$log_likelihood
  bic <- n_params * log(n) - 2 * log_likelihood
  return(bic)
}
