#' Run Mixture Regression Model with Automatic Component Selection
#'
#' This function runs the full mixture regression pipeline:
#' - Prepares the data
#' - Selects the best number of components using BIC
#' - Fits the final model
#' - Prints the model summary
#'
#' @param df Data frame with predictors and response variable.
#' @param response_col Name of the response column (default: "y").
#' @param seed Optional random seed.
#'
#' @return A list with model, best_k, and bic
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
#' head(df)
#'
#'run_mixture_pipeline(df)
#'
#' @export
run_mixture_pipeline <- function(df, response_col = "y", seed = NULL) {
  feature_cols <- setdiff(names(df), c(response_col, "cluster"))
  X_raw <- as.matrix(df[, feature_cols])
  y <- df[[response_col]]
  X <- cbind(1, X_raw)  # Add intercept

  # Step 1: Use BIC to find best number of components
  bic_result <- select_best_k_bic(X, y, seed = seed)
  best_k <- bic_result$best_k
  cat("\n[OK] Best number of components based on BIC:", best_k, "\n")

  # Step 2: Fit final model
  final_model <- fit_mixture_regression(X, y, n_components = best_k, seed = seed)
  final_bic <- compute_bic(final_model, X, y)

  # Step 3: Display parameters
  cat("\n[INFO] Final Model (k =", best_k, ")\n")
  cat("BIC:", round(final_bic, 2), "\n\n")
  for (k in 1:best_k) {
    cat(paste0("Component ", k, ":\n"))
    cat(sprintf("  Weight (\u03c0):     %.4f\n", final_model$weights[k]))
    cat(sprintf("  Coefficients (\u03b2): %s\n", paste(round(final_model$betas[[k]], 4), collapse = ", ")))
    cat(sprintf("  Std Dev (\u03c3):     %.4f\n", final_model$sigmas[k]))
    cat(strrep("-", 40), "\n")
  }

  return(list(
    model = final_model,
    best_k = best_k,
    bic = final_bic
  ))
}

