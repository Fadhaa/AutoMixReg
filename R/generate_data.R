#' Generate Mixture Regression Data
#'
#' This function generates synthetic data from a mixture of linear regression models.
#' You can optionally specify the true regression coefficients (`betas`), noise, and cluster probabilities.
#'
#' @param n_samples Number of samples to generate
#' @param n_features Number of features (excluding intercept)
#' @param betas A list of numeric vectors specifying the true regression coefficients for each component.
#'        Each vector should have length = n_features + 1 (intercept included).
#' @param cluster_probs Vector of probabilities summing to 1, determining component weights
#' @param noise_std Standard deviation of the error term
#' @param seed Optional random seed
#'
#' @return A data.frame with generated features (x1, ..., xN), the response variable \code{y}, and the true \code{cluster} label
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
#' @export
generate_mixture_data <- function(n_samples,
                                  n_features,
                                  betas,
                                  cluster_probs,
                                  noise_std = 1,
                                  seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  n_clusters <- length(betas)

  # Validate inputs
  stopifnot(all(sapply(betas, length) == (n_features + 1)))
  stopifnot(length(cluster_probs) == n_clusters)
  stopifnot(abs(sum(cluster_probs) - 1) < 1e-6)

  # Assign clusters
  cluster <- sample(seq_len(n_clusters), size = n_samples, replace = TRUE, prob = cluster_probs)

  # Generate features
  X <- matrix(rnorm(n_samples * n_features), ncol = n_features)
  X_with_intercept <- cbind(1, X)  # Add intercept

  # Generate y
  y <- numeric(n_samples)
  for (k in seq_len(n_clusters)) {
    idx <- which(cluster == k)
    n_k <- length(idx)
    if (n_k > 0) {
      y[idx] <- X_with_intercept[idx, ] %*% betas[[k]] + rnorm(n_k, mean = 0, sd = noise_std)
    }
  }

  # Return as data.frame
  df <- as.data.frame(X)
  colnames(df) <- paste0("x", seq_len(n_features))
  df$y <- y
  df$cluster <- cluster

  return(df)
}
