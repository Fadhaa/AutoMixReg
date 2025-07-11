
# AutoMixReg

**AutoMixReg** is an R package for simulating and fitting **Mixture of Linear Regressions** using the **EM algorithm**, and automatically selecting the best number of components using the Bayesian Information Criterion (BIC). It is useful for unsupervised regression modeling and model-based clustering in regression settings....

## Example Usage

### 1. Simulate Data from a Mixture of Linear Regressions

```r
library(AutoMixReg)

custom_betas <- list(
  c(0.5, 1.5, 2, 3),   # Cluster 1
  c(1, 2, 3, 4),       # Cluster 2
  c(2, 3, 4, 6)        # Cluster 3
)

df <- generate_mixture_data(
  n_samples = 1000,
  n_features = 3,
  betas = custom_betas,
  cluster_probs = c(0.4, 0.3, 0.3),
  noise_std = 1,
  seed = 42
)

head(df)
---

## 2. Fit a Mixture Regression Model (EM Algorithm)
```r
feature_cols <- setdiff(names(df), c("y", "cluster"))
X <- cbind(1, as.matrix(df[, feature_cols]))
y <- df$y

model <- fit_mixture_regression(X, y, n_components = 3)

# Inspect estimated parameters
model$weights
model$betas
model$sigmas
---

## 3. Automatically Select Best k Using BIC

```r
bic_result <- select_best_k_bic(X, y, k_max = 6)
bic_result$best_k
---

## 4. One-Line Run Pipeline
```r
run_mixture_pipeline(df)
---






























