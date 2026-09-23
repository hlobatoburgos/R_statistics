
#===============================================================
#                                                              |
#                 CHAPTER 1                                    |
#        DESCRIPTIVE STATISTICS OF ONE VARIABLE                |
#                                                              |
#===============================================================

# ============================================================
# 1. MEASURES OF CENTRAL TENDENCY
# ============================================================

# Arithmetic mean
my_mean <- function(x) {
  sum(x) / length(x)
}

# Weighted mean
my_weighted_mean <- function(x, w) {
  sum(x * w) / sum(w)
}

# Mean using frequencies
my_mean_freq <- function(x, n) {
  sum(x * n) / sum(n)
}

# Quadratic mean / RMS
my_rms <- function(x) {
  sqrt(sum(x^2) / length(x))
}

# RMS using frequencies
my_rms_freq <- function(x, n) {
  sqrt(sum(n * x^2) / sum(n))
}

# ============================================================
# 2. MEASURES OF POSITION
# ============================================================

# Median
# R:
# median(x)

# c = 0.25 -> Q1
# c = 0.50 -> Q2 / median
# c = 0.75 -> Q3

my_quantile <- function(x, c) {
  
  x <- sort(x)
  N <- length(x)
  
  cN <- c * N
  E <- floor(cN)
  D <- cN - E
  
  if (D != 0) {
    x[E + 1]
  } else {
    (x[E] + x[E + 1]) / 2
  }
}

# ============================================================
# 3. MEASURES OF DISPERSION
# ============================================================

# Range
my_range <- function(x) {
  max(x) - min(x)
}

# Interquartile range
my_iqr <- function(x) {
  my_quantile(x, 0.75) - my_quantile(x, 0.25)
}

# Interpercentile range P99 - P1
my_interpercentile_range <- function(x) {
  my_quantile(x, 0.99) - my_quantile(x, 0.01)
}

# ------------------------------------------------------------
# Mean deviation
# MD(p) = sum |xi - p| * ni / N
# ------------------------------------------------------------

my_mean_deviation <- function(x, p) {
  sum(abs(x - p)) / length(x)
}

my_mean_deviation_freq <- function(x, n, p) {
  sum(n * abs(x - p)) / sum(n)
}

# ------------------------------------------------------------
# Mean squared error
# MSE(p) = sum ni(xi-p)^2 / N
# ------------------------------------------------------------

my_mse <- function(x, p) {
  sum((x - p)^2) / length(x)
}

my_mse_freq <- function(x, n, p) {
  sum(n * (x - p)^2) / sum(n)
}

# ------------------------------------------------------------
# Population variance
# ------------------------------------------------------------

my_variance <- function(x) {
  mu <- mean(x)
  sum((x - mu)^2) / length(x)
}

my_variance_freq <- function(x, n) {
  N <- sum(n)
  mu <- sum(x * n) / N
  sum(n * (x - mu)^2) / N
}

# Variance from:
# sum(ni*xi^2), N and mean
#
# sigma^2 = sum(ni*xi^2)/N - mu^2

my_variance_from_sums <- function(sum_x2_n, N, mu) {
  sum_x2_n / N - mu^2
}

# Variance from second ordinary moment
#
# sigma^2 = m2 - mu^2

my_variance_from_m2 <- function(m2, mu) {
  m2 - mu^2
}

# ------------------------------------------------------------
# Standard deviation
# ------------------------------------------------------------

my_sd <- function(x) {
  sqrt(my_variance(x))
}

my_sd_from_variance <- function(variance) {
  sqrt(variance)
}

# ------------------------------------------------------------
# Sample variance
# ------------------------------------------------------------

my_sample_variance <- function(x) {
  xbar <- mean(x)
  sum((x - xbar)^2) / (length(x) - 1)
}

my_sample_sd <- function(x) {
  sqrt(my_sample_variance(x))
}

# ============================================================
# 4. COEFFICIENT OF VARIATION
# ============================================================

# CV = sigma / |mu|

my_cv <- function(x) {
  my_sd(x) / abs(mean(x))
}

my_cv_from_values <- function(mu, sigma) {
  sigma / abs(mu)
}

my_cv_percent <- function(x) {
  100 * my_cv(x)
}

my_cv_percent_from_values <- function(mu, sigma) {
  100 * sigma / abs(mu)
}

# ============================================================
# 5. STANDARDISATION
# ============================================================

# Z = (X - mu) / sigma

my_z <- function(x, mu, sigma) {
  (x - mu) / sigma
}

# Recover X:
# X = mu + Z*sigma

my_x_from_z <- function(z, mu, sigma) {
  mu + z * sigma
}

# ============================================================
# 6. MOMENTS
# ============================================================

# Moment of order r about c
#
# m_r(c) = sum((xi-c)^r) / N

my_moment <- function(x, r, c) {
  sum((x - c)^r) / length(x)
}

my_moment_freq <- function(x, n, r, c) {
  sum(n * (x - c)^r) / sum(n)
}

# Ordinary moment
#
# m_r = sum(xi^r) / N

my_ordinary_moment <- function(x, r) {
  sum(x^r) / length(x)
}

my_ordinary_moment_freq <- function(x, n, r) {
  sum(n * x^r) / sum(n)
}

# Central moment
#
# mu_r = sum((xi-mu)^r) / N

my_central_moment <- function(x, r) {
  mu <- mean(x)
  sum((x - mu)^r) / length(x)
}

my_central_moment_freq <- function(x, n, r) {
  N <- sum(n)
  mu <- sum(x * n) / N
  sum(n * (x - mu)^r) / N
}

# ============================================================
# 7. ORDINARY / CENTRAL MOMENTS
# ============================================================

# m1 = mu

# mu2 = m2 - mu^2

my_central_moment_2 <- function(m2, mu) {
  m2 - mu^2
}

# mu3 = m3 - 3*m2*mu + 2*mu^3

my_central_moment_3 <- function(m3, m2, mu) {
  m3 - 3 * m2 * mu + 2 * mu^3
}

# mu4 = m4 - 4*m3*mu + 6*m2*mu^2 - 3*mu^4

my_central_moment_4 <- function(m4, m3, m2, mu) {
  m4 - 4 * m3 * mu + 6 * m2 * mu^2 - 3 * mu^4
}

# ============================================================
# 8. MODE
# ============================================================

my_mode <- function(x) {
  
  frequencies <- table(x)
  max_frequency <- max(frequencies)
  
  modes <- as.numeric(
    names(frequencies)[frequencies == max_frequency]
  )
  
  if (length(modes) == length(frequencies)) {
    return(NULL)
  }
  
  modes
}

# ============================================================
# 9. SKEWNESS
# ============================================================
# Pearson:
# AP = (mu - Mo) / sigma

my_pearson_skew <- function(mu, mode, sigma) {
  (mu - mode) / sigma
}

# Fisher:
# g1 = mu3 / sigma^3

my_fisher_skew <- function(x) {
  mu3 <- my_central_moment(x, 3)
  sigma <- my_sd(x)
  
  mu3 / sigma^3
}

my_fisher_skew_from_values <- function(mu3, sigma) {
  mu3 / sigma^3
}

# ============================================================
# 10. KURTOSIS
# ============================================================
# Fisher:
# g2 = mu4 / sigma^4 - 3

my_kurtosis <- function(x) {
  mu4 <- my_central_moment(x, 4)
  sigma <- my_sd(x)
  
  mu4 / sigma^4 - 3
}

my_kurtosis_from_values <- function(mu4, sigma) {
  mu4 / sigma^4 - 3
}

#===============================================================
#                                                              |
#                 CHAPTER 2                                    |
#             STATISTICAL MODELLING                            |
#                                                              |
#===============================================================

# ============================================================
# 1. BIVARIATE / JOINT DATA
# ============================================================
# Joint frequency table from paired observations
#
# x = values of X
# y = values of Y

my_joint_frequency <- function(x, y) {
  table(x, y)
}

# Relative joint frequency
my_joint_relative_frequency <- function(x, y) {
  table(x, y) / length(x)
}

# Marginal frequencies of X
my_marginal_x <- function(F) {
  rowSums(F)
}

# Marginal frequencies of Y
my_marginal_y <- function(F) {
  colSums(F)
}


# Total number of observations
my_total_frequency <- function(F) {
  sum(F)
}

# ============================================================
# 2. CONDITIONAL DISTRIBUTIONS
# ============================================================
# Distribution of Y conditioned on X = a
#
# Returns relative frequencies

my_y_given_x <- function(F, x_index) {
  F[x_index, ] / sum(F[x_index, ])
}

# Distribution of X conditioned on Y = b
#
# Returns relative frequencies

my_x_given_y <- function(F, y_index) {
  F[, y_index] / sum(F[, y_index])
}

# Conditional mean of Y given X = a

my_mean_y_given_x <- function(y, F, x_index) {
  frequencies <- F[x_index, ]
  sum(y * frequencies) / sum(frequencies)
}

# Conditional mean of X given Y = b

my_mean_x_given_y <- function(x, F, y_index) {
  frequencies <- F[, y_index]
  sum(x * frequencies) / sum(frequencies)
}

# ============================================================
# 3. INDEPENDENCE
# ============================================================

# Expected frequencies under independence
#
# Eij = ni. * n.j / N

my_expected_frequencies <- function(F) {
  
  row_totals <- rowSums(F)
  col_totals <- colSums(F)
  N <- sum(F)
  
  outer(row_totals, col_totals) / N
}

# Check independence
#
# Two variables are independent if:
# nij = ni. * n.j / N

my_independence <- function(F, tolerance = 1e-10) {
  
  expected <- my_expected_frequencies(F)
  
  all(abs(F - expected) < tolerance)
}

# ============================================================
# 4. COVARIANCE
# ============================================================

# Population covariance

my_covariance <- function(x, y) {
  
  mean_x <- mean(x)
  mean_y <- mean(y)
  
  sum((x - mean_x) * (y - mean_y)) / length(x)
}

# Covariance using frequencies
#
# Cov(X,Y) = sum[nij(xi-xbar)(yj-ybar)] / N

my_covariance_freq <- function(x, y, F) {
  
  N <- sum(F)
  
  mean_x <- sum(rowSums(F) * x) / N
  mean_y <- sum(colSums(F) * y) / N
  
  total <- 0
  
  for (i in seq_along(x)) {
    for (j in seq_along(y)) {
      total <- total +
        F[i, j] * (x[i] - mean_x) * (y[j] - mean_y)
    }
  }
  
  total / N
}

# Covariance from moments
#
# Cov(X,Y) = m11 - mx*my

my_covariance_from_moments <- function(m11, mx, my) {
  m11 - mx * my
}

# ============================================================
# 5. CORRELATION
# ============================================================
# Pearson correlation coefficient
#
# r = Cov(X,Y) / (sx * sy)

my_correlation <- function(x, y) {
  my_covariance(x, y) /
    (my_sd(x) * my_sd(y))
}

# Correlation when covariance and SDs are already given

my_correlation_from_values <- function(cov_xy, sigma_x, sigma_y) {
  cov_xy / (sigma_x * sigma_y)
}

# ============================================================
# 6. SIMPLE LINEAR REGRESSION
# ============================================================
# Regression of Y on X:
#
# y = a + bx
#
# b = Cov(X,Y) / Var(X)
# a = ybar - b*xbar

my_regression_y_on_x <- function(x, y) {
  
  b <- my_covariance(x, y) / my_variance(x)
  a <- mean(y) - b * mean(x)
  
  c(intercept = a, slope = b)
}

# Regression of X on Y:
#
# x = a + by

my_regression_x_on_y <- function(x, y) {
  
  b <- my_covariance(x, y) / my_variance(y)
  a <- mean(x) - b * mean(y)
  
  c(intercept = a, slope = b)
}

# Regression coefficients from summary information

my_slope_y_on_x <- function(cov_xy, variance_x) {
  cov_xy / variance_x
}

my_slope_x_on_y <- function(cov_xy, variance_y) {
  cov_xy / variance_y
}


# Intercept Y on X
my_intercept_y_on_x <- function(mean_x, mean_y, slope) {
  mean_y - slope * mean_x
}

# Intercept X on Y
my_intercept_x_on_y <- function(mean_x, mean_y, slope) {
  mean_x - slope * mean_y
}

# ============================================================
# 7. REGRESSION PREDICTIONS
# ============================================================
# Given y = a + bx

my_predict_y <- function(x, a, b) {
  a + b * x
}


# Given x = a + by

my_predict_x <- function(y, a, b) {
  a + b * y
}

# ============================================================
# 8. REGRESSION USING r
# ============================================================
# b(Y/X) = r * sy / sx

my_slope_y_on_x_from_r <- function(r, sigma_x, sigma_y) {
  r * sigma_y / sigma_x
}

# b(X/Y) = r * sx / sy

my_slope_x_on_y_from_r <- function(r, sigma_x, sigma_y) {
  r * sigma_x / sigma_y
}

# ============================================================
# 9. REGRESSION RELATIONSHIPS
# ============================================================
# r^2 = b * b'
#
# b  = slope Y/X
# b' = slope X/Y

my_r_from_regression_slopes <- function(b, b_prime) {
  
  r_squared <- b * b_prime
  
  if (r_squared < 0) {
    return(NA)
  }
  
  sign(b) * sqrt(r_squared)
}

# Check whether two proposed regression slopes
# can belong to the same bivariate data set

my_valid_regression_slopes <- function(b, b_prime) {
  b * b_prime >= 0 && b * b_prime <= 1
}

# ============================================================
# 10. REGRESSION LINES MUST CROSS AT THE CENTRE OF GRAVITY
# ============================================================

# If:
#
# Y/X: y = a + bx
# X/Y: x = a2 + b2*y
#
# The centre of gravity is:
# (mean_x, mean_y)

my_centre_of_gravity <- function(mean_x, mean_y) {
  c(x = mean_x, y = mean_y)
}

# ============================================================
# 11. MSE OF LINEAR REGRESSION
# ============================================================

# MSE = Var(Y) - b^2 Var(X)

my_regression_mse <- function(variance_y, slope, variance_x) {
  variance_y - slope^2 * variance_x
}

# Alternative:
#
# MSE = Var(Y)(1-r^2)

my_regression_mse_from_r <- function(variance_y, r) {
  variance_y * (1 - r^2)
}

# Root mean squared error

my_rmse_regression <- function(variance_y, slope, variance_x) {
  sqrt(my_regression_mse(
    variance_y,
    slope,
    variance_x
  ))
}

# ============================================================
# 12. COEFFICIENT OF DETERMINATION
# ============================================================

# R^2 = r^2

my_r_squared <- function(r) {
  r^2
}

# ============================================================
# 13. VARIANCE DECOMPOSITION
# ============================================================

# Var(Y) = MSE + Var(Y_est)

my_explained_variance <- function(variance_y, mse) {
  variance_y - mse
}


# Var(Y_est) = b^2 Var(X)

my_variance_y_est <- function(slope, variance_x) {
  slope^2 * variance_x
}

# ============================================================
# 14. SPEARMAN CORRELATION
# ============================================================

# Spearman correlation using ranks

my_spearman <- function(x, y) {
  cor(rank(x), rank(y))
}

# ============================================================
# 15. LOGARITHMIC REGRESSION
# ============================================================

# Model:
#
# y = a + b ln(x)

my_log_regression <- function(x, y) {
  
  model <- lm(y ~ log(x))
  
  coefficients(model)
}


# Prediction from logarithmic model

my_predict_log <- function(x, model) {
  predict(model, newdata = data.frame(x = x))
}

# ============================================================
# 16. CONTINGENCY TABLE / CHI-SQUARE
# ============================================================
# Chi-square statistic
#
# chi^2 = sum((O-E)^2/E)

my_chi_square <- function(F) {
  
  expected <- my_expected_frequencies(F)
  
  sum((F - expected)^2 / expected)
}

# Contingency coefficient
#
# C = sqrt(chi^2 / (chi^2 + N))

my_contingency_coefficient <- function(chi_square, N) {
  sqrt(chi_square / (chi_square + N))
}

# Calculate both at once

my_contingency_analysis <- function(F) {
  
  N <- sum(F)
  chi2 <- my_chi_square(F)
  C <- my_contingency_coefficient(chi2, N)
  
  c(
    chi_square = chi2,
    contingency_coefficient = C
  )
}

# ============================================================
# 17. GROUPED BIVARIATE DATA
# ============================================================
# If X and Y are grouped into intervals,
# use class marks.

my_class_mark <- function(lower, upper) {
  (lower + upper) / 2
}

# Marginal mean of X from bivariate frequency table

my_bivariate_mean_x <- function(x, F) {
  
  N <- sum(F)
  nx <- rowSums(F)
  
  sum(x * nx) / N
}

# Marginal mean of Y

my_bivariate_mean_y <- function(y, F) {
  
  N <- sum(F)
  ny <- colSums(F)
  
  sum(y * ny) / N
}

# Variance of X from bivariate table

my_bivariate_variance_x <- function(x, F) {
  
  N <- sum(F)
  nx <- rowSums(F)
  
  mu_x <- sum(x * nx) / N
  
  sum(nx * (x - mu_x)^2) / N
}

# Variance of Y from bivariate table

my_bivariate_variance_y <- function(y, F) {
  
  N <- sum(F)
  ny <- colSums(F)
  
  mu_y <- sum(y * ny) / N
  
  sum(ny * (y - mu_y)^2) / N
}


# Covariance from grouped bivariate table

my_bivariate_covariance <- function(x, y, F) {
  
  N <- sum(F)
  
  mu_x <- my_bivariate_mean_x(x, F)
  mu_y <- my_bivariate_mean_y(y, F)
  
  total <- 0
  
  for (i in seq_along(x)) {
    for (j in seq_along(y)) {
      
      total <- total +
        F[i, j] *
        (x[i] - mu_x) *
        (y[j] - mu_y)
    }
  }
  
  total / N
}

# ============================================================
# 18. REGRESSION FROM GROUPED BIVARIATE DATA
# ============================================================

my_bivariate_regression_y_on_x <- function(x, y, F) {
  
  cov_xy <- my_bivariate_covariance(x, y, F)
  var_x <- my_bivariate_variance_x(x, F)
  
  b <- cov_xy / var_x
  
  mean_x <- my_bivariate_mean_x(x, F)
  mean_y <- my_bivariate_mean_y(y, F)
  
  a <- mean_y - b * mean_x
  
  c(intercept = a, slope = b)
}

my_bivariate_regression_x_on_y <- function(x, y, F) {
  
  cov_xy <- my_bivariate_covariance(x, y, F)
  var_y <- my_bivariate_variance_y(y, F)
  
  b <- cov_xy / var_y
  
  mean_x <- my_bivariate_mean_x(x, F)
  mean_y <- my_bivariate_mean_y(y, F)
  
  a <- mean_x - b * mean_y
  
  c(intercept = a, slope = b)
}

# ============================================================
# 19. MULTIPLE LINEAR REGRESSION
# ============================================================
# Model:
# z = a0 + a1*x + a2*y

my_multiple_regression <- function(x, y, z) {
  lm(z ~ x + y)
}


# Get coefficients

my_multiple_coefficients <- function(x, y, z) {
  
  model <- lm(z ~ x + y)
  
  coefficients(model)
}


# Predictions

my_multiple_predictions <- function(x, y, z) {
  
  model <- lm(z ~ x + y)
  
  predict(model)
}


# ============================================================
# 20. SSE
# ============================================================

# Sum of Squared Errors

my_sse <- function(actual, predicted) {
  sum((actual - predicted)^2)
}


# SSE directly from a regression model

my_sse_model <- function(model) {
  sum(residuals(model)^2)
}


# ============================================================
# 21. RESIDUAL VARIANCE
# ============================================================

# Residual variance:
#
# SSE / N
#
# when following the course's MSE/variance convention

my_residual_variance <- function(actual, predicted) {
  sum((actual - predicted)^2) / length(actual)
}


# ============================================================
# 22. MULTIPLE REGRESSION R^2
# ============================================================

my_multiple_r_squared <- function(model) {
  summary(model)$r.squared
}


# ============================================================
# 23. RESIDUALS
# ============================================================

my_residuals <- function(actual, predicted) {
  actual - predicted
}


# ============================================================
# END OF CHAPTER 2
# ============================================================
