# ============================================================
# CHAPTER 1 - DESCRIPTIVE STATISTICS OF ONE VARIABLE
# ============================================================

# 1. CENTRAL TENDENCY

# x: data vector
my_mean <- function(x) sum(x) / length(x)

# x: values, w: weights
my_weighted_mean <- function(x, w) sum(x * w) / sum(w)

# x: values, n: frequencies
my_mean_freq <- function(x, n) sum(x * n) / sum(n)

# x: data vector
my_rms <- function(x) sqrt(sum(x^2) / length(x))

# x: values, n: frequencies
my_rms_freq <- function(x, n) sqrt(sum(n * x^2) / sum(n))


# 2. POSITION

# x: data vector, c: quantile (0.25, 0.5, 0.75...)
my_quantile <- function(x, c) {
  x <- sort(x)
  N <- length(x)
  cN <- c * N
  E <- floor(cN)
  D <- cN - E
  if (D != 0) x[E + 1] else (x[E] + x[E + 1]) / 2
}


# 3. DISPERSION

# x: data vector
my_range <- function(x) max(x) - min(x)

# x: data vector
my_iqr <- function(x) my_quantile(x, 0.75) - my_quantile(x, 0.25)

# x: data vector
my_interpercentile_range <- function(x) {
  my_quantile(x, 0.99) - my_quantile(x, 0.01)
}

# x: data vector, p: reference value
my_mean_deviation <- function(x, p) {
  sum(abs(x - p)) / length(x)
}

# x: values, n: frequencies, p: reference value
my_mean_deviation_freq <- function(x, n, p) {
  sum(n * abs(x - p)) / sum(n)
}

# x: data vector, p: reference value
my_mse <- function(x, p) sum((x - p)^2) / length(x)

# x: values, n: frequencies, p: reference value
my_mse_freq <- function(x, n, p) {
  sum(n * (x - p)^2) / sum(n)
}

# x: data vector; population variance
my_variance <- function(x) {
  mu <- mean(x)
  sum((x - mu)^2) / length(x)
}

# x: values, n: frequencies
my_variance_freq <- function(x, n) {
  N <- sum(n)
  mu <- sum(x * n) / N
  sum(n * (x - mu)^2) / N
}

# sum_x2_n: sum(n*x^2), N: total frequency, mu: mean
my_variance_from_sums <- function(sum_x2_n, N, mu) {
  sum_x2_n / N - mu^2
}

# m2: second ordinary moment, mu: mean
my_variance_from_m2 <- function(m2, mu) m2 - mu^2

# x: data vector
my_sd <- function(x) sqrt(my_variance(x))

# variance: already calculated variance
my_sd_from_variance <- function(variance) sqrt(variance)

# x: data vector; sample variance
my_sample_variance <- function(x) {
  xbar <- mean(x)
  sum((x - xbar)^2) / (length(x) - 1)
}

# x: data vector
my_sample_sd <- function(x) sqrt(my_sample_variance(x))


# 4. COEFFICIENT OF VARIATION

# x: data vector
my_cv <- function(x) my_sd(x) / abs(mean(x))

# mu: mean, sigma: standard deviation
my_cv_from_values <- function(mu, sigma) sigma / abs(mu)

# x: data vector; CV in %
my_cv_percent <- function(x) 100 * my_cv(x)

# mu: mean, sigma: standard deviation
my_cv_percent_from_values <- function(mu, sigma) {
  100 * sigma / abs(mu)
}


# 5. STANDARDISATION

# x: value, mu: mean, sigma: SD
my_z <- function(x, mu, sigma) (x - mu) / sigma

# z: standardised value, mu: mean, sigma: SD
my_x_from_z <- function(z, mu, sigma) mu + z * sigma


# 6. MOMENTS

# x: data, r: order, c: reference point
my_moment <- function(x, r, c) {
  sum((x - c)^r) / length(x)
}

# x: values, n: frequencies, r: order, c: reference point
my_moment_freq <- function(x, n, r, c) {
  sum(n * (x - c)^r) / sum(n)
}

# x: data, r: order
my_ordinary_moment <- function(x, r) {
  sum(x^r) / length(x)
}

# x: values, n: frequencies, r: order
my_ordinary_moment_freq <- function(x, n, r) {
  sum(n * x^r) / sum(n)
}

# x: data, r: order
my_central_moment <- function(x, r) {
  mu <- mean(x)
  sum((x - mu)^r) / length(x)
}

# x: values, n: frequencies, r: order
my_central_moment_freq <- function(x, n, r) {
  N <- sum(n)
  mu <- sum(x * n) / N
  sum(n * (x - mu)^r) / N
}


# 7. ORDINARY / CENTRAL MOMENTS

# m2: second ordinary moment, mu: mean
my_central_moment_2 <- function(m2, mu) m2 - mu^2

# m3: third ordinary moment, m2: second moment, mu: mean
my_central_moment_3 <- function(m3, m2, mu) {
  m3 - 3 * m2 * mu + 2 * mu^3
}

# m4,m3,m2: ordinary moments, mu: mean
my_central_moment_4 <- function(m4, m3, m2, mu) {
  m4 - 4 * m3 * mu + 6 * m2 * mu^2 - 3 * mu^4
}


# 8. MODE

# x: data vector
my_mode <- function(x) {
  f <- table(x)
  m <- as.numeric(names(f)[f == max(f)])
  if (length(m) == length(f)) return(NULL)
  m
}


# 9. SKEWNESS

# mu: mean, mode: mode, sigma: SD
my_pearson_skew <- function(mu, mode, sigma) {
  (mu - mode) / sigma
}

# x: data vector
my_fisher_skew <- function(x) {
  mu3 <- my_central_moment(x, 3)
  sigma <- my_sd(x)
  mu3 / sigma^3
}

# mu3: third central moment, sigma: SD
my_fisher_skew_from_values <- function(mu3, sigma) {
  mu3 / sigma^3
}


# 10. KURTOSIS

# x: data vector
my_kurtosis <- function(x) {
  mu4 <- my_central_moment(x, 4)
  sigma <- my_sd(x)
  mu4 / sigma^4 - 3
}

# mu4: fourth central moment, sigma: SD
my_kurtosis_from_values <- function(mu4, sigma) {
  mu4 / sigma^4 - 3
}


# ============================================================
# CHAPTER 2 - STATISTICAL MODELLING
# ============================================================

# 1. BIVARIATE / JOINT DATA

# x,y: paired observations
my_joint_frequency <- function(x, y) table(x, y)

# x,y: paired observations
my_joint_relative_frequency <- function(x, y) {
  table(x, y) / length(x)
}

# F: joint frequency table
my_marginal_x <- function(F) rowSums(F)

# F: joint frequency table
my_marginal_y <- function(F) colSums(F)

# F: joint frequency table
my_total_frequency <- function(F) sum(F)


# 2. CONDITIONAL DISTRIBUTIONS

# F: joint table, x_index: row corresponding to X value
my_y_given_x <- function(F, x_index) {
  F[x_index, ] / sum(F[x_index, ])
}

# F: joint table, y_index: column corresponding to Y value
my_x_given_y <- function(F, y_index) {
  F[, y_index] / sum(F[, y_index])
}

# y: Y values, F: joint table, x_index: X row
my_mean_y_given_x <- function(y, F, x_index) {
  f <- F[x_index, ]
  sum(y * f) / sum(f)
}

# x: X values, F: joint table, y_index: Y column
my_mean_x_given_y <- function(x, F, y_index) {
  f <- F[, y_index]
  sum(x * f) / sum(f)
}


# 3. INDEPENDENCE

# F: joint frequency table
my_expected_frequencies <- function(F) {
  rx <- rowSums(F)
  cy <- colSums(F)
  N <- sum(F)
  outer(rx, cy) / N
}

# F: joint frequency table
my_independence <- function(F, tolerance = 1e-10) {
  E <- my_expected_frequencies(F)
  all(abs(F - E) < tolerance)
}


# 4. COVARIANCE

# x,y: paired observations
my_covariance <- function(x, y) {
  mx <- mean(x)
  my <- mean(y)
  sum((x - mx) * (y - my)) / length(x)
}

# x,y: class values, F: joint frequency table
my_covariance_freq <- function(x, y, F) {
  N <- sum(F)
  mx <- sum(rowSums(F) * x) / N
  my <- sum(colSums(F) * y) / N
  total <- 0
  
  for (i in seq_along(x)) {
    for (j in seq_along(y)) {
      total <- total + F[i,j] * (x[i] - mx) * (y[j] - my)
    }
  }
  total / N
}

# m11: E(XY), mx: mean X, my: mean Y
my_covariance_from_moments <- function(m11, mx, my) {
  m11 - mx * my
}


# 5. CORRELATION

# x,y: paired observations
my_correlation <- function(x, y) {
  my_covariance(x, y) / (my_sd(x) * my_sd(y))
}

# cov_xy: covariance, sigma_x/y: standard deviations
my_correlation_from_values <- function(cov_xy, sigma_x, sigma_y) {
  cov_xy / (sigma_x * sigma_y)
}


# 6. SIMPLE LINEAR REGRESSION

# x,y: paired observations; returns intercept and slope of Y/X
my_regression_y_on_x <- function(x, y) {
  b <- my_covariance(x, y) / my_variance(x)
  a <- mean(y) - b * mean(x)
  c(intercept = a, slope = b)
}

# x,y: paired observations; returns intercept and slope of X/Y
my_regression_x_on_y <- function(x, y) {
  b <- my_covariance(x, y) / my_variance(y)
  a <- mean(x) - b * mean(y)
  c(intercept = a, slope = b)
}

# cov_xy: covariance, variance_x: variance of X
my_slope_y_on_x <- function(cov_xy, variance_x) {
  cov_xy / variance_x
}

# cov_xy: covariance, variance_y: variance of Y
my_slope_x_on_y <- function(cov_xy, variance_y) {
  cov_xy / variance_y
}

# mean_x/y: means, slope: regression slope
my_intercept_y_on_x <- function(mean_x, mean_y, slope) {
  mean_y - slope * mean_x
}

# mean_x/y: means, slope: regression slope
my_intercept_x_on_y <- function(mean_x, mean_y, slope) {
  mean_x - slope * mean_y
}


# 7. REGRESSION PREDICTIONS

# x: X value, a: intercept, b: Y/X slope
my_predict_y <- function(x, a, b) a + b * x

# y: Y value, a: intercept, b: X/Y slope
my_predict_x <- function(y, a, b) a + b * y


# 8. REGRESSION USING r

# r: correlation, sigma_x/y: SDs
my_slope_y_on_x_from_r <- function(r, sigma_x, sigma_y) {
  r * sigma_y / sigma_x
}

# r: correlation, sigma_x/y: SDs
my_slope_x_on_y_from_r <- function(r, sigma_x, sigma_y) {
  r * sigma_x / sigma_y
}


# 9. REGRESSION RELATIONSHIPS

# b: Y/X slope, b_prime: X/Y slope
my_r_from_regression_slopes <- function(b, b_prime) {
  r2 <- b * b_prime
  if (r2 < 0) return(NA)
  sign(b) * sqrt(r2)
}

# b,b_prime: two regression slopes
my_valid_regression_slopes <- function(b, b_prime) {
  b * b_prime >= 0 && b * b_prime <= 1
}


# 10. CENTRE OF GRAVITY

# mean_x, mean_y: means of X and Y
my_centre_of_gravity <- function(mean_x, mean_y) {
  c(x = mean_x, y = mean_y)
}


# 11. REGRESSION MSE

# variance_y: Var(Y), slope: Y/X slope, variance_x: Var(X)
my_regression_mse <- function(variance_y, slope, variance_x) {
  variance_y - slope^2 * variance_x
}

# variance_y: Var(Y), r: correlation
my_regression_mse_from_r <- function(variance_y, r) {
  variance_y * (1 - r^2)
}

# variance_y: Var(Y), slope: Y/X slope, variance_x: Var(X)
my_rmse_regression <- function(variance_y, slope, variance_x) {
  sqrt(my_regression_mse(variance_y, slope, variance_x))
}


# 12. COEFFICIENT OF DETERMINATION

# r: correlation
my_r_squared <- function(r) r^2


# 13. VARIANCE DECOMPOSITION

# variance_y: Var(Y), mse: regression MSE
my_explained_variance <- function(variance_y, mse) {
  variance_y - mse
}

# slope: Y/X slope, variance_x: Var(X)
my_variance_y_est <- function(slope, variance_x) {
  slope^2 * variance_x
}


# 14. SPEARMAN

# x,y: paired observations
my_spearman <- function(x, y) {
  cor(rank(x), rank(y))
}


# 15. LOGARITHMIC REGRESSION

# x,y: observations; model y = a + b*ln(x)
my_log_regression <- function(x, y) {
  model <- lm(y ~ log(x))
  coefficients(model)
}

# x: new X value, model: logarithmic regression model
my_predict_log <- function(x, model) {
  predict(model, newdata = data.frame(x = x))
}


# 16. CONTINGENCY TABLE / CHI-SQUARE

# F: observed frequency table
my_chi_square <- function(F) {
  E <- my_expected_frequencies(F)
  sum((F - E)^2 / E)
}

# chi_square: chi-square statistic, N: total observations
my_contingency_coefficient <- function(chi_square, N) {
  sqrt(chi_square / (chi_square + N))
}

# F: observed frequency table
my_contingency_analysis <- function(F) {
  N <- sum(F)
  chi2 <- my_chi_square(F)
  C <- my_contingency_coefficient(chi2, N)
  c(chi_square = chi2, contingency_coefficient = C)
}


# 17. GROUPED BIVARIATE DATA

# lower, upper: class limits
my_class_mark <- function(lower, upper) {
  (lower + upper) / 2
}

# x: X class marks, F: joint frequency table
my_bivariate_mean_x <- function(x, F) {
  N <- sum(F)
  nx <- rowSums(F)
  sum(x * nx) / N
}

# y: Y class marks, F: joint frequency table
my_bivariate_mean_y <- function(y, F) {
  N <- sum(F)
  ny <- colSums(F)
  sum(y * ny) / N
}

# x: X class marks, F: joint frequency table
my_bivariate_variance_x <- function(x, F) {
  N <- sum(F)
  nx <- rowSums(F)
  mx <- sum(x * nx) / N
  sum(nx * (x - mx)^2) / N
}

# y: Y class marks, F: joint frequency table
my_bivariate_variance_y <- function(y, F) {
  N <- sum(F)
  ny <- colSums(F)
  my <- sum(y * ny) / N
  sum(ny * (y - my)^2) / N
}

# x,y: class marks, F: joint frequency table
my_bivariate_covariance <- function(x, y, F) {
  N <- sum(F)
  mx <- my_bivariate_mean_x(x, F)
  my <- my_bivariate_mean_y(y, F)
  total <- 0
  
  for (i in seq_along(x)) {
    for (j in seq_along(y)) {
      total <- total + F[i,j] * (x[i] - mx) * (y[j] - my)
    }
  }
  total / N
}


# 18. REGRESSION FROM GROUPED BIVARIATE DATA

# x,y: class marks, F: joint frequency table
my_bivariate_regression_y_on_x <- function(x, y, F) {
  cov_xy <- my_bivariate_covariance(x, y, F)
  var_x <- my_bivariate_variance_x(x, F)
  b <- cov_xy / var_x
  mx <- my_bivariate_mean_x(x, F)
  my <- my_bivariate_mean_y(y, F)
  a <- my - b * mx
  c(intercept = a, slope = b)
}

# x,y: class marks, F: joint frequency table
my_bivariate_regression_x_on_y <- function(x, y, F) {
  cov_xy <- my_bivariate_covariance(x, y, F)
  var_y <- my_bivariate_variance_y(y, F)
  b <- cov_xy / var_y
  mx <- my_bivariate_mean_x(x, F)
  my <- my_bivariate_mean_y(y, F)
  a <- mx - b * my
  c(intercept = a, slope = b)
}


# 19. MULTIPLE LINEAR REGRESSION

# x,y: predictors, z: dependent variable
my_multiple_regression <- function(x, y, z) {
  lm(z ~ x + y)
}

# x,y: predictors, z: dependent variable
my_multiple_coefficients <- function(x, y, z) {
  coefficients(lm(z ~ x + y))
}

# x,y: predictors, z: dependent variable
my_multiple_predictions <- function(x, y, z) {
  predict(lm(z ~ x + y))
}


# 20. SSE

# actual: observed values, predicted: fitted values
my_sse <- function(actual, predicted) {
  sum((actual - predicted)^2)
}

# model: regression model from lm()
my_sse_model <- function(model) {
  sum(residuals(model)^2)
}


# 21. RESIDUAL VARIANCE

# actual: observed values, predicted: fitted values
my_residual_variance <- function(actual, predicted) {
  sum((actual - predicted)^2) / length(actual)
}


# 22. MULTIPLE REGRESSION R-SQUARED

# model: regression model from lm()
my_multiple_r_squared <- function(model) {
  summary(model)$r.squared
}


# 23. RESIDUALS

# actual: observed values, predicted: fitted values
my_residuals <- function(actual, predicted) {
  actual - predicted
}
