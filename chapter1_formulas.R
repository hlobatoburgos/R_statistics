# ============================================================
# Chapter 1: Descriptive Statistics of one variable
# Formulas from the course notes, implemented in R
# Universidad de Malaga - Metodos Estadisticos
# ============================================================
#
# For every function below:
#   x  = vector of raw data values (or class marks, for grouped data)
#   n  = vector of absolute frequencies (n_i), OPTIONAL.
#        Leave n = NULL to treat x as raw (ungrouped) data.
#        Pass n when working with a frequency table / grouped data.
#
# A base-R equivalent is noted in a comment where one already exists.


# ------------------------------------------------------------
# 1. FREQUENCIES  (slides 5, 9)
# ------------------------------------------------------------

# Absolute frequency:  n_i = count of modality x_i
absolute_freq <- function(x) table(x)

# Relative frequency:  f_i = n_i / N
relative_freq <- function(x) prop.table(table(x))

# Cumulative absolute frequency:  N_i = sum_{j<=i} n_j
cum_absolute_freq <- function(x) cumsum(table(x))

# Cumulative relative frequency:  F_i = N_i / N
cum_relative_freq <- function(x) cumsum(prop.table(table(x)))

# --- Example from the notes: menu prices ---
menus <- c(6,8,6,8,6,8,12,6,8,8,6,8,8,8,12,
           12,8,8,12,6,8,6,6,8,12,6,6,6,6,6)
absolute_freq(menus)
relative_freq(menus)
cum_absolute_freq(menus)
cum_relative_freq(menus)


# ------------------------------------------------------------
# 2. GROUPED DATA: interval width & class marks  (slide 13)
# ------------------------------------------------------------

# Interval width:  a_i = L_i - L_{i-1}
interval_width <- function(L) diff(L)

# Class mark:  x_i = (L_{i-1} + L_i) / 2
class_marks <- function(L) (head(L, -1) + tail(L, -1)) / 2


# ------------------------------------------------------------
# 3. MEASURES OF CENTRAL TENDENCY  (slides 25-40)
# ------------------------------------------------------------

# Simple arithmetic mean:  mu = sum(x_i * n_i) / N
arithmetic_mean <- function(x, n = NULL) {
  if (is.null(n)) return(mean(x))        # base R: mean(x)
  sum(x * n) / sum(n)
}

# Weighted mean:  WM = sum(x_i * w_i) / sum(w_i)
weighted_mean_r <- function(x, w) sum(x * w) / sum(w)
# base R equivalent: weighted.mean(x, w)

# Quadratic mean / RMS:  RMS = sqrt( sum(n_i * x_i^2) / N )
quadratic_mean <- function(x, n = NULL) {
  if (is.null(n)) return(sqrt(mean(x^2)))
  sqrt(sum(n * x^2) / sum(n))
}

# Mode: value(s) with the highest frequency (may return more than one)
mode_stat <- function(x) {
  tb <- table(x)
  as.numeric(names(tb)[tb == max(tb)])
}

# Median
median_stat <- function(x) median(x)     # base R: median(x)


# ------------------------------------------------------------
# 4. QUANTILES, QUARTILES, DECILES, PERCENTILES  (slides 41-47)
# ------------------------------------------------------------
# The notes' discrete-case rule (split cN into integer E and decimal D;
# take place E+1 if D != 0, else average places E and E+1) is exactly
# R's quantile "type 2", so we use that throughout.

quantile_c <- function(x, c) quantile(x, probs = c, type = 2, names = FALSE)

Q1 <- function(x) quantile_c(x, 0.25)
Q2 <- function(x) quantile_c(x, 0.50)    # equals the median
Q3 <- function(x) quantile_c(x, 0.75)
decile     <- function(x, k) quantile_c(x, k / 10)    # k = 1..9
percentile <- function(x, k) quantile_c(x, k / 100)   # k = 1..99


# ------------------------------------------------------------
# 5. DISPERSION MEASURES  (slides 48-58)
# ------------------------------------------------------------

# Range:  R = max - min
range_stat <- function(x) max(x) - min(x)

# Interquartile range:  RQ = Q3 - Q1
IQR_stat <- function(x) Q3(x) - Q1(x)    # base R: IQR(x, type = 2)

# Intercentile range:  RP = P99 - P1
intercentile_range <- function(x) percentile(x, 99) - percentile(x, 1)

# Mean deviation with respect to a value p:  MD(p) = sum(|x_i - p| n_i) / N
mean_deviation_about <- function(x, p, n = NULL) {
  if (is.null(n)) return(mean(abs(x - p)))
  sum(abs(x - p) * n) / sum(n)
}
# Mean deviation about the mean:  MD = MD(mu)
MD <- function(x, n = NULL) mean_deviation_about(x, arithmetic_mean(x, n), n)

# Mean squared error with respect to p:  MSE(p) = sum(n_i (x_i - p)^2) / N
mean_squared_error <- function(x, p, n = NULL) {
  if (is.null(n)) return(mean((x - p)^2))
  sum(n * (x - p)^2) / sum(n)
}

# Population variance:  V = sigma^2 = MSE(mu)
pop_variance <- function(x, n = NULL) mean_squared_error(x, arithmetic_mean(x, n), n)

# Population standard deviation:  sigma = sqrt(V)
pop_sd <- function(x, n = NULL) sqrt(pop_variance(x, n))

# Sample quasi-variance (divides by n-1) -- the usual unbiased estimator:
#   s^2 = sum(n_i (x_i - xbar)^2) / (n - 1)
sample_quasi_variance <- function(x) var(x)   # base R var() already uses n-1
sample_quasi_sd       <- function(x) sd(x)    # base R sd()  already uses n-1

# Sample variance (divides by n):  s_n^2 = (n-1)/n * s^2
sample_variance_n <- function(x) {
  n <- length(x)
  var(x) * (n - 1) / n
}
sample_sd_n <- function(x) sqrt(sample_variance_n(x))


# ------------------------------------------------------------
# 6. COMPARISON MEASURES  (slides 59-62)
# ------------------------------------------------------------

# Standardised variable:  z_i = (x_i - mu) / sigma   (population sigma)
standardise <- function(x) (x - mean(x)) / pop_sd(x)   # cf. base R scale(), which uses sd() (n-1)

# Pearson's coefficient of variation:  CV = sigma / |mu|
CV_stat <- function(x) pop_sd(x) / abs(mean(x))


# ------------------------------------------------------------
# 7. MOMENTS  (slides 63-68)
# ------------------------------------------------------------

# Moment of order r about a point c:  m_r(c) = sum(n_i (x_i - c)^r) / N
moment_about <- function(x, r, c, n = NULL) {
  if (is.null(n)) return(mean((x - c)^r))
  sum(n * (x - c)^r) / sum(n)
}

# Ordinary moment of order r (about 0):  m_r = sum(n_i x_i^r) / N
ordinary_moment <- function(x, r, n = NULL) moment_about(x, r, 0, n)

# Central moment of order r (about the mean):  mu_r = sum(n_i (x_i - mu)^r) / N
central_moment <- function(x, r, n = NULL) {
  mu <- arithmetic_mean(x, n)
  moment_about(x, r, mu, n)
}


# ------------------------------------------------------------
# 8. SHAPE MEASURES: skewness & kurtosis  (slides 69-73)
# ------------------------------------------------------------

# Pearson's skewness coefficient:  AP = (mu - Mo) / sigma
pearson_skewness <- function(x) {
  (mean(x) - mode_stat(x)[1]) / pop_sd(x)
}

# Fisher's skewness coefficient:  g1 = mu_3 / sigma^3
fisher_skewness <- function(x) {
  central_moment(x, 3) / pop_sd(x)^3
}

# Fisher's kurtosis coefficient:  g2 = mu_4 / sigma^4 - 3
fisher_kurtosis <- function(x) {
  central_moment(x, 4) / pop_sd(x)^4 - 3
}


# ============================================================
# DEMO: applying everything to the "shop expenditure" exercise
# (the 100-value data set from your worksheet)
# ============================================================

arr <- c(21.8, 40.8, 25.1, 34.6, 39.1, 37.8, 38.4, 36.0, 34.4, 30.1,
         25.1, 37.4, 38.0, 25.1, 37.8, 29.4, 34.3, 27.2, 23.8, 29.9,
         26.8, 36.8, 25.0, 36.1, 24.9, 38.5, 23.8, 36.8, 31.7, 20.6,
         29.8, 24.3, 36.5, 39.0, 40.9, 27.4, 22.2, 28.2, 31.1, 31.5,
         38.9, 24.6, 22.3, 39.1, 25.9, 21.6, 24.1, 29.4, 22.1, 28.9,
         42.4, 38.5, 43.8, 25.9, 26.5, 33.8, 20.8, 37.5, 37.6, 38.4,
         27.4, 28.5, 31.5, 28.5, 25.1, 31.3, 22.3, 25.3, 22.6, 22.3,
         20.4, 31.1, 21.2, 28.7, 24.9, 23.2, 26.4, 20.6, 27.8, 26.9,
         30.2, 20.5, 31.1, 30.0, 26.7, 25.4, 25.1, 30.3, 28.8, 23.6,
         25.3, 26.6, 23.8, 28.5, 27.0, 23.9, 21.9, 31.9, 26.0, 23.5)

cat("(a) Mean               :", arithmetic_mean(arr), "\n")   # 29.262
cat("(b) Median              :", median_stat(arr), "\n")       # 28
cat("(c) IQR                 :", IQR_stat(arr), "\n")           # 9.5
cat("(d) Standard deviation  :", pop_sd(arr), "\n")             # 6.0644 (population, /N)
cat("(e) Coefficient of var. :", CV_stat(arr), "\n")             # 0.2072
cat("(f) Fisher skewness g1  :", fisher_skewness(arr), "\n")    # 0.5197
cat("(g) Fisher kurtosis g2  :", fisher_kurtosis(arr), "\n")    # -0.8489
