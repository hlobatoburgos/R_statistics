# ============================================================
# DESCRIPTIVE STATISTICS - R FUNCTIONS
# Chapter 1: Descriptive Statistics of one variable
# ============================================================


# ============================================================
# 1. MEASURES OF CENTRAL TENDENCY
# ============================================================

# Weighted mean
my_weighted_mean <- function(x, w) {
  sum(x * w) / sum(w)
}

# Quadratic mean / RMS
my_rms <- function(x) {
  sqrt(sum(x^2) / length(x))
}


# ============================================================
# 2. MEASURES OF POSITION
# ============================================================

# Built into R:
# mean(x)
# median(x)
# quantile(x)


# ============================================================
# 3. MEASURES OF DISPERSION
# ============================================================

# Mean deviation about p
my_mean_deviation <- function(x, p) {
  sum(abs(x - p)) / length(x)
}

# Mean squared error about p
my_mse <- function(x, p) {
  sum((x - p)^2) / length(x)
}

# Population variance
my_variance <- function(x) {
  mu <- mean(x)
  sum((x - mu)^2) / length(x)
}

# Population standard deviation
my_sd <- function(x) {
  sqrt(my_variance(x))
}

# Sample variance
my_sample_variance <- function(x) {
  xbar <- mean(x)
  sum((x - xbar)^2) / (length(x) - 1)
}

# Sample standard deviation
my_sample_sd <- function(x) {
  sqrt(my_sample_variance(x))
}

# Coefficient of variation
my_cv <- function(x) {
  my_sd(x) / abs(mean(x))
}

# Coefficient of variation as percentage
my_cv_percent <- function(x) {
  100 * my_cv(x)
}


# ============================================================
# 4. STANDARDISATION
# ============================================================

# Standardised variable
my_z <- function(x, mu, sigma) {
  (x - mu) / sigma
}


# ============================================================
# 5. MOMENTS
# ============================================================

# Moment of order r with respect to c
my_moment <- function(x, r, c) {
  sum((x - c)^r) / length(x)
}

# Ordinary moment of order r
my_ordinary_moment <- function(x, r) {
  sum(x^r) / length(x)
}
#RELATION BETWEEN ORDINAY MOMENT AND CENTRAL MOMENT: 
#|| m1 = mu || m2 = σ ^2 + mu^2 || m_3 = mu_3 + 3*m_2*mu - 2*mu^3 || mu_4 = m_4 - 4*m_3*mu + 6*m_2*mu^2 - 3*mu^4 ||
# Central moment of order r
my_central_moment <- function(x, r) {
  mu <- mean(x)
  sum((x - mu)^r) / length(x)
}


# ============================================================
# 6. MEASURES OF SHAPE
# ============================================================

# Fisher skewness
my_fisher_skew <- function(x) {
  mu3 <- my_central_moment(x, 3)
  sigma <- my_sd(x)
  
  mu3 / sigma^3
}

# Fisher kurtosis
my_kurtosis <- function(x) {
  mu4 <- my_central_moment(x, 4)
  sigma <- my_sd(x)
  
  mu4 / sigma^4 - 3
}


# ============================================================
# 7. FREQUENCY DATA
# ============================================================

# We will add these next.


# ============================================================
# 8. GROUPED DATA
# ============================================================

# We will add these next.


