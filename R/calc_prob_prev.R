##' Function to calculate confidence in a variant estimate assuming cross-sectional sampling
##'
##'
##' @param p_v1 variant prevalence (proportion)
##' @param n sample size
##' @param omega probability of sequencing (or other characterization) success
##' @param precision desired precision in variant prevalence estimate
##' @param c_ratio coefficient of detection ratio, calculated as the ratio of the coefficients of variant 1 to variant 2
##' @return scalar of expected sample size
##'
##' @author Shirlee Wohl, Elizabeth C. Lee, Bethany L. DiPrete, and Justin Lessler
##'
##' @examples
##' calc_prob_prev(p_v1 = 0.1, n = 200, omega = 0.8, c_ratio = 1)
##'
##' @family VOC_detection
##'
##' @export


calc_prob_prev <- function(p_v1, n, omega, precision, c_ratio) {
  if (!all(is.numeric(p_v1), p_v1 > 0 & p_v1 < 1)) stop("Variant prevalence must be numeric and between 0 and 1.")
  if (!all(is.numeric(n), n > 0)) stop("Sample size must be numeric and greater than 0.")
  if (!all(is.numeric(omega), omega > 0 & omega <= 1)) stop("Probability of characterization success must be numeric and between 0 and 1.")
  if (!all(is.numeric(precision), precision > 0 & precision < 1)) stop("Desired precision must be numeric and between 0 and 1.")
  if (!all(is.numeric(c_ratio), c_ratio > 0)) stop("Coefficient of detection ratio must be numeric and greater than 0.")

  p_star <- calc_observed_freq(p_v1, c_ratio)
  n_star <- n * omega
  z <- sqrt( (n_star * ((p_star*precision)^2) ) / (p_star * (1-p_star)) )
  prob <- 1 - (2*(1-pnorm(z)))
  return(prob)
}
