##' Function to calculate sample size needed for variant detection assuming cross-sectional sampling
##'
##'
##' @param p_v1 variant prevalence (proportion)
##' @param prob desired probability of detection
##' @param omega probability of sequencing (or other characterization) success
##' @param c_ratio coefficient of detection ratio, calculated as the ratio of the coefficients of variant 1 to variant 2. default = 1 (no bias)
##' @return scalar of expected sample size
##'
##' @author Shirlee Wohl, Elizabeth C. Lee, Bethany L. DiPrete, and Justin Lessler
##'
##' @examples
##' vartrack_samplesize_detect_xsect(p_v1 = 0.1, prob = 0.95, omega = 0.8, c_ratio = 1)
##'
##' @family variant tracking functions
##'
##' @export


vartrack_samplesize_detect_xsect <- function(p_v1, prob, omega, c_ratio = 1) {
  if (!all(is.numeric(p_v1), p_v1 > 0 & p_v1 < 1)) stop("Variant prevalence must be numeric and between 0 and 1.")
  if (!all(is.numeric(prob), prob > 0 & prob < 1)) stop("Desired probability of detection must be numeric and between 0 and 1.")
  if (!all(is.numeric(omega), omega > 0 & omega <= 1)) stop("Sequencing success rate must be numeric and between 0 and 1.")
  if (!all(is.numeric(c_ratio), c_ratio > 0)) stop("Coefficient of detection ratio must be numeric and greater than 0")

  p_star <- varfreq_obs_freq(p_v1, c_ratio)
  n <- (log(1 - prob)) / (log(1 - p_star))
  n_samples <- n / omega
  return(n_samples)
}