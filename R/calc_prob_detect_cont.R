##' Function to calculate probability of detecting a variant given a per-timestep sample size assuming periodic sampling
##'
##' @param n per-timestep (e.g., per day) sample size
##' @param t time step number (e.g., days) at which variant should be detected by
##' @param p0 initial variant prevalence (# introductions / population size)
##' @param r logistic growth rate
##' @param c_ratio coefficient of detection ratio, calculated as the ratio of the coefficients of variant 1 to variant 2. default = 1 (no bias)
##' @return scalar of detection probability
##'
##' @author Shirlee Wohl, Elizabeth C. Lee, Bethany L. DiPrete, and Justin Lessler
##'
##' @examples
##' calc_prob_detect_cont(n = 158, t = 30, p0 = 1/10000, r = 0.1, c_ratio = 1)
##'
##' @family VOC_detection
##'
##' @export


calc_prob_detect_cont <- function(n, t, p0, r, c_ratio = 1) {
  if (!all(is.numeric(n), n > 0)) stop("Number of samples per timestep must be numeric and greater than zero.")
  if (!is.numeric(t)) stop("Time step must be numeric.")
  if (!all(is.numeric(p0), p0 > 0 & p0 < 1)) stop("Initial variant prevalence must be numeric and between 0 and 1.")
  if (!all(is.numeric(r), r != 0)) stop("Growth rate must be numeric and non-zero.")
  if (!all(is.numeric(c_ratio), c_ratio > 0)) stop("Coefficient of detection ratio must be numeric and greater than 0")

  prob <- 1 - exp(-n * (calc_cdf_logistic(t, p0, r, c_ratio) - calc_cdf_logistic(0, p0, r, c_ratio)))
  return(prob)
}
