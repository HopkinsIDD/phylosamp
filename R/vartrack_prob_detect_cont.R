##' Calculate probability of detecting a variant given a per-timestep sample size assuming periodic sampling
##'
##' This function calculates the probability of detecting the presence of a variant
##' given a sample size and either a desired maximum time until detection
##' or a desired prevalence by which to detect the variant by.
##' It assumes a periodic sampling strategy, where samples are collected at regular intervals (time steps).
##'
##' @param n per-timestep (e.g., per day) sample size
##' @param t time step number (e.g., days) at which variant should be detected by. Default = NA (either \code{'t'} or \code{'p_v1'} should be provided, not both)
##' @param p_v1 the desired prevalence to detect a variant by. Default = NA (either \code{'t'} or \code{'p_v1'} should be provided, not both)
##' @param omega probability of sequencing (or other characterization) success
##' @param p0 initial variant prevalence (# introductions / infected population size)
##' @param r logistic growth rate
##' @param c_ratio coefficient of detection ratio, calculated as the ratio of the coefficients of variant 1 to variant 2. Default = 1 (no bias)
##' @return scalar of detection probability
##'
##' @author Shirlee Wohl, Elizabeth C. Lee, Bethany L. DiPrete, and Justin Lessler
##'
##' @examples
##' vartrack_prob_detect_cont(n = 158, t = 30, omega = 0.8, p0 = 1/10000, r = 0.1, c_ratio = 1)
##'
##' @family variant detection functions
##' @family variant tracking functions
##'
##' @export


vartrack_prob_detect_cont <- function(n, t = NA, p_v1 = NA, omega, p0, r, c_ratio = 1) {
  if (is.na(t) & is.na(p_v1)) stop("Either time to detection or detection prevalence must be provided.")
  if (!is.na(t) & !is.na(p_v1)) stop("Please provide either a desired time to detection OR desired prevalence.")
  
  if (!is.na(t)) {
    if (!is.numeric(t)) stop("Time step must be numeric.")
  }
  
  if (!is.na(p_v1)) {
    if (!all(is.numeric(p_v1), p_v1 > 0 & p_v1 < 1)) stop("Variant prevalence must be numeric and between 0 and 1.")
  }
  
  if (!all(is.numeric(n), n > 0)) stop("Number of samples per timestep must be numeric and greater than zero.")
  if (!all(is.numeric(p0), p0 > 0 & p0 < 1)) stop("Initial variant prevalence must be numeric and between 0 and 1.")
  if (!all(is.numeric(omega), omega > 0 & omega <= 1)) stop("Sequencing success rate must be numeric and between 0 and 1.")
  if (!all(is.numeric(r), r != 0)) stop("Growth rate must be numeric and non-zero.")
  if (!all(is.numeric(c_ratio), c_ratio > 0)) stop("Coefficient of detection ratio must be numeric and greater than 0.")
  
  if (is.na(t)) {
    a <- (1/p0)-1
    t <- ceiling(-(1/r) * log( ((1/p_v1)-1) / a ))
  }
  
  n_star <- n * omega
  
  prob <- 1 - exp(-n_star * (varfreq_cdf_logistic(t, p0, r, c_ratio) - varfreq_cdf_logistic(0, p0, r, c_ratio)))
  return(prob)
}
