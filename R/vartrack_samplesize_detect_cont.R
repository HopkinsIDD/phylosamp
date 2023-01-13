##' Function to calculate sample size needed for variant detection assuming periodic sampling
##'
##' @param prob desired probability of detection
##' @param t time step number (e.g., days) at which variant should be detected by
##' @param p_v1 the desired prevalence to detect a variant by
##' @param omega the sequencing success rate
##' @param p0 initial variant prevalence (# introductions / infected population size)
##' @param r logistic growth rate
##' @param c_ratio coefficient of detection ratio, calculated as the ratio of the coefficients of variant 1 to variant 2. default = 1 (no bias)
##' @return scalar of expected sample size
##'
##' @author Shirlee Wohl, Elizabeth C. Lee, Bethany L. DiPrete, and Justin Lessler
##'
##' @examples
##' vartrack_samplesize_detect_cont(prob = 0.95, t = 30, omega = 0.8, p0 = 1/10000, r = 0.1, c_ratio = 1)
##'
##' @family variant tracking functions
##'
##' @export


vartrack_samplesize_detect_cont <- function(prob, t = NA, p_v1 = NA, omega, p0, r, c_ratio = 1) {
  if (is.na(t) & is.na(p_v1)) stop("Either time to detection or detection prevalence must be provided.")
  if (!is.na(t) & !is.na(p_v1)) stop("Please provide either a desired time to detection OR desired prevalence.")
  
  if (!is.na(t)) {
    if (!is.numeric(t)) stop("Time step must be numeric.")
  }
  
  if (!is.na(p_v1)) {
    if (!all(is.numeric(p_v1), p_v1 > 0 & p_v1 < 1)) stop("Variant prevalence must be numeric and between 0 and 1.")
  }
  
  if (!all(is.numeric(prob), prob > 0 & prob < 1)) stop("Desired probability of detection must be numeric and between 0 and 1.")
  if (!all(is.numeric(p0), p0 > 0 & p0 < 1)) stop("Initial variant prevalence must be numeric and between 0 and 1.")
  if (!all(is.numeric(omega), omega > 0 & omega <= 1)) stop("Sequencing success rate must be numeric and between 0 and 1.")
  if (!all(is.numeric(r), r != 0)) stop("Growth rate must be numeric and non-zero.")
  if (!all(is.numeric(c_ratio), c_ratio > 0)) stop("Coefficient of detection ratio must be numeric and greater than 0.")

  if (is.na(t)) {
    a <- (1/p0)-1
    t <- ceiling(-(1/r) * log( ((1/p_v1)-1) / a ))
  }
  
  n <- -(log(1 - prob)) / (varfreq_cdf_logistic(t, p0, r, c_ratio) - varfreq_cdf_logistic(0, p0, r, c_ratio))
  n_samples <- n / omega
  return(n_samples)
}