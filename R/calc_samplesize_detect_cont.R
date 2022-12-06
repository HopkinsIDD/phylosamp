##' Function to calculate sample size needed for variant detection assuming periodic sampling
##'
##' @param prob desired probability of detection
##' @param t time step number (e.g., days) at which variant should be detected by
##' @param p_detect the desired prevalence to detect a variant by
##' @param p0 initial variant prevalence (# introductions / population size)
##' @param r logistic growth rate
##' @param c_ratio coefficient of detection ratio, calculated as the ratio of the coefficients of variant 1 to variant 2. default = 1 (no bias)
##' @return scalar of expected sample size
##'
##' @author Shirlee Wohl, Elizabeth C. Lee, Bethany L. DiPrete, and Justin Lessler
##'
##' @examples
##' calc_samplesize_detect_cont(prob = 0.95, t = 30, p0 = 1/10000, r = 0.1, c_ratio = 1)
##'
##' @family VOC_detection
##'
##' @export


calc_samplesize_detect_cont <- function(prob, t = NA, p_detect = NA, p0, r, c_ratio = 1) {
  if (is.na(t) & is.na(p_detect)) stop("Either time to detection or detection prevalence must be provided.")
  if (!is.na(t) & !is.na(p_detect)) stop("Please provide either a desired time to detection OR desired prevalence.")
  
  if (!is.na(t)) {
    if (!is.numeric(t)) stop("Time step must be numeric.")
  }
  
  if (!is.na(p_detect)) {
    if (!all(is.numeric(p_detect), p_detect > 0 & p_detect < 1)) stop("Variant prevalence must be numeric and between 0 and 1.")
  }
  
  if (!all(is.numeric(prob), prob > 0 & prob < 1)) stop("Desired probability of detection must be numeric and between 0 and 1.")
  if (!all(is.numeric(p0), p0 > 0 & p0 < 1)) stop("Initial variant prevalence must be numeric and between 0 and 1.")
  if (!all(is.numeric(r), r != 0)) stop("Growth rate must be numeric and non-zero.")
  if (!all(is.numeric(c_ratio), c_ratio > 0)) stop("Coefficient of detection ratio must be numeric and greater than 0.")

  if (is.na(t)) {
    a <- (1/p0)-1
    t <- ceiling(-(1/r) * log( ((1/p_detect)-1) / a ))
  }
  
  n <- -(log(1 - prob)) / (calc_cdf_logistic(t, p0, r, c_ratio) - calc_cdf_logistic(0, p0, r, c_ratio))
  return(n)
}
