##' Function to calculate sample size needed for variant detection assuming periodic sampling
##'
##' @param prob desired probability of detection
##' @param t time step number (e.g., days) at which variant should be detected by
##' @param p0 initial variant prevalence (# introductions / population size)
##' @param r logistic growth rate
##' @param c_ratio coefficient of detection ratio, calculated as the ratio of the coefficients of variant 1 to variant 2. default = 1 (no bias)
##' @return scalar of expected sample size
##'
##' @author Shirlee Wohl, Elizabeth C. Lee, Bethany L. DiPrete, and Justin Lessler
##'
##' @example R/examples/
##'
##' @family VOC_detection
##'
##' @export


calc_samplesize_detect_cont <- function(prob,t,p0,r,c_ratio=1){

  if(!all(is.numeric(prob), prob>0 & prob<1)) stop('Desired probability of detection must be numeric and between 0 and 1.')
    if(!is.numeric(t)) stop('Time step must be numeric.')
  if(!all(is.numeric(p0), p0>0 & p0<1)) stop('Initial variant prevalence must be numeric and between 0 and 1.')
  if(!all(is.numeric(r), r!=0)) stop('Growth rate must be numeric and non-zero.')
  if(!all(is.numeric(c_ratio), c_ratio>0)) stop('Coefficient of detection ratio must be numeric and greater than 0')

  n = -(log(1-prob)) / (calc_cdf_logistic(t,p0,r,c_ratio)-calc_cdf_logistic(0,p0,r,c_ratio))
  return(n)
}