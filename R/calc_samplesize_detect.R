##' Function to calculate sample size needed for variant detection assuming cross-sectional sampling
##'
##'
##' @param p_v1 variant prevalence (proportion)
##' @param prob desired probability of detection
##' @param c_ratio coefficient of detection ratio, calculated as the ratio of the coefficients of variant 1 to variant 2
##' @return scalar of expected sample size
##'
##' @author Shirlee Wohl, Elizabeth C. Lee, Bethany L. DiPrete, and Justin Lessler
##'
##' @example R/examples/
##'
##' @family VOC_detection
##'
##' @export


calc_samplesize_detect <- function(p_v1,prob,c_ratio){

  if(!all(is.numeric(p_v1), p_v1>0 & p_v1<1)) stop('Variant prevalence must be numeric and between 0 and 1.')
  if(!all(is.numeric(prob), prob>0 & prob<1)) stop('Desired probability of detection must be numeric and between 0 and 1.')
  if(!all(is.numeric(c_ratio), c_ratio>0)) stop('Coefficient of detection ratio must be numeric and greater than 0')

  p_star <- calc_observed_freq(p_v1,c_ratio)
  n = (log(1-prob)) / (log(1-p_star))
  return(n)
}