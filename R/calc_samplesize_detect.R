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
  p_star <- calc_observed_freq(p_v1,c_ratio)
  n = (log(1-prob)) / (log(1-p_star))
  return(n)
}