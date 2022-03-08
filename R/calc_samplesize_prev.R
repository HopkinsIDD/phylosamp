##' Function to calculate sample size needed for variant prevalence estimation under cross-sectional sampling
##'
##' @param p_v1 variant prevalence (proportion)
##' @param prob desired confidence in variant prevalence estimate
##' @param precision desired precision in variant prevalence estimate
##' @param c_ratio coefficient of detection ratio, calculated as the ratio of the coefficients of variant 1 to variant 2. default = 1 (no bias)
##' @return scalar of sample size
##'
##' @author Shirlee Wohl, Elizabeth C. Lee, Bethany L. DiPrete, and Justin Lessler
##'
##' @example R/examples/
##'
##' @family VOC_prevalence
##'
##' @export


calc_samplesize_prev <- function(p_v1,prob,precision,c_ratio){
  p_star <- calc_observed_freq(p_v1,c_ratio)
  Z <- qnorm(1-((1-prob)/2))
  n = ((Z^2)*p_star*(1-p_star)) / ((p_star*precision)^2)
  return(n)
}
