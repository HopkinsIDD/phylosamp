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

  if(!all(is.numeric(p_v1), p_v1>0 & p_v1<1)) stop('Variant prevalence must be numeric and between 0 and 1.')
  if(!all(is.numeric(prob), prob>0 & prob<1)) stop('Desired probability of detection must be numeric and between 0 and 1.')
  if(!all(is.numeric(precision), precision>0 & precision<1)) stop('Desired precision must be numeric and between 0 and 1.')
  if(!all(is.numeric(c_ratio), c_ratio>0)) stop('Coefficient of detection ratio must be numeric and greater than 0')

  p_star <- calc_observed_freq(p_v1,c_ratio)
  Z <- qnorm(1-((1-prob)/2))
  n = ((Z^2)*p_star*(1-p_star)) / ((p_star*precision)^2)
  return(n)
}
