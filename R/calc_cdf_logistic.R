##' Function to calculate cumulative observed variant prevalence at time t given logistic growth (CDF)
##'
##' @param t time step number (e.g., days) at which to calculate prevalence
##' @param p0 initial variant prevalence (# introductions / population size)
##' @param r logistic growth rate
##' @param c_ratio coefficient of detection ratio, calculated as the ratio of the coefficients of variant 1 to variant 2; default = 1 (no bias)
##' @return scalar giving the cdf of variant prevalence at time t
##'
##' @author Shirlee Wohl, Elizabeth C. Lee, Bethany L. DiPrete, and Justin Lessler
##'
##' @example R/examples/
##'
##' @family VOC_loggrowth
##'
##' @export


calc_cdf_logistic <- function(t,p0,r,c_ratio=1){
  a <- (1/p0)-1
  b <- a*(1/c_ratio)
  G <- (1/r) * log(b+exp(r*t))
  return(G)
}
