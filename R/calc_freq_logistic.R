##' Function to calculate observed variant prevalence at time t given logistic growth (PDF)
##'
##'
##' @param t time step number (e.g., days) at which to calculate prevalence
##' @param p.0 initial variant prevalence (# introductions / population size)
##' @param r logistic growth rate
##' @param c_ratio coefficient of detection ratio, calculated as the ratio of the coefficients of variant 1 to variant 2; default = 1 (no bias)
##' @return scalar giving the variant prevalence at time t
##'
##' @author Shirlee Wohl, Elizabeth C. Lee, Bethany L. DiPrete, and Justin Lessler
##'
##' @example R/examples/
##'
##' @family VOC_loggrowth
##'
##' @export


calc_freq_logistic <- function(t,p0,r,c_ratio=1){
  a <- (1/p0)-1
  b <- a*(1/c_ratio)
  g <- 1 / (1 + (b*exp(-r*t)))
  return(g)
}