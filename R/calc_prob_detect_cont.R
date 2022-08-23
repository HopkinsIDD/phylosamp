##' Function to calculate probability of detecting a variant given a per-timestep sample size assuming periodic sampling
##'
##' @param n per-timestep (e.g., per day) sample size
##' @param t time step number (e.g., days) at which variant should be detected by
##' @param p0 initial variant prevalence (# introductions / population size)
##' @param r logistic growth rate
##' @param c_ratio coefficient of detection ratio, calculated as the ratio of the coefficients of variant 1 to variant 2. default = 1 (no bias)
##' @return scalar of detection probability
##'
##' @author Shirlee Wohl, Elizabeth C. Lee, Bethany L. DiPrete, and Justin Lessler
##'
##' @example R/examples/
##'
##' @family VOC_detection
##'
##' @export


calc_prob_detect_cont <- function(n,t,p0,r,c_ratio=1){
  prob <- 1-exp(-n*(calc_cdf_logistic(t,p0,r,c_ratio)-calc_cdf_logistic(0,p0,r,c_ratio)))
  return(prob)
}