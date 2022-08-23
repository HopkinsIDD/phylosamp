##' Function to calculate multiplicative bias (observed / actual) in variant prevalence
##'
##' This function calculates the multiplicative bias of the observed variant proportion in \eqn{H_v1} relative to the actual variant proportion in \eqn{N_v1}.
##' This function assumes that variant 1 is the variant of concern (VOC).
##' This function is specific to the two-variant system.
##'
##' @param p_v1 actual variant prevalence (proportion)
##' @param c_ratio coefficient of detection ratio, calculated as the ratio of the coefficients of variant 1 to variant 2
##' @return scalar giving the multiplicative bias of variant 1
##'
##' @author Shirlee Wohl, Elizabeth C. Lee, Bethany L. DiPrete, and Justin Lessler
##'
##' @example R/examples/
##'
##' @family VOC_bias
##'
##' @export


calc_expected_mbias <- function(p_v1,c_ratio){

  if(!all(is.numeric(p_v1), p_v1>0 & p_v1<1)) stop('Variant prevalence must numeric and between 0 and 1.')
  if(!all(is.numeric(c_ratio), c_ratio!=0)) stop('Coefficient of detection ratio must be numeric and non-zero')

  bias <- p_v1 + ((1/c_ratio) * (1 - p_v1))
  return(1/bias)
}