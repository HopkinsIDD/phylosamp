##' Function to calculate observed variant prevalence
##'
##' This function calculates the observed variant prevalence in \eqn{H_v1} from the coefficient of detection ratio and the actual variant prevalence \eqn{N_v1}.
##' This function assumes that variant 1 is the variant of concern (VOC).
##' This function is specific to the two-variant system.
##'
##' @param p_v1 actual variant prevalence (proportion)
##' @param c_ratio coefficient of detection ratio, calculated as the ratio of the coefficients of variant 1 to variant 2
##' @return scalar of observed prevalence of variant 1
##'
##' @author Shirlee Wohl, Elizabeth C. Lee, Bethany L. DiPrete, and Justin Lessler
##'
##' @examples
##' varfreq_obs_freq(p_v1 = 0.1, c_ratio = 1.1)
##'
##' @family variant frequency functions
##'
##' @export


varfreq_obs_freq <- function(p_v1, c_ratio) {
  if (!all(is.numeric(p_v1), p_v1 > 0 & p_v1 < 1)) stop("Variant prevalence must be numeric and between 0 and 1.")
  if (!all(is.numeric(c_ratio), c_ratio > 0)) stop("Coefficient of detection ratio must be numeric and greater than 0")

  obs <- p_v1 / (p_v1 + (1 / c_ratio) * (1 - p_v1))
  return(obs)
}
