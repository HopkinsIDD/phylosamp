##' Function to calculate the coefficient of detection ratio for two variants
##'
##' This function calculates the coefficient of detection ratio \eqn{C_{V_1}/C_{V_2}} for two variants.
##' This function assumes that variant 1 is the variant of concern (VOC).
##' This function is specific to the two-variant system.
##' Parameters not provided are assumed to be equivalent between the two variants.
##'
##' @param phi_v1 probability that a tested infection caused by variant 1 results in a positive test (sensitivity)
##' @param phi_v2 probability that a tested infection caused by variant 2 results in a positive test (sensitivity)
##' @param gamma_v1 probability that a detected infection caused by variant 1 meets some quality threshold
##' @param gamma_v2 probability that a detected infection caused by variant 2 meets some quality threshold
##' @param alpha_v1 probability that an infection caused by variant 1 is asymptomatic
##' @param alpha_v2 probability that an infection caused by variant 2 is asymptomatic
##' @param beta_a probability of testing an asymptomatic infection (any variant); note that this parameter is not required if alpha_v1==alpha_v2
##' @param beta_s probability of testing a symptomatic infection (any variant); note that this parameter is not required if alpha_v1==alpha_v2
##' @return scalar giving the multiplicative bias of variant 1
##'
##' @author Shirlee Wohl, Elizabeth C. Lee, Bethany L. DiPrete, and Justin Lessler
##'
##' @examples
##' calc_cod_ratio(phi_v1=0.975, phi_v2=0.95, gamma_v1=0.8, gamma_v2=0.6)
##'
##' @family VOC_bias
##'
##' @export


calc_cod_ratio <- function(phi_v1=1,phi_v2=1,gamma_v1=1,gamma_v2=1,
                           alpha_v1=1,alpha_v2=1,beta_a=1,beta_s=1) {
  if (!all(is.numeric(phi_v1), phi_v1 > 0 & phi_v1 <= 1)) stop("Testing sensitivity must be numeric and between 0 and 1.")
  if (!all(is.numeric(phi_v2), phi_v2 > 0 & phi_v1 <= 1)) stop("Testing sensitivity must be numeric and between 0 and 1.")
  if (!all(is.numeric(gamma_v1), gamma_v1 > 0 & gamma_v1 <= 1)) stop("Probability of characterization success must be numeric and between 0 and 1.")
  if (!all(is.numeric(gamma_v2), gamma_v2 > 0 & gamma_v2 <= 1)) stop("Probability of characterization success must be numeric and between 0 and 1.")
  if (!all(is.numeric(alpha_v1), alpha_v1 >= 0 & alpha_v1 <= 1)) stop("Asymptomatic rate must be numeric and between 0 and 1.")
  if (!all(is.numeric(alpha_v2), alpha_v2 >= 0 & alpha_v2 <= 1)) stop("Asymptomatic rate must be numeric and between 0 and 1.")
  if (!all(is.numeric(beta_a), beta_a > 0 & beta_a <= 1)) stop("Testing probability must be numeric and between 0 and 1.")
  if (!all(is.numeric(beta_s), beta_s > 0 & beta_s <= 1)) stop("Testing probability must be numeric and between 0 and 1.")

  c_v1 <- phi_v1 * gamma_v1 * ( alpha_v1*beta_a + (1-alpha_v1)*beta_s )
  c_v2 <- phi_v2 * gamma_v2 * ( alpha_v2*beta_a + (1-alpha_v2)*beta_s )
  return(c_v1 / c_v2)
}