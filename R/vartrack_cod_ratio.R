##' Calculate the coefficient of detection ratio for two variants
##'
##' This function calculates the coefficient of detection ratio \eqn{C_{V_1}/C_{V_2}} for two variants.
##' This function assumes that variant 1 is the variant of concern.
##' This function is specific to the two-variant system.
##' Parameters not provided are assumed to be equivalent between the two variants.
##'
##' @param phi_v1 probability that a tested infection caused by variant 1 results in a positive test (sensitivity)
##' @param phi_v2 probability that a tested infection caused by variant 2 results in a positive test (sensitivity)
##' @param gamma_v1 probability that a detected infection caused by variant 1 meets some quality threshold
##' @param gamma_v2 probability that a detected infection caused by variant 2 meets some quality threshold
##' @param psi_v1 probability that an infection caused by variant 1 is asymptomatic
##' @param psi_v2 probability that an infection caused by variant 2 is asymptomatic
##' @param tau_a probability of testing an asymptomatic infection (any variant); note that this parameter is not required if psi_v1==psi_v2
##' @param tau_s probability of testing a symptomatic infection (any variant); note that this parameter is not required if psi_v1==psi_v2
##' @return scalar giving the multiplicative bias of variant 1
##'
##' @author Shirlee Wohl, Elizabeth C. Lee, Bethany L. DiPrete, and Justin Lessler
##'
##' @examples
##' vartrack_cod_ratio(phi_v1=0.975, phi_v2=0.95, gamma_v1=0.8, gamma_v2=0.6)
##'
##' @family variant tracking functions
##'
##' @export


vartrack_cod_ratio <- function(phi_v1 = 1, phi_v2 = 1, gamma_v1 = 1, gamma_v2 = 1,
    psi_v1 = 1, psi_v2 = 1, tau_a = 1, tau_s = 1) {
    if (!all(is.numeric(phi_v1), phi_v1 > 0 & phi_v1 <= 1))
        stop("Testing sensitivity must be numeric and between 0 and 1.")
    if (!all(is.numeric(phi_v2), phi_v2 > 0 & phi_v1 <= 1))
        stop("Testing sensitivity must be numeric and between 0 and 1.")
    if (!all(is.numeric(gamma_v1), gamma_v1 > 0 & gamma_v1 <= 1))
        stop("Probability of characterization success must be numeric and between 0 and 1.")
    if (!all(is.numeric(gamma_v2), gamma_v2 > 0 & gamma_v2 <= 1))
        stop("Probability of characterization success must be numeric and between 0 and 1.")
    if (!all(is.numeric(psi_v1), psi_v1 >= 0 & psi_v1 <= 1))
        stop("Asymptomatic rate must be numeric and between 0 and 1.")
    if (!all(is.numeric(psi_v2), psi_v2 >= 0 & psi_v2 <= 1))
        stop("Asymptomatic rate must be numeric and between 0 and 1.")
    if (!all(is.numeric(tau_a), tau_a > 0 & tau_a <= 1))
        stop("Testing probability must be numeric and between 0 and 1.")
    if (!all(is.numeric(tau_s), tau_s > 0 & tau_s <= 1))
        stop("Testing probability must be numeric and between 0 and 1.")

    c_v1 <- phi_v1 * gamma_v1 * (psi_v1 * tau_a + (1 - psi_v1) * tau_s)
    c_v2 <- phi_v2 * gamma_v2 * (psi_v2 * tau_a + (1 - psi_v2) * tau_s)
    return(c_v1/c_v2)
}
