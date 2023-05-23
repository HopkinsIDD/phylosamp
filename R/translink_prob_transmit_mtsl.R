##' Calculate probability of transmission assuming multiple-transmission and single-linkage
##'
##' This function calculates the probability that two cases are linked by direct transmission
##' given that they have been linked by phylogenetic criteria. The multiple-transmission and single-linkage method assumes the following:
##' \enumerate{
##'      \item Each case \eqn{i} is, on average, the infector of `R` cases in the population (\eqn{N})
##'      \item Each case \eqn{i} is allowed to be linked by the linkage criteria to only one other case \eqn{j} in the sampled population (\eqn{M}).
##'      }
##'
##' @param sensitivity scalar or vector giving the sensitivity of the linkage criteria
##' @param specificity scalar or vector giving the specificity of the linkage criteria
##' @param rho scalar or vector giving the proportion of the final outbreak size that is sampled
##' @param M scalar or vector giving the number of cases sampled
##' @param R scalar or vector giving the effective reproductive number of the pathogen
##'
##' @return scalar or vector giving the probability of transmission between two cases given linkage by phylogenetic criteria
##'
##' @author John Giles, Shirlee Wohl, and Justin Lessler
##'
##' @examples
##' # Perfect sensitivity and specificity
##' translink_prob_transmit_mtsl(sensitivity=1, specificity=1, rho=0.5, M=100, R=1)
##'
##' translink_prob_transmit_mtsl(sensitivity=0.99, specificity=0.9, rho=1, M=50, R=1)
##'
##' translink_prob_transmit_mtsl(sensitivity=0.99, specificity=0.9, rho=0.5, M=100, R=1)
##'
##' @family transmission linkage functions
##'
##' @export
##'

translink_prob_transmit_mtsl <- function(specificity, sensitivity, rho, M, R) {
    if (!all(is.numeric(sensitivity), sensitivity >= 0 & sensitivity <= 1))
        stop("sensitivity must be numeric between 0 and 1")
    if (!all(is.numeric(specificity), specificity >= 0 & specificity <= 1))
        stop("specificity must be numeric between 0 and 1")
    if (!all(is.numeric(rho), rho > 0 & rho <= 1))
        stop("rho must be numeric > 0 and <= 1")
    if (!all(is.numeric(M) | is.integer(M), M >= 0))
        stop("Sample size (M) must be integer or numeric greater than 0")
    if (!all(is.numeric(R), R > 0))
        stop("Reproductive number (R) must be numeric greater than 0")
    if (!all(is.numeric(R), R <= 1))
        warning("Reproductive number (R) is usually less than 1 for finite outbreaks")

    (1 - exp(-rho * sensitivity * (R + 1)))/(1 - ((specificity^(M - 1)) * exp(rho *
        (R + 1) * ((1 - sensitivity)/specificity - 1))))
}
