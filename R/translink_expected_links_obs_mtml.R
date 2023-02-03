##' Calculate expected number of observed pairs assuming multiple-transmission and multiple-linkage
##'
##' This function calculates the expected number of pairs observed in a sample of size \code{M}.
##' The multiple-transmission and multiple-linkage method assumes the following:
##' \enumerate{
##'      \item Each case \eqn{i} is, on average, the infector of \code{R} cases in the population (\eqn{N})
##'      \item Each case \eqn{i} is allowed to be linked by the linkage criteria to multiple cases \eqn{j} in the sampled population (\eqn{M}).
##'      \item Linkage events are independent of one another (i.e, linkage of case \eqn{i} to case \eqn{j} has no bearing on linkage of case \eqn{i} to any other sample).
##'      }
##'
##' @param sensitivity scalar or vector giving the sensitivity of the linkage criteria
##' @param specificity scalar or vector giving the specificity of the linkage criteria
##' @param rho scalar or vector giving the proportion of the final outbreak size that is sampled
##' @param M scalar or vector giving the number of cases sampled
##' @param R scalar or vector giving the effective reproductive number of the pathogen
##'
##' @return scalar or vector giving the expected number of linked pairs observed in the sample
##'
##' @author John Giles, Shirlee Wohl and Justin Lessler
##'
##' @examples
##' # Perfect sensitivity and specificity
##' translink_expected_links_obs_mtml(sensitivity=1, specificity=1, rho=0.5, M=100, R=1)
##'
##' translink_expected_links_obs_mtml(sensitivity=0.99, specificity=0.9, rho=1, M=50, R=1)
##'
##' translink_expected_links_obs_mtml(sensitivity=0.99, specificity=0.9, rho=0.5, M=100, R=1)
##'
##' @family transmission linkage functions
##'
##' @export
##'
##'

translink_expected_links_obs_mtml <- function(specificity, sensitivity, rho, M, R) {
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

    (M/2) * ((sensitivity * rho * (R + 1)) + ((1 - specificity) * (M - 1 - (rho *
        (R + 1)))))
}
