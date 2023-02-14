##' Calculate expected number of observed pairs assuming single-transmission and single-linkage
##'
##' This function calculates the expected number of link pairs observed in a sample of size `M`.
##' The single-transmission and single-linkage method assumes the following:
##' \enumerate{
##'      \item Each case \eqn{i} is linked by transmission to only one other case \eqn{j} in the population (\eqn{N}).
##'      \item Each case \eqn{i} is linked by the linkage criteria to only one other case \eqn{j} in the sampled population (\eqn{M}).
##'      }
##'
##' @param sensitivity scalar or vector giving the sensitivity of the linkage criteria
##' @param specificity scalar or vector giving the specificity of the linkage criteria
##' @param rho scalar or vector giving the proportion of the final outbreak size that is sampled
##' @param M scalar or vector giving the number of cases sampled
##'
##' @return scalar or vector giving the expected number of linked pairs observed in the sample
##'
##' @author John Giles, Shirlee Wohl, and Justin Lessler
##'
##' @examples
##' # perfect sensitivity and specificity
##' translink_expected_links_obs_stsl(sensitivity=1, specificity=1, rho=0.5, M=100)
##'
##' translink_expected_links_obs_stsl(sensitivity=0.99, specificity=0.9, rho=1, M=50)
##'
##' translink_expected_links_obs_stsl(sensitivity=0.99, specificity=0.9, rho=0.5, M=100)
##'
##' @family transmission linkage functions
##'
##' @export

translink_expected_links_obs_stsl <- function(sensitivity, specificity, rho, M) {
    if (!all(is.numeric(sensitivity), sensitivity >= 0 & sensitivity <= 1))
        stop("sensitivity must be numeric between 0 and 1")
    if (!all(is.numeric(specificity), specificity >= 0 & specificity <= 1))
        stop("specificity must be numeric between 0 and 1")
    if (!all(is.numeric(rho), rho > 0 & rho <= 1))
        stop("rho must be numeric > 0 and <= 1")
    if (!all(is.numeric(M) | is.integer(M), M >= 0))
        stop("Sample size (M) must be integer or numeric greater than 0")

    (M/2) * ((sensitivity * rho) + (rho * (1 - sensitivity) * (1 - specificity^(M -
        2))) + ((1 - rho) * (1 - specificity^(M - 1))))
}
