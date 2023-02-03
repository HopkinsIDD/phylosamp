##' Expected number of observed pairs assuming single-transmission and single-linkage
##'
##' @description
##' `r lifecycle::badge('deprecated')`
##' This function calculates the expected number of link pairs observed in a sample of size `M`.
##' The single-transmission and single-linkage method assumes the following:
##' \enumerate{
##'      \item Each case \eqn{i} is linked by transmission to only one other case \eqn{j} in the population (\eqn{N}).
##'      \item Each case \eqn{i} is linked by the linkage criteria to only one other case \eqn{j} in the sampled population (\eqn{M}).
##'      }
##'
##' @param eta scalar or vector giving the sensitivity of the linkage criteria
##' @param chi scalar or vector giving the specificity of the linkage criteria
##' @param rho scalar or vector giving the proportion of the final outbreak size that is sampled
##' @param M scalar or vector giving the number of cases sampled
##'
##' @return scalar or vector giving the expected number of linked pairs observed in the sample
##'
##' @author John Giles, Shirlee Wohl, and Justin Lessler
##'
##' @examples
##' # perfect sensitivity and specificity
##' obs_pairs_stsl(eta=1, chi=1, rho=0.5, M=100)
##'
##' obs_pairs_stsl(eta=0.99, chi=0.9, rho=1, M=50)
##'
##' obs_pairs_stsl(eta=0.99, chi=0.9, rho=0.5, M=100)
##'
##' @family obs_pairs
##'
##' @export

obs_pairs_stsl <- function(eta, chi, rho, M) {
    lifecycle::deprecate_soft("1.0.0", "obs_pairs_stsl()", "translink_expected_links_obs_stsl()")
    lifecycle::deprecate_soft("1.0.0", "obs_pairs_stsl(eta)", "translink_expected_links_obs_stsl(sensitivity)")
    lifecycle::deprecate_soft("1.0.0", "obs_pairs_stsl(chi)", "translink_expected_links_obs_stsl(specificity)")

    if (!all(is.numeric(eta), eta >= 0 & eta <= 1))
        stop("eta must be numeric between 0 and 1")
    if (!all(is.numeric(chi), chi >= 0 & chi <= 1))
        stop("chi must be numeric between 0 and 1")
    if (!all(is.numeric(rho), rho > 0 & rho <= 1))
        stop("rho must be numeric > 0 and <= 1")
    if (!all(is.numeric(M) | is.integer(M), M >= 0))
        stop("Sample size (M) must be integer or numeric greater than 0")

    (M/2) * ((eta * rho) + (rho * (1 - eta) * (1 - chi^(M - 2))) + ((1 - rho) * (1 -
        chi^(M - 1))))
}
