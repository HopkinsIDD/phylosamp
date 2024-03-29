##' Expected number of true transmission pairs assuming single-transmission and single-linkage
##'
##' @description
##' `r lifecycle::badge('deprecated')`
##' This function calculates the expected number of true transmission pairs in a sample of size `M`.
##' The single-transmission and single-linkage method assumes the following:
##' \enumerate{
##'      \item Each case \eqn{i} is linked by transmission to only one other case \eqn{j} in the population (\eqn{N}).
##'      \item Each case \eqn{i} is linked by the linkage criteria to only one other case \eqn{j} in the sampled population (\eqn{M}).
##'      }
##'
##' @param eta scalar or vector giving the sensitivity of the linkage criteria
##' @param rho scalar or vector giving the proportion of the final outbreak size that is sampled
##' @param M scalar or vector giving the number of cases sampled
##'
##' @return scalar or vector giving the expected number of true transmission pairs in the sample
##'
##' @author John Giles, Shirlee Wohl, and Justin Lessler
##'
##' @examples
##' true_pairs_stsl(eta=0.95, rho=0.2, M=200)
##'
##' @family true_pairs
##'
##' @export

true_pairs_stsl <- function(eta, rho, M) {
    lifecycle::deprecate_soft("1.0.0", "true_pairs_stsl()", "translink_expected_links_true_stsl()")
    lifecycle::deprecate_soft("1.0.0", "true_pairs_stsl(eta)", "translink_expected_links_true_stsl(sensitivity)")

    if (!all(is.numeric(eta), eta >= 0 & eta <= 1))
        stop("eta must be numeric between 0 and 1")
    if (!all(is.numeric(rho), rho > 0 & rho <= 1))
        stop("rho must be numeric > 0 and <= 1")
    if (!all(is.numeric(M) | is.integer(M), M >= 0))
        stop("Sample size (M) must be integer or numeric greater than 0")

    (M/2) * eta * rho
}
