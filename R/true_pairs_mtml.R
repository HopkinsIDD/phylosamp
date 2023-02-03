##' Expected number of true transmission pairs assuming multiple-transmission and multiple-linkage
##'
##' This function calculates the expected number of true transmission pairs in a sample of size \code{M}.
##' The multiple-transmission and multiple-linkage method assumes the following:
##' \enumerate{
##'      \item Each case \eqn{i} is, on average, the infector of \code{R} cases in the population (\eqn{N})
##'      \item Each case \eqn{i} is allowed to be linked by the linkage criteria to multiple cases \eqn{j} in the sampled population (\eqn{M}).
##'      \item Linkage events are independent of one another (i.e, linkage of case \eqn{i} to case \eqn{j} has no bearing on linkage of case \eqn{i} to any other sample).
##'      }
##'
##' @param eta scalar or vector giving the sensitivity of the linkage criteria
##' @param rho scalar or vector giving the proportion of the final outbreak size that is sampled
##' @param M scalar or vector giving the number of cases sampled
##' @param R scalar or vector giving the effective reproductive number of the pathogen
##'
##' @return scalar or vector giving the expected number of true transmission pairs in the sample
##'
##' @author John Giles, Shirlee Wohl and Justin Lessler
##'
##' @examples
##' true_pairs_mtml(eta=0.95, rho=0.2, M=1000, R=1)
##'
##' @family true_pairs
##'
##' @export
##'

true_pairs_mtml <- function(eta, rho, M, R) {
    lifecyle::deprecate_soft("1.0.0", "true_pairs_mtml()", "translink_expected_links_true_mtml()")
    lifecyle::deprecate_soft("1.0.0", "true_pairs_mtml(eta)", "translink_expected_links_true_mtml(sensitivity)")

    if (!all(is.numeric(eta), eta >= 0 & eta <= 1))
        stop("eta must be numeric between 0 and 1")
    if (!all(is.numeric(rho), rho > 0 & rho <= 1))
        stop("rho must be numeric > 0 and <= 1")
    if (!all(is.numeric(M) | is.integer(M), M >= 0))
        stop("Sample size (M) must be integer or numeric greater than 0")
    if (!all(is.numeric(R), R > 0))
        stop("Reproductive number (R) must be numeric greater than 0")
    if (!all(is.numeric(R), R <= 1))
        warning("Reproductive number (R) is usually less than 1 for finite outbreaks")

    (M * rho * (R + 1) * eta)/2
}
