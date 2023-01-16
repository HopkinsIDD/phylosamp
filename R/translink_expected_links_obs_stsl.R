##' Expected number of observed pairs assuming single-transmission and single-linkage
##'
##' This function calculates the expected number of link pairs observed in a sample of size \code{M}.
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
##' translink_expected_links_obs_stsl(eta=1, chi=1, rho=0.5, M=100)
##'
##' translink_expected_links_obs_stsl(eta=0.99, chi=0.9, rho=1, M=50)
##'
##' translink_expected_links_obs_stsl(eta=0.99, chi=0.9, rho=0.5, M=100)
##'
##' @family transmission linkage functions
##'
##' @export

translink_expected_links_obs_stsl <- function(eta,
                           chi, # specificity of the linkage criteria
                           rho, # sampling proportion
                           M # number of cases sampled
) {
  if (!all(is.numeric(eta), eta >= 0 & eta <= 1)) stop("eta must be numeric between 0 and 1")
  if (!all(is.numeric(chi), chi >= 0 & chi <= 1)) stop("chi must be numeric between 0 and 1")
  if (!all(is.numeric(rho), rho > 0 & rho <= 1)) stop("rho must be numeric > 0 and <= 1")
  if (!all(is.numeric(M) | is.integer(M), M >= 0)) stop("Sample size (M) must be integer or numeric greater than 0")

  (M / 2) * ((eta * rho) + (rho * (1 - eta) * (1 - chi^(M - 2))) + ((1 - rho) * (1 - chi^(M - 1))))
}
