##' Calculate expected number of transmission links in a sample
##'
##' This function calculates the expected number of observed pairs in the sample that are linked by the linkage criteria. The function requires the sensitivity \eqn{\eta}
##' and specificity \eqn{\chi} of the linkage criteria, and sample size \eqn{M}. Assumptions about transmission and linkage (single or multiple)
##' can be specified.
##'
##' @param eta scalar or vector giving the sensitivity of the linkage criteria
##' @param chi scalar or vector giving the specificity of the linkage criteria
##' @param rho scalar or vector giving the proportion of the final outbreak size that is sampled
##' @param M scalar or vector giving the number of cases sampled
##' @param R scalar or vector giving the effective reproductive number of the pathogen (default=NULL)
##' @param assumption a character vector indicating which assumptions about transmission and linkage criteria. Default = \code{'mtml'}. Accepted arguments are:
##' \enumerate{
##'      \item \code{'stsl'} for the single-transmission single-linkage assumption.
##'      \item \code{'mtsl'} for the multiple-transmission single-linkage assumption.
##'      \item \code{'mtml'} for the multiple-transmission multiple-linkage assumption.
##'      }
##'
##' @return scalar or vector giving the expected number of observed links in the sample
##'
##' @author John Giles, Shirlee Wohl, and Justin Lessler
##'
##' @examples
##' # The simplest case: single-transmission, single-linkage, and perfect sensitivity
##' translink_expected_links_obs(eta=1, chi=0.9, rho=0.5, M=100, assumption='stsl')
##'
##' # Multiple-transmission and imperfect sensitivity
##' translink_expected_links_obs(eta=0.99, chi=0.9, rho=1, M=50, R=1, assumption='mtsl')
##'
##' # Small outbreak, larger sampling proportion
##' translink_expected_links_obs(eta=0.99, chi=0.95, rho=1, M=50, R=1, assumption='mtml')
##'
##' # Large outbreak, small sampling proportion
##' translink_expected_links_obs(eta=0.99, chi=0.95, rho=0.05, M=1000, R=1, assumption='mtml')
##'
##' @family transmission linkage functions
##'
##' @export
##'

translink_expected_links_obs <- function(eta, # sensitivity of the linkage criteria
                      chi, # specificity of the linkage criteria
                      rho, # sampling proportion
                      M, # number of cases sampled
                      R = NULL, # effective reproductive number
                      assumption = "mtml" # assume most general case if not specified
) {
  if (assumption == "stsl") {
    message("Calculating expected number of links assuming single-transmission and single-linkage")
    out <- translink_expected_links_obs_stsl(eta = eta, chi = chi, rho = rho, M = M)
  } else if (assumption == "mtsl") {
    message("Calculating expected number of links assuming multiple-transmission and single-linkage")
    out <- translink_expected_links_obs_mtsl(eta = eta, chi = chi, rho = rho, M = M, R = R)
  } else if (assumption == "mtml") {
    message("Calculating expected number of links assuming multiple-transmission and multiple-linkage")
    out <- translink_expected_links_obs_mtml(eta = eta, chi = chi, rho = rho, M = M, R = R)
  } else {
    stop("Incorrect assumption argument")
  }

  return(out)
}
