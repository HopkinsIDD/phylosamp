##' Calculate expected number of true transmission pairs
##'
##' This function calculates the expected number true transmission pairs in a sample of size \code{M}.
##' Assumptions about transmission and linkage (single or multiple) can be specified.
##'
##' @param sensitivity scalar or vector giving the sensitivity of the linkage criteria
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
##' @return scalar or vector giving the expected number of true transmission pairs in the sample
##'
##' @author John Giles, Shirlee Wohl, and Justin Lessler
##'
##' @examples
##' translink_expected_links_true(sensitivity=0.99, rho=0.75, M=100, R=1)
##'
##' @family transmission linkage functions
##'
##' @export
##'

translink_expected_links_true <- function(sensitivity, # sensitivity of the linkage criteria
                       rho, # sampling proportion
                       M, # number of cases sampled
                       R = NULL, # effective reproductive number
                       assumption = "mtml" # assume most general case if not specified
) {
  if (assumption == "stsl") {
    message("Calculating expected number of links assuming single-transmission and single-linkage")
    out <- translink_expected_links_true_stsl(sensitivity = sensitivity, rho = rho, M = M)
  } else if (assumption == "mtsl") {
    message("Calculating expected number of links assuming multiple-transmission and single-linkage")
    out <- translink_expected_links_true_mtsl(sensitivity = sensitivity, rho = rho, M = M, R = R)
  } else if (assumption == "mtml") {
    message("Calculating expected number of links assuming multiple-transmission and multiple-linkage")
    out <- translink_expected_links_true_mtml(sensitivity = sensitivity, rho = rho, M = M, R = R)
  } else {
    stop("Incorrect assumption argument")
  }

  return(out)
}
