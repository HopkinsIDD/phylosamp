##' Calculate false discovery rate of identifying transmission pairs in a sample
##'
##' This function calculates the false discovery rate (proportion of linked pairs that are false positives) in a sample given the sensitivity \eqn{\sensitivity}
##' and specificity \eqn{\specificity} of the linkage criteria, and sample size \eqn{M}. Assumptions about transmission and linkage (single or multiple)
##' can be specified.
##'
##' @param sensitivity scalar or vector giving the sensitivity of the linkage criteria
##' @param specificity scalar or vector giving the specificity of the linkage criteria
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
##' @return scalar or vector giving the true discovery rate
##'
##' @author John Giles, Shirlee Wohl, and Justin Lessler
##'
##' @examples
##' # The simplest case: single-transmission, single-linkage, and perfect sensitivity
##' translink_fdr(sensitivity=1, specificity=0.9, rho=0.5, M=100, assumption='stsl')
##'
##' # Multiple-transmission and imperfect sensitivity
##' translink_fdr(sensitivity=0.99, specificity=0.9, rho=1, M=50, R=1, assumption='mtsl')
##'
##' # Small outbreak, larger sampling proportion
##' translink_fdr(sensitivity=0.99, specificity=0.95, rho=1, M=50, R=1, assumption='mtml')
##'
##' # Large outbreak, small sampling proportion
##' translink_fdr(sensitivity=0.99, specificity=0.95, rho=0.5, M=1000, R=1, assumption='mtml')
##'
##' @family transmission linkage functions
##'
##' @export
##'

translink_fdr <- function(sensitivity, # sensitivity of the linkage criteria
                               specificity, # specificity of the linkage criteria
                               rho, # sampling proportion
                               M, # number of cases sampled
                               R = NULL, # effective reproductive number
                               assumption = "mtml" # assume most general case if not specified
) {
  suppressMessages(
    1 - translink_tdr(
      sensitivity = sensitivity,
      specificity = specificity,
      rho = rho,
      M = M,
      R = R,
      assumption = assumption
    )
  )
}
