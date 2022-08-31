##' Calculate false discovery rate of a sample
##'
##' This function calculates the false discovery rate (proportion of linked pairs that are false positives) in a sample given the sensitivity \eqn{\eta}
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
##'      \item \code{'stsl'} for the single-transmission single-linkage assumption (\code{\link{prob_trans_stsl}}).
##'      \item \code{'mtsl'} for the multiple-transmission single-linkage assumption (\code{\link{prob_trans_mtsl}}).
##'      \item \code{'mtml'} for the multiple-transmission multiple-linkage assumption (\code{\link{prob_trans_mtml}}).
##'      }
##'
##' @return scalar or vector giving the true discovery rate
##'
##' @author John Giles, Shirlee Wohl, and Justin Lessler
##'
##' @example R/examples/falsediscoveryrate.R
##'
##' @family discovery_rate
##'
##' @export
##'

falsediscoveryrate <- function(eta, # sensitivity of the linkage criteria
                               chi, # specificity of the linkage criteria
                               rho, # sampling proportion
                               M, # number of cases sampled
                               R = NULL, # effective reproductive number
                               assumption = "mtml" # assume most general case if not specified
) {
  suppressMessages(
    1 - truediscoveryrate(
      eta = eta,
      chi = chi,
      rho = rho,
      M = M,
      R = R,
      assumption = assumption
    )
  )
}
