##' Calculate true discovery rate of a sample
##'
##' This function calculates the true discovery rate (proportion of true transmission pairs) in a sample given the sensitivity \eqn{\eta}
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
##' @example R/examples/truediscoveryrate.R
##'
##' @family discovery_rate
##'
##' @export
##'

truediscoveryrate <- function(
  eta,    # sensitivity of the linkage criteria
  chi,    # specificity of the linkage criteria
  rho,    # sampling proportion
  M,      # number of cases sampled
  R=NULL,       # effective reproductive number
  assumption='mtml' # assume most general case if not specified
){

  if (assumption == 'stsl') {

    message('Calculating true discovery rate assuming single-transmission and single-linkage')
    out <- prob_trans_stsl(eta=eta, chi=chi, rho=rho, M=M)

  } else if (assumption == 'mtsl') {

    message('Calculating true discovery rate assuming multiple-transmission and single-linkage')
    out <- prob_trans_mtsl(eta=eta, chi=chi, rho=rho, M=M, R=R)

  } else if (assumption == 'mtml') {

    message('Calculating true discovery rate assuming multiple-transmission and multiple-linkage')
    out <- prob_trans_mtml(eta=eta, chi=chi, rho=rho, M=M, R=R)

  } else {

    stop("Incorrect assumption argument")
  }

  return(out)
}