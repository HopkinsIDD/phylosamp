##' Calculate false discovery rate of a sample
##'
##' @description
##' `r lifecycle::badge('deprecated')
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
##' @examples
##' # The simplest case: single-transmission, single-linkage, and perfect sensitivity
##' falsediscoveryrate(eta=1, chi=0.9, rho=0.5, M=100, assumption='stsl')
##'
##' # Multiple-transmission and imperfect sensitivity
##' falsediscoveryrate(eta=0.99, chi=0.9, rho=1, M=50, R=1, assumption='mtsl')
##'
##' # Small outbreak, larger sampling proportion
##' falsediscoveryrate(eta=0.99, chi=0.95, rho=1, M=50, R=1, assumption='mtml')
##'
##' # Large outbreak, small sampling proportion
##' falsediscoveryrate(eta=0.99, chi=0.95, rho=0.5, M=1000, R=1, assumption='mtml')
##'
##' @family discovery_rate
##'
##' @export
##'

falsediscoveryrate <- function(eta, chi, rho, M, R = NULL, assumption = "mtml") {
    lifecycle::deprecate_soft("1.0.0", "falsediscoveryrate()", "translink_fdr()")
    lifecycle::deprecate_soft("1.0.0", "falsediscoveryrate(eta)", "translink_fdr(sensitivity)")
    lifecycle::deprecate_soft("1.0.0", "falsediscoveryrate(chi)", "translink_fdr(specificity)")

    suppressMessages(1 - truediscoveryrate(eta = eta, chi = chi, rho = rho, M = M,
        R = R, assumption = assumption))
}
