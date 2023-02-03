##' Calculate expected number of links in a sample
##'
##' @description
##' `r lifecycle::badge('deprecated')
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
##'      \item \code{'stsl'} for the single-transmission single-linkage assumption (\code{\link{prob_trans_stsl}}).
##'      \item \code{'mtsl'} for the multiple-transmission single-linkage assumption (\code{\link{prob_trans_mtsl}}).
##'      \item \code{'mtml'} for the multiple-transmission multiple-linkage assumption (\code{\link{prob_trans_mtml}}).
##'      }
##'
##' @return scalar or vector giving the expected number of observed links in the sample
##'
##' @author John Giles, Shirlee Wohl, and Justin Lessler
##'
##' @examples
##' # The simplest case: single-transmission, single-linkage, and perfect sensitivity
##' exp_links(eta=1, chi=0.9, rho=0.5, M=100, assumption='stsl')
##'
##' # Multiple-transmission and imperfect sensitivity
##' exp_links(eta=0.99, chi=0.9, rho=1, M=50, R=1, assumption='mtsl')
##'
##' # Small outbreak, larger sampling proportion
##' exp_links(eta=0.99, chi=0.95, rho=1, M=50, R=1, assumption='mtml')
##'
##' # Large outbreak, small sampling proportion
##' exp_links(eta=0.99, chi=0.95, rho=0.05, M=1000, R=1, assumption='mtml')
##'
##' @family obs_pairs
##'
##' @export
##'

exp_links <- function(eta, chi, rho, M, R = NULL, assumption = "mtml") {
    lifecycle::deprecate_soft("1.0.0", "exp_links()", "translink_expected_links_obs()")
    lifecycle::deprecate_soft("1.0.0", "exp_links(eta)", "translink_expected_links_obs(sensitivity)")
    lifecycle::deprecate_soft("1.0.0", "exp_links(chi)", "translink_expected_links_obs(specificity)")

    if (assumption == "stsl") {
        message("Calculating expected number of links assuming single-transmission and single-linkage")
        out <- obs_pairs_stsl(eta = eta, chi = chi, rho = rho, M = M)
    } else if (assumption == "mtsl") {
        message("Calculating expected number of links assuming multiple-transmission and single-linkage")
        out <- obs_pairs_mtsl(eta = eta, chi = chi, rho = rho, M = M, R = R)
    } else if (assumption == "mtml") {
        message("Calculating expected number of links assuming multiple-transmission and multiple-linkage")
        out <- obs_pairs_mtml(eta = eta, chi = chi, rho = rho, M = M, R = R)
    } else {
        stop("Incorrect assumption argument")
    }

    return(out)
}
