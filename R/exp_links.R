##' Calculate expected number of links in a sample
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
##'      \item \code{'stsl'} for the single-transmission single-linkage assumption (\code{\link{prob_trans_stsl}}).
##'      \item \code{'mtsl'} for the multiple-transmission single-linkage assumption (\code{\link{prob_trans_mtsl}}).
##'      \item \code{'mtml'} for the multiple-transmission multiple-linkage assumption (\code{\link{prob_trans_mtml}}).
##'      }
##'
##' @return scalar or vector giving the expected number of observed links in the sample
##'
##' @author John Giles, Shirlee Wohl, and Justin Lessler
##'
##' @example R/examples/exp_links.R
##'
##' @family obs_pairs
##'
##' @export
##'

exp_links <- function(eta, # sensitivity of the linkage criteria
                      chi, # specificity of the linkage criteria
                      rho, # sampling proportion
                      M, # number of cases sampled
                      R = NULL, # effective reproductive number
                      assumption = "mtml" # assume most general case if not specified
) {
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
