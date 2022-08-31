##' Probability of transmission assuming single-transmission and single-linkage
##'
##' This function calculates the probabilitiy that two cases are linked by direct transmission
##' given that they have been linked by phylogenetic criteria. The single-transmission and single-linkage method assumes the following:
##' \enumerate{
##'      \item Each case \eqn{i} is linked by transmission to only one other case \eqn{j} in the population (\eqn{N}).
##'      \item Each case \eqn{i} is linked by the linkage criteria to only one other case \eqn{j} in the sampled population (\eqn{M}).
##'      }
##'
##' For perfect sensitivity, set \code{eta = 1}.
##'
##' @param eta scalar or vector giving the sensitivity of the linkage criteria
##' @param chi scalar or vector giving the specificity of the linkage criteria
##' @param rho scalar or vector giving the proportion of the final outbreak size that is sampled
##' @param M scalar or vector giving the number of cases sampled
##'
##' @return scalar or vector giving the probability of transmission between two cases given linkage by phylogeneitic criteria
##'
##' @author John Giles, Shirlee Wohl, and Justin Lessler
##'
##' @examples
##' # perfect sensitivity and specificity
##' prob_trans_stsl(eta=1, chi=1, rho=0.2, M=100)
##'
##' # perfect sensitivity only
##' prob_trans_stsl(eta=1, chi=0.95, rho=0.2, M=100)
##'
##' prob_trans_stsl(eta=0.99, chi=0.95, rho=0.9, M=50)
##'
##' prob_trans_stsl(eta=0.99, chi=0.95, rho=0.05, M=100)
##'
##' @family prob_trans
##'
##' @export

prob_trans_stsl <- function(eta, # sensitivity of the linkage criteria
                            chi, # specificity of the linkage criteria
                            rho, # sampling proportion
                            M # number of cases sampled
) {
  if (!all(is.numeric(eta), eta >= 0 & eta <= 1)) stop("eta must be numeric between 0 and 1")
  if (!all(is.numeric(chi), chi >= 0 & chi <= 1)) stop("chi must be numeric between 0 and 1")
  if (!all(is.numeric(rho), rho > 0 & rho <= 1)) stop("rho must be numeric > 0 and <= 1")
  if (!all(is.numeric(M) | is.integer(M), M >= 0)) stop("Sample size (M) must be integer or numeric greater than 0")

  (eta * rho) /
    ((eta * rho) + ((1 - chi^(M - 2)) * (1 - eta) * rho) + ((1 - chi^(M - 1)) * (1 - rho)))
}
