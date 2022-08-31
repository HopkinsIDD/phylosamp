##' Probability of transmission assuming multiple-transmission and multiple-linkage
##'
##' This function calculates the probabilitiy that two cases are linked by direct transmission
##' given that they have been linked by phylogenetic criteria. The multiple-transmission and multiple-linkage method assumes the following:
##' \enumerate{
##'      \item Each case \eqn{i} is, on average, the infector of \code{R} cases in the population (\eqn{N})
##'      \item Each case \eqn{i} is allowed to be linked by the linkage criteria to multiple cases \eqn{j} in the sampled population (\eqn{M}).
##'      \item Linkage events are independent of one another (i.e, linkage of case \eqn{i} to case \eqn{j} has no bearing on linkage of case \eqn{i} to any other sample).
##'      }
##'
##' @param eta scalar or vector giving the sensitivity of the linkage criteria
##' @param chi scalar or vector giving the specificity of the linkage criteria
##' @param rho scalar or vector giving the proportion of the final outbreak size that is sampled
##' @param M scalar or vector giving the number of cases sampled
##' @param R scalar or vector giving the effective reproductive number of the pathogen
##'
##' @return scalar or vector giving the probability of transmission between two cases given linkage by phylogeneitic criteria
##'
##' @author John Giles, Shirlee Wohl, and Justin Lessler
##'
##' @example R/examples/prob_trans_mtml.R
##'
##' @family prob_trans
##'
##' @export
##'

prob_trans_mtml <- function(eta, # sensitivity of the linkage criteria
                            chi, # specificity of the linkage criteria
                            rho, # sampling proportion
                            M, # number of cases sampled
                            R # effective reproductive number
) {
  if (!all(is.numeric(eta), eta >= 0 & eta <= 1)) stop("eta must be numeric between 0 and 1")
  if (!all(is.numeric(chi), chi >= 0 & chi <= 1)) stop("chi must be numeric between 0 and 1")
  if (!all(is.numeric(rho), rho > 0 & rho <= 1)) stop("rho must be numeric > 0 and <= 1")
  if (!all(is.numeric(M) | is.integer(M), M >= 0)) stop("Sample size (M) must be integer or numeric greater than 0")
  if (!all(is.numeric(R), R > 0)) stop("Reproductive number (R) must be numeric greater than 0")
  if (!all(is.numeric(R), R <= 1)) warning("Reproductive number (R) is usually less than 1 for finite outbreaks")

  (eta * rho * (R + 1)) /
    ((eta * rho * (R + 1)) + ((1 - chi) * (M - 1 - (rho * (R + 1)))))
}
