##' Calculate probability of transmission
##'
##' This function calculates the probabilitiy that two cases are linked by direct transmission
##' given that they have been linked by phylogenetic criteria.
##' Assumptions about transmission and linkage (single or multiple) can be specified.
##'
##' @param eta scalar or vector giving the sensitivity of the linkage criteria
##' @param chi scalar or vector giving the specificity of the linkage criteria
##' @param rho scalar or vector giving the proportion of the final outbreak size that is sampled
##' @param M scalar or vector giving the number of cases sampled
##' @param R scalar or vector giving the effective reproductive number of the pathogen
##' @param assumption a character vector indicating which assumptions about transmission and linkage criteria. Default = \code{'mtml'}. Accepted arguments are:
##' \enumerate{
##'      \item \code{'stsl'} for the single-transmission single-linkage assumption.
##'      \item \code{'mtsl'} for the multiple-transmission single-linkage assumption.
##'      \item \code{'mtml'} for the multiple-transmission multiple-linkage assumption.
##'      }
##'
##' @return scalar or vector giving the probability of transmission between two cases given linkage by phylogeneitic criteria
##'
##' @author John Giles, Shirlee Wohl, and Justin Lessler
##'
##' @examples
##' translink_prob_transmit(eta=0.99, chi=0.9, rho=0.5, M=100, R=1)
##'
##' @family transmission linkage functions
##'
##' @export
##'

translink_prob_transmit <- function(eta, # sensitivity of the linkage criteria
                            chi, # specificity of the linkage criteria
                            rho, # sampling proportion
                            M, # number of cases sampled
                            R, # effective reproductive number
                            assumption = "mtml" # assume most general case if not specified
) {
  if (assumption == "stsl") {
    message("Calculating probability of transmission assuming single-transmission and single-linkage")
    out <- translink_prob_transmit_stsl(eta = eta, chi = chi, rho = rho, M = M)
  } else if (assumption == "mtsl") {
    message("Calculating probability of transmission assuming multiple-transmission and single-linkage")
    out <- translink_prob_transmit_mtsl(eta = eta, chi = chi, rho = rho, M = M, R = R)
  } else if (assumption == "mtml") {
    message("Calculating probability of transmission assuming multiple-transmission and multiple-linkage")
    out <- translink_prob_transmit_mtml(eta = eta, chi = chi, rho = rho, M = M, R = R)
  } else {
    stop("Incorrect assumption argument")
  }

  return(out)
}