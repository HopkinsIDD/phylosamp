##' Calculate sample size
##'
##' This function calculates the sample size needed to obtain at least a defined false disovery rate given
##' a final outbreak size \eqn{N}.
##'
##' @param eta scalar or vector giving the sensitivity of the linkage criteria
##' @param chi scalar or vector giving the specificity of the linkage criteria
##' @param N scalar or vector giving the final outbreak size
##' @param R scalar or vector giving the effective reproductive number of the pathogen
##' @param phi scalar or vector giving the desired true discovery rate (1-false discovery rate)
##' @param min_pairs minimum number of linked pairs observed in the sample, defaults to 1 pair (2 samples); this is to ensure reasonable results are obtained
##' @param assumption a character vector indicating which assumptions about transmission and linkage criteria. Default = \code{'mtml'}. Accepted arguments are:
##' \enumerate{
##'      \item \code{'stsl'} for the single-transmission single-linkage assumption.
##'      \item \code{'mtsl'} for the multiple-transmission single-linkage assumption.
##'      \item \code{'mtml'} for the multiple-transmission multiple-linkage assumption.
##'      }
##'
##' @return scalar or vector giving the sample size needed to meet the given conditions
##'
##' @author John Giles, Shirlee Wohl, and Justin Lessler
##'
##' @examples
##' translink_samplesize(eta=0.99, chi=0.995, N=100, R=1, phi=0.75)
##'
##' @family transmission linkage functions
##'
##' @export
##'

translink_samplesize <- function(eta, # sensitivity of the linkage criteria
                       chi, # specificity of the linkage criteria
                       N, # final outbreak size
                       R = NULL, # effective reproductive number
                       phi, # minimum true discovery rate
                       min_pairs = 1, # minimum number of linked pairs, defaults to 1 (2 samples)
                       assumption = "mtml" # assume most general case if not specified
) {
  if (!(is.numeric(phi) & phi >= 0 & phi <= 1)) {
    stop("phi must be numeric between 0 and 1")
  }

  # the max sample size in the final size of the outbreak
  # iterate between minimum and maximum sample size until the desired value is reached
  samplesize_found <- FALSE
  for (i in 2:N) {
    tdr <- suppressMessages(translink_tdr(eta = eta, chi = chi, rho = i / N, M = i, R = R, assumption = assumption))
    obs_pairs <- suppressMessages(translink_expected_links_obs(eta = eta, chi = chi, rho = i / N, M = i, R = R, assumption = assumption))

    if (tdr >= phi & obs_pairs >= min_pairs) {
      samplesize_found <- TRUE
      break
    }
  }

  # check if we successfully achieved the desired true discovery rate
  if (samplesize_found) {
    return(i)
  } else {
    stop("Input values do no produce a viable solution")
  }
}
