##' Calculate sample size needed to identify true transmission links
##'
##' This function calculates the sample size needed to identify transmission links at
##' a predefined false discovery rate, given a final outbreak size \eqn{N}.
##'
##' @param sensitivity scalar or vector giving the sensitivity of the linkage criteria
##' @param specificity scalar or vector giving the specificity of the linkage criteria
##' @param N scalar or vector giving the final outbreak size
##' @param R scalar or vector giving the effective reproductive number of the pathogen
##' @param tdr scalar or vector giving the desired true discovery rate (1-false discovery rate)
##' @param min_pairs minimum number of linked pairs observed in the sample, defaults to 1 pair (2 samples); this is to ensure reasonable results are obtained
##' @param assumption a character vector indicating which assumptions about transmission and linkage criteria. Default = `'mtml'`. Accepted arguments are:
##' \enumerate{
##'      \item `'stsl'` for the single-transmission single-linkage assumption.
##'      \item `'mtsl'` for the multiple-transmission single-linkage assumption.
##'      \item `'mtml'` for the multiple-transmission multiple-linkage assumption.
##'      }
##'
##' @return scalar or vector giving the sample size needed to meet the given conditions
##'
##' @author John Giles, Shirlee Wohl, and Justin Lessler
##'
##' @examples
##' translink_samplesize(sensitivity=0.99, specificity=0.995, N=100, R=1, tdr=0.75)
##'
##' @family transmission linkage functions
##'
##' @export
##'

translink_samplesize <- function(sensitivity, specificity, N, R = NULL, tdr, min_pairs = 1,
    assumption = "mtml") {
    if (!(is.numeric(tdr) & tdr >= 0 & tdr <= 1)) {
        stop("tdr must be numeric between 0 and 1")
    }

    # the max sample size in the final size of the outbreak iterate between
    # minimum and maximum sample size until the desired value is reached
    samplesize_found <- FALSE
    for (i in 2:N) {
        tdr_calc <- suppressMessages(translink_tdr(sensitivity = sensitivity, specificity = specificity,
            rho = i/N, M = i, R = R, assumption = assumption))
        obs_pairs <- suppressMessages(translink_expected_links_obs(sensitivity = sensitivity,
            specificity = specificity, rho = i/N, M = i, R = R, assumption = assumption))

        if (tdr_calc >= tdr & obs_pairs >= min_pairs) {
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
