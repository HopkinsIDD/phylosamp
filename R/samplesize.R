
##' Calculate sample size
##'
##' @description
##' `r lifecycle::badge('deprecated')`
##' This function calculates the sample size needed to obtain at least a defined false disovery rate given
##' a final outbreak size \eqn{N}.
##'
##' @param eta scalar or vector giving the sensitivity of the linkage criteria
##' @param chi scalar or vector giving the specificity of the linkage criteria
##' @param N scalar or vector giving the final outbreak size
##' @param R scalar or vector giving the effective reproductive number of the pathogen
##' @param phi scalar or vector giving the desired true discovery rate (1-false discovery rate)
##' @param min_pairs minimum number of linked pairs observed in the sample, defaults to 1 pair (2 samples); this is to ensure reasonable results are obtained
##' @param assumption a character vector indicating which assumptions about transmission and linkage criteria. Default = `'mtml'`. Accepted arguments are:
##' \enumerate{
##'      \item `'stsl'` for the single-transmission single-linkage assumption ([prob_trans_stsl()]).
##'      \item `'mtsl'` for the multiple-transmission single-linkage assumption ([prob_trans_mtsl()]).
##'      \item `'mtml'` for the multiple-transmission multiple-linkage assumption ([prob_trans_mtml()]).
##'      }
##'
##' @return scalar or vector giving the sample size needed to meet the given conditions
##'
##' @author John Giles, Shirlee Wohl, and Justin Lessler
##'
##' @examples
##' samplesize(eta=0.99, chi=0.995, N=100, R=1, phi=0.75)
##'
##' @family inverse_functions
##'
##' @export
##'

samplesize <- function(eta, chi, N, R = NULL, phi, min_pairs = 1, assumption = "mtml") {
    lifecycle::deprecate_soft("1.0.0", "samplesize()", "translink_samplesize()")
    lifecycle::deprecate_soft("1.0.0", "samplesize(eta)", "translink_samplesize(sensitivity)")
    lifecycle::deprecate_soft("1.0.0", "samplesize(chi)", "translink_samplesize(specificity)")
    lifecycle::deprecate_soft("1.0.0", "samplesize(phi)", "translink_samplesize(tdr)")

    if (!(is.numeric(phi) & phi >= 0 & phi <= 1)) {
        stop("phi must be numeric between 0 and 1")
    }

    # the max sample size in the final size of the outbreak iterate between
    # minimum and maximum sample size until the desired value is reached
    samplesize_found <- FALSE
    for (i in 2:N) {
        tdr_calc <- suppressMessages(truediscoveryrate(eta = eta, chi = chi, rho = i/N,
            M = i, R = R, assumption = assumption))
        obs_pairs <- suppressMessages(exp_links(eta = eta, chi = chi, rho = i/N,
            M = i, R = R, assumption = assumption))

        if (tdr_calc >= phi & obs_pairs >= min_pairs) {
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
