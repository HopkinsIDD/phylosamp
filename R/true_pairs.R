##' Calculate expected number of true transmission pairs
##'
##' @description
##' `r lifecycle::badge('deprecated')`
##' This function calculates the expected number true transmission pairs in a sample of size `M`.
##' Assumptions about transmission and linkage (single or multiple) can be specified.
##'
##' @param eta scalar or vector giving the sensitivity of the linkage criteria
##' @param rho scalar or vector giving the proportion of the final outbreak size that is sampled
##' @param M scalar or vector giving the number of cases sampled
##' @param R scalar or vector giving the effective reproductive number of the pathogen (default=NULL)
##' @param assumption a character vector indicating which assumptions about transmission and linkage criteria. Default = `'mtml'`. Accepted arguments are:
##' \enumerate{
##'      \item `'stsl'` for the single-transmission single-linkage assumption ([prob_trans_stsl()]).
##'      \item `'mtsl'` for the multiple-transmission single-linkage assumption ([prob_trans_mtsl()]).
##'      \item `'mtml'` for the multiple-transmission multiple-linkage assumption ([prob_trans_mtml()]).
##'      }
##'
##' @return scalar or vector giving the expected number of true transmission pairs in the sample
##'
##' @author John Giles, Shirlee Wohl, and Justin Lessler
##'
##' @examples
##' true_pairs(eta=0.99, rho=0.75, M=100, R=1)
##'
##' @family true_pairs
##'
##' @export
##'

true_pairs <- function(eta, rho, M, R = NULL, assumption = "mtml") {
    lifecycle::deprecate_soft("1.0.0", "true_pairs()", "translink_expected_links_true()")
    lifecycle::deprecate_soft("1.0.0", "true_pairs(eta)", "translink_expected_links_true(sensitivity)")

    if (assumption == "stsl") {
        message("Calculating expected number of links assuming single-transmission and single-linkage")
        out <- true_pairs_stsl(eta = eta, rho = rho, M = M)
    } else if (assumption == "mtsl") {
        message("Calculating expected number of links assuming multiple-transmission and single-linkage")
        out <- true_pairs_mtsl(eta = eta, rho = rho, M = M, R = R)
    } else if (assumption == "mtml") {
        message("Calculating expected number of links assuming multiple-transmission and multiple-linkage")
        out <- true_pairs_mtml(eta = eta, rho = rho, M = M, R = R)
    } else {
        stop("Incorrect assumption argument")
    }

    return(out)
}
