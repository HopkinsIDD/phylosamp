##' Calculate sample size needed for variant detection given a desired probability of detection
##'
##' This function calculates the sample size needed for detecting the presence of a variant
##' given a desired probability of detection and sampling strategy.
##'
##' @param prob desired probability of detection
##' @param t time step number (e.g., days) at which variant should be detected by. Default = NA (either `'t'` or `'p_v1'` should be provided, not both)
##' @param p_v1 the desired prevalence to detect a variant by. Default = NA (either `'t'` or `'p_v1'` should be provided, not both)
##' @param omega probability of sequencing (or other characterization) success
##' @param p0_v1 initial variant prevalence (# introductions / infected population size)
##' @param r_v1 logistic growth rate
##' @param c_ratio coefficient of detection ratio, calculated as the ratio of the coefficients of variant 1 to variant 2. Default = 1 (no bias)
##' @param sampling_freq the sampling frequency (must be either 'xsect' or 'cont')
##' @return scalar of expected sample size
##'
##' @author Shirlee Wohl, Elizabeth C. Lee, Bethany L. DiPrete, and Justin Lessler
##'
##' @examples
##' # Cross-sectional sampling
##' vartrack_samplesize_detect(p_v1 = 0.1, prob = 0.95, omega = 0.8,
##'                            c_ratio = 1, sampling_freq = 'xsect')
##'
##' # Periodic sampling
##' vartrack_samplesize_detect(prob = 0.95, t = 30, omega = 0.8, p0_v1 = 1/10000,
##'                            r_v1 = 0.1, c_ratio = 1, sampling_freq = 'cont')
##'
##' @family variant detection functions
##' @family variant tracking functions
##'
##' @export


vartrack_samplesize_detect <- function(prob, t = NA, p_v1 = NA, omega, p0_v1 = NA,
    r_v1 = NA, c_ratio = 1, sampling_freq) {

    if (sampling_freq == "xsect") {
        message("Calculating sample size for variant detection assuming single cross-sectional sample")
        out <- vartrack_samplesize_detect_xsect(p_v1 = p_v1, prob = prob, omega = omega,
            c_ratio = c_ratio)
    } else if (sampling_freq == "cont") {
        message("Calculating sample size for variant detection assuming periodic sampling")
        out <- vartrack_samplesize_detect_cont(prob = prob, t = t, p_v1 = p_v1, omega = omega,
            p0_v1 = p0_v1, r_v1 = r_v1, c_ratio = c_ratio)
    } else {
        stop("Incorrect sampling frequency argument (please specify 'xsect' or 'cont')")
    }

    return(out)
}
