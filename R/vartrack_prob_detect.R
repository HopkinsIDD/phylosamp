##' Calculate the probability of detecting a variant given a sample size
##'
##' This function calculates the probability of detecting the presence of a variant
##' given a sample size and sampling strategy.
##'
##' @param n sample size (either of cross-section or per timestep)
##' @param t time step number (e.g., days) at which variant should be detected by. Default = NA (either `'t'` or `'p_v1'` should be provided, not both)
##' @param p_v1 the desired prevalence to detect a variant by. Default = NA (either `'t'` or `'p_v1'` should be provided, not both)
##' @param omega probability of sequencing (or other characterization) success
##' @param p0_v1 initial variant prevalence (# introductions / infected population size)
##' @param r_v1 logistic growth rate
##' @param c_ratio coefficient of detection ratio, calculated as the ratio of the coefficients of variant 1 to variant 2. Default = 1 (no bias)
##' @param sampling_freq the sampling frequency (must be either 'xsect' or 'cont')
##' @return scalar of detection probability
##'
##' @author Shirlee Wohl, Elizabeth C. Lee, Bethany L. DiPrete, and Justin Lessler
##'
##' @examples
##' # Cross-sectional sampling
##' vartrack_prob_detect(p_v1 = 0.02, n = 100, omega = 0.8, c_ratio = 1, sampling_freq = 'xsect')
##'
##' # Periodic sampling
##' vartrack_prob_detect(n = 158, t = 30, omega = 0.8, p0_v1 = 1/10000, 
##' r_v1 = 0.1, c_ratio = 1, sampling_freq = 'cont')
##'
##' @family variant detection functions
##' @family variant tracking functions
##'
##' @export

vartrack_prob_detect <- function(n, t = NA, p_v1 = NA, omega, p0_v1 = NA, r_v1 = NA,
    c_ratio = 1, sampling_freq) {

    if (sampling_freq == "xsect") {
        message("Calculating probability of detection assuming single cross-sectional sample")
        out <- vartrack_prob_detect_xsect(p_v1 = p_v1, n = n, omega = omega, c_ratio = c_ratio)
    } else if (sampling_freq == "cont") {
        message("Calculating probability of detection assuming periodic sampling")
        out <- vartrack_prob_detect_cont(n = n, t = t, p_v1 = p_v1, omega = omega,
            p0_v1 = p0_v1, r_v1 = r_v1, c_ratio = c_ratio)
    } else {
        stop("Incorrect sampling frequency argument (please specify 'xsect' or 'cont')")
    }

    return(out)
}
