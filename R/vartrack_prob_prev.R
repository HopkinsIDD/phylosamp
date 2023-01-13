##' Function to calculate confidence in a variant estimate given a sample size
##'
##' @param p_v1 variant prevalence (proportion)
##' @param n sample size
##' @param omega probability of sequencing (or other characterization) success
##' @param precision desired precision in variant prevalence estimate
##' @param c_ratio coefficient of detection ratio, calculated as the ratio of the coefficients of variant 1 to variant 2. default = 1 (no bias)
##' @param sampling_freq the sampling frequency (must be either "xsect" or "cont")
##' @return scalar of expected sample size
##'
##' @author Shirlee Wohl, Elizabeth C. Lee, Bethany L. DiPrete, and Justin Lessler
##'
##' @examples
##' vartrack_prob_prev(p_v1 = 0.1, n = 200, omega = 0.8, c_ratio = 1, sampling_freq = "xsect")
##'
##' @family variant tracking functions
##'
##' @export


vartrack_prob_prev <- function(p_v1, n, omega, precision, c_ratio = 1, sampling_freq) {
  
  if (sampling_freq == "xsect"){
    message("Calculating confidence in variant estimate assuming single cross-sectional sample")
    out <- vartrack_prob_prev_xsect(p_v1 = p_v1, n = n, omega = omega, precision = precision, c_ratio = c_ratio)
  }

  else if (sampling_freq == "cont"){
    stop("Functionality for calculating confidence in variant estimate given periodic sampling is not yet implemented in the phylosamp package. You can calculate the confidence in variant estimating given a cross-sectional sample by specifying 'xsect' as the sampling frequency")
  }

  else {
    stop("Incorrect sampling frequency argument (please specify 'xsect' or 'cont')")
  }
}