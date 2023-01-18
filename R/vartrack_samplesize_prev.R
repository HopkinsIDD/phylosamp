##' Calculate sample size needed for estimating variant prevalence given a desired confidence
##'
##' This function calculates the sample size needed for estimating variant prevalence
##' given a desired confidence and desired precision in the variant prevalence estimate.
##' Currently, only cross-sectional sampling is supported.
##' 
##' @param p_v1 variant prevalence (proportion)
##' @param prob desired confidence in variant prevalence estimate
##' @param precision desired precision in variant prevalence estimate
##' @param omega probability of sequencing (or other characterization) success
##' @param c_ratio coefficient of detection ratio, calculated as the ratio of the coefficients of variant 1 to variant 2. Default = 1 (no bias)
##' @param sampling_freq the sampling frequency (must be "xsect" in current implementation)
##' @return scalar of sample size
##'
##' @author Shirlee Wohl, Elizabeth C. Lee, Bethany L. DiPrete, and Justin Lessler
##'
##' @examples
##' vartrack_samplesize_prev(p_v1 = 0.1, prob = 0.95, precision = 0.25, omega = 0.8, c_ratio = 1, sampling_freq = "xsect")
##'
##' @family variant prevalence estimation functions
##' @family variant tracking functions
##'
##' @export


vartrack_samplesize_prev <- function(p_v1, prob, precision, omega, c_ratio = 1, sampling_freq) {
  
  if (sampling_freq == "xsect"){
    message("Calculating sample size for variant prevalence estimation assuming single cross-sectional sample")
    out <- vartrack_samplesize_prev_xsect(p_v1 = p_v1, prob = prob, precision = precision, omega = omega, c_ratio = c_ratio)
  }

  else if (sampling_freq == "cont"){
    stop("Functionality for the sample size needed for estimating variant prevalence given periodic sampling is not yet implemented in the phylosamp package. You can calculate the sample size needed for variant prevalence estimation given a cross-sectional sample by specifying 'xsect' as the sampling frequency")
  }

  else {
    stop("Incorrect sampling frequency argument (please specify 'xsect' or 'cont')")
  }
}