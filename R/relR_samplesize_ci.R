##' Calculate sample size for detecting differential transmission with uncertainty bounds
##' 
##' @description 
##' This function assumes you
##' want to correct for imbalance, if not there is a closed form solution
##' for the estimated sample size that does not include uncertainty bounds.
##' (see [`relR_samplesize`]).
##'
##' @template R_a
##' @template R_b
##' @template p_a
##' @template N
##' @template alpha
##' @template alternative
##' @template power
##' @template sensitivity
##' @template specificity
##' @template overdispersion
##' @param nsims The number of inner simulations run per estimate.
##'     Default: 10000
##' @param uncertainty_percent The percent of the uncertainty interval.
##'     Default: .95
##' @param B The number of outer simulations run to estimate the uncertainty.
##'     Default: 1000
##'
##' @return A vector with three quantities:
##' * sample size: Sample size needed achieve desired type I and II error rates
##'     under assumptions. Will return NA and throw a warning if impossible.
##' * lower bound: The lower bound of an uncertainty interval
##' * upper bound: The upper bound of an uncertainty interval
##' @export

relR_samplesize_ci <- function(R_a,
                               R_b,
                               p_a,
                               N,
                               alpha = 0.05,
                               alternative = c("two_sided", "less", "greater"),
                               power = 0.8,
                               sensitivity = 1,
                               specificity = 1,
                               overdispersion = NULL,
                               nsims = 1000,
                               uncertainty_percent = 0.95,
                               B = 1000) {
  rlang::check_installed("purrr", reason = "for `relR_samplesize_ci()`")
  m <- purrr::map_dbl(
    1:B,
    ~ relR_samplesize_simsolve(
      R_a = R_a,
      R_b = R_b,
      p_a = p_a,
      N = N,
      alpha = alpha,
      alternative = alternative,
      power = power,
      sensitivity = sensitivity,
      specificity = specificity,
      overdispersion = overdispersion,
      nsims = nsims
    )
  )
  return(
    c(
      "sample size" = round(mean(m), 1),
      "lower bound" = stats::quantile(m, (1 - uncertainty_percent) / 2),
      "upper bound" = stats::quantile(m, 1 - ((
        1 - uncertainty_percent
      ) / 2))
    )
  )

}
