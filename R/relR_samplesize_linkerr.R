##' Function to run the sample size calculation correcting for imperfect
##' sensitivity and specificity, but not doing any simulation based corrections.
##'
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
##' @param allow_impossible_m Logical. Indicates whether a value for `m` can be
##'    returned that is greater than the input `N`. Default: `FALSE`.
##'
##' @return Sample size needed achieve desired type I and II error rates
##'     under assumptions. Will return NA and throw a warning if impossible.
relR_samplesize_linkerr <- function(R_a,
                                    R_b,
                                    p_a,
                                    N,
                                    alpha = 0.05,
                                    alternative = c("two_sided", "less", "greater"),
                                    power = 0.8,
                                    sensitivity = 1,
                                    specificity = 1,
                                    overdispersion = NULL,
                                    allow_impossible_m = FALSE) {
  check_proportion(sensitivity)
  check_proportion(specificity)

  if (specificity < 1) {
    m <- relR_samplesize_solve(
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
      allow_impossible_m = allow_impossible_m
    )
  } else {
    ## Correct R_a and R_b for imperfect sensitivity.
    R_a <- R_a * (sensitivity)
    R_b <- R_b * (sensitivity)
    m <- relR_samplesize_basic(
      R_a = R_a,
      R_b = R_b,
      p_a = p_a,
      N = N,
      alpha = alpha,
      alternative = alternative,
      power = power,
      overdispersion = overdispersion,
      allow_impossible_m = allow_impossible_m
    )
  }

  return(m)

}

update_R_linkerr <- function(R, p, N, m, sensitivity, specificity) {
  R_star <- R * (sensitivity + specificity - 1) +
    (((m * p - 1) / 2) * (1 - specificity) * (N -  1)) / ((m - 1) * p)
  return(R_star)
}
