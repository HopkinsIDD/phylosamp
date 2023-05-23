##' Calculate sample size needed to detect differential transmission
##' 
##' @description 
##' Function for calculating sample size given a set of assumptions. This is the
##' high level wrapper function that users should call directly.
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
##' @param correct_for_imbalance Logical. Should we use simulation to correct
##'     for being over/under powered due to large differences in group sizes?
##'     Default: `FALSE`.
##'
##' @return Sample size needed achieve desired type I and II error rates
##'     under assumptions. Will return `NA` and throw a warning if impossible.
##' @export
relR_samplesize <- function(R_a,
                            R_b,
                            p_a,
                            N,
                            alpha = 0.05,
                            alternative = c("two_sided", "less", "greater"),
                            power = 0.8,
                            sensitivity = 1,
                            specificity = 1,
                            overdispersion = NULL,
                            correct_for_imbalance = FALSE) {
  if (correct_for_imbalance) {
    m <-
      relR_samplesize_simsolve(
        R_a = R_a,
        R_b = R_b,
        p_a = p_a,
        N = N,
        alpha = alpha,
        alternative = alternative,
        power = power,
        sensitivity = sensitivity,
        specificity = specificity,
        overdispersion = overdispersion
      )

  } else {
    m <-
      relR_samplesize_linkerr(
        R_a = R_a,
        R_b = R_b,
        p_a = p_a,
        N = N,
        alpha = alpha,
        alternative = alternative,
        power = power,
        sensitivity = sensitivity,
        specificity = specificity,
        overdispersion = overdispersion
      )
  }
  return(m)

}
