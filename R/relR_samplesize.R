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
##' @examples 
##' 
##' ## Calculate sample size needed to detect a difference between groups where 
##' ## group A has a reproductive value of 2, group B has a reproductive 
##' ## value of 2.5, the groups are balanced, and the total outbreak size is 
##' ## 1,000
##' 
##' relR_samplesize(R_a = 2, 
##'                 R_b = 2.5, 
##'                 p_a = 0.5,
##'                 N = 1000)
##' 
##' ## Update the above calculation to account for imperfect sensitivity = 0.7
##' relR_samplesize(R_a = 2, 
##'                 R_b = 2.5, 
##'                 p_a = 0.5,
##'                 N = 1000,
##'                 sensitivity = 0.7)
##' 
##' ## Update the above calculation to allow for overdispersion
##' relR_samplesize(R_a = 2, 
##'                 R_b = 2.5, 
##'                 p_a = 0.5,
##'                 N = 1000,
##'                 sensitivity = 0.7,
##'                 overdispersion = 2000)
##' 
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
