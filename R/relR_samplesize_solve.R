##' Calculate optimal sample size for detecting differential transmission with imperfect specificity
##' 
##' @description 
##' Function to solve for optimal sample size when the specificity isn't 1
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
##' @return The sample size
relR_samplesize_solve <- function(R_a,
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
  check_positive(N)
  check_proportion(p_a)
  check_proportion(sensitivity)
  check_proportion(specificity)
  check_proportion(alpha)
  check_proportion(power)

  m <-
    stats::optimize(
      relR_samplesize_opterr,
      interval = c(1, N * 2),
      R_a = R_a,
      R_b = R_b,
      p_a = p_a,
      N = N,
      sensitivity = sensitivity,
      specificity = specificity,
      overdispersion = overdispersion,
      alpha = alpha,
      alternative = alternative,
      power = power
    )$minimum

  if (m > N & !allow_impossible_m) {
    cli::cli_warn(
      c(
        "Necessary sample size not achievable.",
        "i" = "The necessary sample size given the input parameters is at least {round(m)}",
        "x" = "You input {.var N} = {N} as the total size of the outbreak"
      )
    )
    return(NA)
  }

  return(m)
}


##' Function to calculate the error in estimated sample size for use in optimize function
##'
##' @param m the sample size.
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
##'
##' @return Squared error between the input sample size and estimated sample size
##'
relR_samplesize_opterr <-
  function(m,
           R_a,
           R_b,
           p_a,
           N,
           alpha,
           alternative,
           power,
           sensitivity,
           specificity,
           overdispersion) {
    p_b <- 1 - p_a

    ## Correct R_a and R_b for sensitivity and specificity
    r_astar <-  update_R_linkerr(
      R = R_a,
      p = p_a,
      N = N,
      m = m,
      sensitivity = sensitivity,
      specificity = specificity
    )

    r_bstar <- update_R_linkerr(
      R = R_b,
      p = p_b,
      N = N,
      m = m,
      sensitivity = sensitivity,
      specificity = specificity
    )


    ## if the given inputs make the corrected R_a larger than R_b when the input
    ## is the opposite direction, return big error
    if (((r_astar > r_bstar) &
         (R_a < R_b)) | ((r_astar < r_bstar) & (R_a > R_b))) {
      return(100000)
    }


    ## Calculate error
    err <- relR_samplesize_basic(
      R_a = r_astar,
      R_b = r_bstar,
      p_a = p_a,
      N = N,
      alpha = alpha,
      alternative = alternative,
      power = power,
      overdispersion = overdispersion,
      allow_impossible_m = TRUE
    ) - m
    return(err ^ 2)

  }
