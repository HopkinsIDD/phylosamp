##' Calculate optimized sample size for detecting differential transmission
##' 
##' @description 
##' Function to calculate optimized sample size by solving the  
##' transcendental equation that occurs when you replace the R values with ones
##' that account for sensitivity and specificity.
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
##' @param epsilon Numeric. Dictates the minimum value for `R_b = R_a + epsilon`
##'   attempted in the simulation. Default: 0.01.
##' @param nsims Dictates the number of simulations for each power simulation.
##'   Default: 100000
##' @param tolerance Dictates the tolerance for the binary search. Default: 10.
##'
##' @return Simulated sample size needed achieve desired type I and II error rates
##'     under assumptions. Will return NA and throw a warning if impossible.

relR_samplesize_simsolve <- function(R_a,
                                     R_b,
                                     p_a,
                                     N,
                                     alpha = 0.05,
                                     alternative = c("two_sided", "less", "greater"),
                                     power = 0.8,
                                     sensitivity = 1,
                                     specificity = 1,
                                     overdispersion = NULL,
                                     epsilon = 0.01,
                                     nsims = 100000,
                                     tolerance = 10) {
  m_1 <-
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
      overdispersion = overdispersion,
      allow_impossible_m = TRUE
    )
  m_0 <- relR_samplesize_linkerr(
    R_a = R_a,
    R_b = R_a + epsilon,
    p_a = p_a,
    N = N,
    alpha = alpha,
    alternative = alternative,
    power = power,
    sensitivity = sensitivity,
    specificity = specificity,
    overdispersion = overdispersion,
    allow_impossible_m = TRUE
  )



  if (m_1 > N) {
    m_1 <- N
    pwr_m1 <- relR_power_simulated(
      m = m_1,
      R_a = R_a,
      R_b = R_b,
      p_a = p_a,
      N = N,
      alpha = alpha,
      alternative = alternative,
      sensitivity = sensitivity,
      specificity = specificity,
      overdispersion = overdispersion,
      nsims = nsims
    )
    if (is.na(pwr_m1) | pwr_m1 < power) {
      cli::cli_warn(c("Necessary sample size not achievable."))
      return(NA)
    }
  }


  pwr_m1 <- relR_power_simulated(
    m = m_1,
    R_a = R_a,
    R_b = R_b,
    p_a = p_a,
    N = N,
    alpha = alpha,
    alternative = alternative,
    sensitivity = sensitivity,
    specificity = specificity,
    overdispersion = overdispersion,
    nsims = nsims
  )
  ## TODO: look at this
  while (pwr_m1 > power) {
    m_1 <- round(m_1 / 2)
    pwr_m1 <- relR_power_simulated(
      m = m_1,
      R_a = R_a,
      R_b = R_b,
      p_a = p_a,
      N = N,
      alpha = alpha,
      alternative = alternative,
      sensitivity = sensitivity,
      specificity = specificity,
      overdispersion = overdispersion,
      nsims = nsims
    )
  }

  m <-
    samplesize_binopt(
      m_0,
      m_1,
      R_a  = R_a,
      R_b = R_b,
      p_a = p_a,
      N = N,
      alpha = alpha,
      alternative = alternative,
      power = power,
      sensitivity = sensitivity,
      specificity = specificity,
      overdispersion = overdispersion,
      nsims = nsims,
      tolerance = tolerance
    )


  if (m > N) {
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



samplesize_binopt <-
  function(m_0,
           m_1,
           R_a,
           R_b,
           p_a,
           N,
           alpha,
           alternative,
           power,
           sensitivity,
           specificity,
           overdispersion,
           nsims = 100000,
           tolerance = 10) {
    m_2 <- round(mean(c(m_0, m_1)))

    if (abs(m_0 - m_1) < tolerance) {
      return(m_2)
    }

    p_2 <-
      relR_power_simulated(
        m = m_2,
        R_a = R_a,
        R_b = R_b,
        p_a = p_a,
        N = N,
        alpha = alpha,
        alternative = alternative,
        sensitivity = sensitivity,
        specificity = specificity,
        overdispersion = overdispersion,
        nsims = nsims
      )
    p_0 <-
      relR_power_simulated(
        m = m_0,
        R_a = R_a,
        R_b = R_b,
        p_a = p_a,
        N = N,
        alpha = alpha,
        alternative = alternative,
        sensitivity = sensitivity,
        specificity = specificity,
        overdispersion = overdispersion,
        nsims = nsims
      )
    p_1 <-
      relR_power_simulated(
        m = m_1,
        R_a = R_a,
        R_b = R_b,
        p_a = p_a,
        N = N,
        alpha = alpha,
        alternative = alternative,
        sensitivity = sensitivity,
        specificity = specificity,
        overdispersion = overdispersion,
        nsims = nsims
      )

    if (power > max(p_0, p_1, p_2)) {
      m_0 <- max(m_0, m_1, m_2)
      m_1 <- max(m_0, m_1, m_2) + tolerance
      return(
        samplesize_binopt(
          m_0,
          m_1,
          R_a,
          R_b,
          p_a,
          N,
          alpha,
          alternative,
          power,
          sensitivity,
          specificity,
          overdispersion,
          nsims,
          tolerance
        )
      )
    } else if ((power < p_2 &
                power > p_0) | (power < p_0 & power > p_2)) {
      return(
        samplesize_binopt(
          m_0,
          m_2,
          R_a,
          R_b,
          p_a,
          N,
          alpha,
          alternative,
          power,
          sensitivity,
          specificity,
          overdispersion,
          nsims,
          tolerance
        )
      )
    } else {
      return(
        samplesize_binopt(
          m_2,
          m_1,
          R_a,
          R_b,
          p_a,
          N,
          alpha,
          alternative,
          power,
          sensitivity,
          specificity,
          overdispersion,
          nsims,
          tolerance
        )
      )
    }

  }
