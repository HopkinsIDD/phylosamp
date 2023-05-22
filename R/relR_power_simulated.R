##' Simulate power using Poisson distributions
##'
##' @param m the sample size.
##' @template R_a
##' @template R_b
##' @template p_a
##' @template N
##' @template alpha
##' @template alternative
##' @template sensitivity
##' @template specificity
##' @template overdispersion
##' @param nsims Numeric. The number of simulations. Default: 100000
##'
##' @return Simulated power
relR_power_simulated <- function(m,
                                 R_a,
                                 R_b,
                                 p_a,
                                 N,
                                 alpha = 0.05,
                                 alternative = c("two_sided", "less", "greater"),
                                 sensitivity = 1,
                                 specificity = 1,
                                 overdispersion = NULL,
                                 nsims = 100000) {
  check_proportion(p_a)
  check_positive(N)
  check_positive(R_a)
  check_positive(R_b)
  check_proportion(alpha)
  check_proportion(sensitivity)
  check_proportion(specificity)
  check_positive(nsims)

  alternative <- rlang::arg_match(alternative)

  ## calculate ratio
  p_b <- 1 - p_a
  d <- p_b / p_a

  ## Get updated R values based on linkage error

  r_astar <- update_R_linkerr(
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

  ## TODO what if R_a > R_b?
  if (((r_astar > r_bstar) &
       (R_a < R_b)) | ((r_astar < r_bstar) & (R_a > R_b))) {
    cli::cli_abort("Based on your input parameters it is not possible to complete this study.")
  }

  if (!is.null(overdispersion) && overdispersion != Inf) {
    k <- overdispersion
    R_a <- stats::rgamma(nsims, shape = k, scale = R_a / k)
    R_b <- stats::rgamma(nsims, shape = k, scale = R_b / k)
  } else {
    k <- Inf
  }

  ## simulate the number of like edges in each group from Poisson distributions
  eaa <- stats::rpois(nsims,
                      (R_a * p_a ^ 2 * m * (m - 1) / (N - 1)) *
                        (sensitivity + specificity - 1) +
                        ((m * p_a * (m * p_a - 1)) / 2) * (1 - specificity))
  ebb <- stats::rpois(nsims,
                      (R_b * p_b ^ 2 * m * (m - 1) / (N - 1)) *
                        (sensitivity + specificity - 1) +
                        ((m * p_b * (m * p_b - 1)) / 2) * (1 - specificity))


  ## We are ignoring cases where the number of edges in either group is 0
  eaa <- ifelse(eaa == 0, NA, eaa)
  ebb <- ifelse(ebb == 0, NA, ebb)
  ## TODO: do we need to warn if this is deleting a large proportion? like if
  ## almost all are 0 it would make the sample size we are proposing (and the
  ## power) unlikely Or maybe check whether the lambdas are less than 5 or
  ## something?

  se <- sqrt((1 / ebb) + (1 / eaa) + 2 / k)

  if (alternative == "two_sided") {
    p <-
      mean(
        (log(ebb / eaa) - log(d ^ 2) - stats::qnorm(1 - alpha / 2) * se > 0) |
          (log(ebb / eaa) - log(d ^ 2) + stats::qnorm(1 - alpha / 2) * se < 0),
        na.rm = TRUE)
  } else if (alternative == "less") {
    p <-
      mean(log(ebb / eaa) - log(d ^ 2) + stats::qnorm(1 - alpha) * se < 0,
           na.rm = TRUE)
  } else if (alternative == "greater") {
    p <-
      mean(log(ebb / eaa) - log(d ^ 2) - stats::qnorm(1 - alpha) * se > 0,
           na.rm = TRUE)
  }
  return(p)
}
