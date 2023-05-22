##' Function that does the simple derived sample size calculation with
##' no corrections. I.e., directly applies the math as if sensitivity
##' and specificity are perfect.
##'
##' @template R_a
##' @template R_b
##' @template p_a
##' @template N
##' @template alpha
##' @template alternative
##' @template power
##' @template overdispersion
##' @param allow_impossible_m Logical. Indicates whether a value for `m` can be
##'    returned that is greater than the input `N`. Default: `FALSE`.
##'
##' @return The required sample size. `NA` if larger than `N`.
##'
relR_samplesize_basic <- function(R_a,
                                  R_b,
                                  p_a,
                                  N,
                                  alpha = 0.05,
                                  alternative = c("two_sided", "less", "greater"),
                                  power = 0.8,
                                  overdispersion = NULL,
                                  allow_impossible_m = FALSE) {
  check_positive(R_a)
  check_positive(R_b)
  check_positive(N)
  check_proportion(p_a)
  check_proportion(alpha)
  check_proportion(power)

  alternative <- rlang::arg_match(alternative)

  if (alternative == "two_sided") {
    alpha <- alpha / 2
  }

  ##calculate ratio
  p_b <- 1 - p_a
  d <- p_b / p_a

  ## do the samplesize calculation

  if (is.null(overdispersion)) {
    m  <- 1 / 2 * (sqrt(
      4 * (1 + R_a / (R_b * d ^ 2)) *
        (stats::qnorm(1 - alpha) + stats::qnorm(power)) ^
        2 * (N - 1) /
        (log(R_b / R_a) ^ 2 * R_a * p_a ^ 2) + 1
    ) + 1)

  } else {
    k_limit <-
      (2 * (stats::qnorm(1 - alpha) + stats::qnorm(power)) ^ 2 /
         log(R_b / R_a) ^ 2)
    if (overdispersion < k_limit) {
      if (allow_impossible_m) {
        return(2 * N)
      } else {
      cli::cli_abort(
        c(
          "You need a bigger {.var overdispersion}",
          "i" = "{.var overdispersion} must be at least {round(k_limit, 0)}",
          "x" = "You input {.var overdispersion} = {overdispersion}"
        )
      )
      }
    }

    m  <- 1 / 2 * (sqrt(4 * (1 + R_a / (R_b * d ^ 2)) *
                          (1 / (
                            log(R_b / R_a) ^ 2 /
                              (stats::qnorm(1 - alpha) + stats::qnorm(power)) ^
                              2 -
                              2 / overdispersion
                          )) *
                          ((N - 1) / (R_a * p_a ^ 2)) + 1) + 1)
  }

  if (m > N && !allow_impossible_m) {
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
