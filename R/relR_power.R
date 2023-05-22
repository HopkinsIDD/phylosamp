##' Function to calculate the power given a sample size. This is the top
##' level function to be called to calculate power given a sample size m
##' and a proportion sampled.
##'
##' @param m the sample size.
##' @template R_a
##' @template R_b
##' @template p_a
##' @template N
##' @template rho
##' @template alpha
##' @template alternative
##' @template sensitivity
##' @template specificity
##' @template overdispersion
##'
##' @return The power given `m`
##'
relR_power <- function(m,
                       R_a,
                       R_b,
                       p_a,
                       N = NULL,
                       rho = NULL,
                       alpha = 0.05,
                       alternative = c("two_sided", "less", "greater"),
                       sensitivity = 1,
                       specificity = 1,
                       overdispersion = NULL) {

  if (is.null(N) && is.null(rho)) {
    cli::cli_abort("You must specify one of `N` or `rho`.")
  } else if (is.null(N)) {
    N <- m / rho
  } else if (is.null(rho)) {
    rho <- N * m
  } else {
    cli::cli_abort("You may specify only one of `N` or `rho`.")
  }
  check_positive(m)
  check_positive(R_a)
  check_positive(R_b)
  check_proportion(p_a)
  check_proportion(sensitivity)
  check_proportion(specificity)

  alternative <- rlang::arg_match(alternative)

  p_b <- 1 - p_a
  d <- p_b / p_a

  ## Update R for sensitivity and specificity

  r_a <- update_R_linkerr(
    R = R_a,
    p = p_a,
    N = N,
    m = m,
    sensitivity = sensitivity,
    specificity = specificity
  )

  r_b <- update_R_linkerr(
    R = R_b,
    p = p_b,
    N = N,
    m = m,
    sensitivity = sensitivity,
    specificity = specificity
  )

  ## check if the direction is flipped by sensitivity / specificity
  if (((R_a > R_b) & (r_a < r_b)) | ((R_a < R_b) & (r_a > r_b))) {
    cli::cli_warn("Based on your input parameters it is not possible to complete this study.")
    return(NA)
  }

  if (is.null(overdispersion)) {
    k <- Inf
  } else {
    k <- overdispersion
  }

  if (alternative == "two_sided") {
    sig <- stats::qnorm(1 - alpha / 2)
    p <- stats::pnorm(sig,
                      mean = log(r_b / r_a) /
                        sqrt((1 + r_a / (d ^ 2 * r_b)) * (N - 1) / (r_a * p_a ^ 2 * m * (m - 1)) + 2 / k),
                      lower.tail = FALSE) +
      stats::pnorm(-sig,
                   mean = log(r_b / r_a) /
                     sqrt((1 + r_a / (d ^ 2 * r_b)) * (N - 1) / (r_a * p_a ^ 2 * m * (m - 1))) + 2 / k)
  } else if (alternative == "greater") {
    sig <- stats::qnorm(1 - alpha)
    p <- stats::pnorm(sig,
                      mean = log(r_b / r_a) /
                        sqrt((1 + r_a / (d ^ 2 * r_b)) * (N - 1) / (r_a * p_a ^ 2 * m * (m - 1)) + 2 / k),
                      lower.tail = FALSE)
  } else if (alternative == "less") {
    sig <- stats::qnorm(alpha)
    p <- stats::pnorm(sig,
                      mean = log(r_b / r_a) /
                        sqrt((1 + r_a / (d ^ 2 * r_b)) * (N - 1) / (r_a * p_a ^ 2 * m * (m - 1)) + 2 / k),
                      lower.tail = TRUE)
  }
  return(p)
}
