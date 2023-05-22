check_proportion <- function(p) {
  name_p <- rlang::enexpr(p)
  if (p < 0 | p > 1) {
    cli::cli_abort(c("`{name_p}` must be between 0 and 1.",
                     "x" = "You input `{name_p}` = {p}"),
                   call = NULL)
  }
}

check_m_N <- function(m, N) {
  if (m > N) {
    cli::cli_warn(
      c(
        "Necessary sample size not achievable.",
        "i" = "The necessary sample size given the input parameters is {round(m)}",
        "x" = "You input {.var N} = {N} as the total size of the outbreak"
      )
    )
    return(NA)
  }
}
check_positive <- function(x) {
  name_x <- rlang::enexpr(x)
  if (x <= 0) {
    cli::cli_abort(c("`{name_x}` must be positive.",
                     "x" = "You input `{name_x}` = {x}"),
                   call = NULL)
  }
}
