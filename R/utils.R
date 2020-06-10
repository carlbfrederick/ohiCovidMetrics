#' Checks that input values are non-negative, non-missing integers
#'
#' @param val value
#' @param arg.name argument name for warning/error message.
#'
#' @return value
check_nonneg <- function(val, arg.name) {
  if (is.na(val)) {
    stop("<<", arg.name, ">> is missing, please check your data.")
  }
  if (!is.na(val) & val < 0) {
    stop("<<", arg.name, ">> is negative, please check your data.")
  }
  if (!is.integer(val)) {
    warning("<<", arg.name, ">> is not an integer.\n",
            "  I am trying to coerce the value to integer, but this might cause unintended issues.\n",
            "  See ?as.integer for coercion behavior.")
    return(as.integer(val))
  }
  return(val)
}
