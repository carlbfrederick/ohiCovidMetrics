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
  if (!is.na(val) & val < 0 & !is.character(val)) {
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

#' Calculates rolling weeks backward from the end_date.
#'
#' The purpose is to group a vector of dates into 7 day weeks
#' backward in time. For example, if the end date was 2020-06-09,
#' the days 2020-06-03 through 2020-06-09 would be one week,
#' 2020-05-27 through 2020-06-02 would be another week, 2020-05-20
#' through 2020-05-26 another and so on. The function will work even
#' if the vector of dates is missing one or more dates in the series.
#'
#' @param date_vector the vector of dates to group into weeks. Must be
#'                    in Date format or something that can be coerced to
#'                    Date by \code{\link[base]{as.Date}}.
#' @param end_date the day on which to start counting backwards. Defaults to
#'                 the current date, i.e. \code{Sys.Date}.
#'
#' @return a vector of weeks counting backward in time. The current week is
#'         numbered 1, the previous is numbered 2, etc. The week farthest in
#'         the past will currently return NA values so that you would mistakenly
#'         aggregate a week that may not be the full 7 days.
#'
#' @importFrom lubridate weeks
#' @importFrom lubridate days
#'
#' @export
#'
#' @examples
#' #General
#' date_vector1 <- seq.Date(from = as.Date("2020-01-01"), to = as.Date("2020-06-09"), by = 1)
#' data.frame(date = date_vector1,
#'            weeknum = rolling_week(date_vector = date_vector1,
#'                                   end_date = as.Date(Sys.Date())))
#'
#' #In a tidyverse pipe
#' tibble(tv_date = date_vector1) %>%
#'   mutate(
#'     tv_weeknum = rolling_week(tv_date, end_Date = as.Date(Sys.Date()))
#'   )
rolling_week <- function(date_vector, end_date = as.Date(Sys.Date())){
  #Coerce to dates
  date_vector <- as.Date(date_vector)
  end_date = as.Date(end_date)

  min_date <- min(date_vector)

  period_length <- as.numeric(difftime(end_date, min_date))
  period_weeks <- floor(period_length / 7)

  week_ends <-  c(end_date + lubridate::days(1), end_date - lubridate::weeks(1:period_weeks))

  out <- period_weeks - cut(date_vector, breaks = week_ends, include_lowest = TRUE, right = TRUE, label = FALSE) + 1

  return(out)
}

#' Clean up reversals in daily count time series
#'
#' This function was written to overcome the fact that the Covid Historical
#' Data Table has negative counts on a few days in the time series because
#' of changes in the underlying data. The function fixes that by subtracting
#' the negative count from the prior positive day's count.
#'
#' If there are two or more negative counts in a row, the function subtracts
#' the sum of the negative count from the most recent day with a positive
#' count. It will issue a warning if this cleaning algorithm results in a
#' daily time series that still has negative counts.
#'
#' This algorithm ensures that the total cumulative count after the cleaning
#' is equal to what it was originally.
#'
#' @param daily_time_series a time series of count data in chronological order
#'
#' @return a vector of counts of the same length as the original
#' @export
#'
#' @importFrom dplyr tibble
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom stats diffinv
#'
#'
#' @examples
#' \dontrun{
#'   suppressPackageStartupMessages(library(dplyr))
#'
#'   pull_histTable() %>%
#'     group_by(fips) %>%
#'     mutate(
#'       tests_daily_clean = clean_reversals(tests_daily)
#'     )
#' }
clean_reversals <- function(daily_time_series) {
  if (sum(is.na(daily_time_series)) > 0) {
    stop("The vector of counts you supplied contains missing data, please fix this and try again.")
  }

  #identify negative value pattern, start and stop indexs to replace
  neg_pattern <- rle(sign(daily_time_series))

  if (all(neg_pattern$value > -1)) {
    return(daily_time_series)
  }

  start_pos <- stats::diffinv(neg_pattern$lengths, xi = 1)
  end_pos <- stats::diffinv(neg_pattern$lengths, xi = 0)

  start_pos <- start_pos[-length(start_pos)]
  end_pos <- end_pos[-1]

  np_tbl <- dplyr::tibble(
    start = start_pos,
    end = end_pos,
    sign = neg_pattern$values,
    length = neg_pattern$lengths
    ) %>%
    dplyr::filter(sign == -1) %>%
    dplyr::mutate(
      replace_position = end + (sign * length)
    )

  #for each instance of negative counts, replace the negatives with zero and subtract from most recent non-negative value
  for (i in seq(1, nrow(np_tbl), by = 1)) {
    total_backup <- sum(daily_time_series[seq(np_tbl$start[i], np_tbl$end[i], by = 1)])
    daily_time_series[seq(np_tbl$start[i], np_tbl$end[i], by = 1)] <- 0
    daily_time_series[np_tbl$replace_position[i]] <- daily_time_series[np_tbl$replace_position[i]] + total_backup
    rm(total_backup)
  }

  #warn if this introduced other negative counts
  if (any(daily_time_series < 0)) {
    warning("cleaning reversals introduced additional reversals (negative daily change).\n",
            "---------------------------------------------------------------------------\n",
            "  You can re-run this cleaning step to try and correct it, but this indicates\n",
            "  there was a serious data issue that resulted in a reversal that was greater\n",
            "  than the magnitude of the prior day's total.")
  }

  daily_time_series
}
