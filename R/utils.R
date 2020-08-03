#' Checks that input values are non-negative, non-missing integers
#'
#' @param val value
#' @param arg.name argument name for warning/error message.
#' @param verbose a logical where FALSE means messages are suppressed. Default = TRUE
#'
#' @return value
check_nonneg <- function(val, arg.name, verbose = TRUE) {
  if (any(is.na(val))) {
    stop("<<", arg.name, ">> has 1 or more missing values, please check your data.")
  }
  if (all(!is.na(val)) && any(val < 0) && !is.character(val)) {
    stop("<<", arg.name, ">> has one ore more negative values, please check your data.")
  }
  if (!is.integer(val)) {
    if (verbose) {
      message("<<", arg.name, ">> is not an integer.\n",
              "  I am trying to coerce the value to integer, but this might cause unintended issues.\n",
              "  See ?as.integer for coercion behavior.")
    }
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
#' suppressPackageStartupMessages(library(dplyr))
#'
#' date_vector1 <- seq.Date(from = as.Date("2020-01-01"), to = as.Date("2020-06-09"), by = 1)
#' data.frame(date = date_vector1,
#'            weeknum = rolling_week(date_vector = date_vector1,
#'                                   end_date = Sys.Date()))
#'
#' #In a tidyverse pipe
#' tibble(tv_date = date_vector1) %>%
#'   mutate(
#'     tv_weeknum = rolling_week(tv_date, end_date = Sys.Date())
#'   )
rolling_week <- function(date_vector, end_date = as.Date(Sys.Date())){
  #Coerce to dates
  date_vector <- as.Date(date_vector, tz = "America/Chicago")
  end_date = as.Date(end_date)

  min_date <- min(date_vector)

  period_length <- as.numeric(difftime(end_date, min_date)) + 1
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
#' @param verbose a logical where FALSE means messages are suppressed. Default = TRUE
#'
#' @return a vector of counts of the same length as the original
#' @export
#'
#' @importFrom dplyr tibble
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom stats diffinv
#' @importFrom rlang .data
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
clean_reversals <- function(daily_time_series, verbose = TRUE) {
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
    "start" = start_pos,
    "end" = end_pos,
    "sign" = neg_pattern$values,
    "length" = neg_pattern$lengths
    ) %>%
    dplyr::filter(sign == -1) %>%
    dplyr::mutate(
      replace_position = .data$end + (.data$sign * .data$length)
    )

  #for each instance of negative counts, replace the negatives with zero and subtract from most recent non-negative value
  for (i in seq(1, nrow(np_tbl), by = 1)) {
    total_backup <- sum(daily_time_series[seq(np_tbl$start[i], np_tbl$end[i], by = 1)])
    daily_time_series[seq(np_tbl$start[i], np_tbl$end[i], by = 1)] <- 0
    daily_time_series[np_tbl$replace_position[i]] <- daily_time_series[np_tbl$replace_position[i]] + total_backup
    rm(total_backup)
  }

  #warn if this introduced other negative counts
  if (any(daily_time_series < 0) && verbose) {
    message("cleaning reversals introduced additional reversals (negative daily change).\n",
            "---------------------------------------------------------------------------\n",
            "  You can re-run this cleaning step to try and correct it, but this indicates\n",
            "  there was a serious data issue that resulted in a reversal that was greater\n",
            "  than the magnitude of the prior day's total.")
  }

  daily_time_series
}

#' Ensure that all units of observation have all dates within the max and min
#' of the date variable.
#'
#' @param df the data.frame to fill in
#' @param grouping_vars a character vector with the variable
#'        names that define the grouping structure that will
#'        uniquely identify the relavant units of observation
#' @param date_var name of the date variable to be filled in
#'
#' @return the same data.frame with any required dates filled in for each
#'         unit. Variables without grouping_vars will be missing.
#' @export
#'
#' @importFrom dplyr distinct
#' @importFrom dplyr select
#' @importFrom dplyr ungroup
#' @importFrom dplyr full_join
#' @importFrom dplyr tibble
#' @importFrom dplyr all_of
#'
#' @examples
#' library(dplyr)
#'
#' set.seed(123)
#' tmp <- tibble(County = sample(LETTERS[1:3], 15, replace = TRUE),
#'               Date = sample(seq(as.Date("2020-07-01"), as.Date("2020-07-07"), by = 1),
#'                             15, replace = TRUE)) %>%
#'   distinct(.) %>%
#'   mutate(
#'     outcome_var = sample(2:10, size = nrow(.), replace = TRUE)
#'   ) %>%
#'   arrange(County, Date)
#'
#' #Compare
#'
#' tmp
#'
#' #To
#'
#' arrange(fill_dates(tmp, grouping_vars = "County", date_var = "Date"),
#'         County, Date)
#'
fill_dates <- function(df, grouping_vars, date_var) {
  min_date <- as.Date(min(df[[date_var]], na.rm=TRUE), tz = "America/Chicago")
  max_date <- as.Date(max(df[[date_var]], na.rm=TRUE), tz = "America/Chicago")

  #Grab lists of variables names
  other_vars <- setdiff(names(df), c(grouping_vars, date_var))
  merge_by <- c(grouping_vars, "alldates")
  names(merge_by) <- c(grouping_vars, date_var)

  skeleton_df <- dplyr::full_join(
    dplyr::distinct(dplyr::select(dplyr::ungroup(df), dplyr::all_of(grouping_vars))),
    dplyr::tibble(alldates = seq(min_date, max_date, by = 1)),
    by = character()
  )

  #missing indicators
  df <- df %>%
    dplyr::mutate(dplyr::across(other_vars, is.na, .names = "miss_{col}"))

  #merge and fill all missing with zeros
  out <- dplyr::full_join(df, skeleton_df, by = merge_by) %>%
    dplyr::mutate(dplyr::across(other_vars, ~ifelse(is.na(.x), 0, .x))) %>%
    dplyr::mutate(dplyr::across(paste("miss", other_vars, sep = "_"), ~ifelse(is.na(.x), FALSE, .x)))

  #replace missing values
  for (var in other_vars) {
    out[[var]][out[[paste("miss", var, sep = "_")]]] <- NA
    out[[paste("miss", var, sep = "_")]] <- NULL
  }

  return(out)
}

#' Combine metric data into a single output file
#'
#' @param case     data.frame produced by \code{\link{process_confirmed_cases}}
#' @param hosp     data.frame produced by \code{\link{process_hospital}}
#' @param test     data.frame produced by \code{\link{process_testing}}
#' @param cli      data.frame produced by \code{\link{process_cli}}
#' @param ili      data.frame produced by \code{\link{process_ili}}
#' @param test_targets data.frame produced by \code{\link{process_test_targets}}
#' @param outfile  file name (including path) for output data file
#'
#' @return invisibly returns the combined data
#' @export
#'
#' @importFrom readr write_csv
#' @importFrom dplyr full_join
#' @importFrom tinytest run_test_file
#'
#' @examples
#' \dontrun{
#'   #add examples to me please,
#' }
merge_metric_files <- function(case, hosp, test, cli, ili, test_targets, outfile) {
  #Start with Cases and Testing
  out <- dplyr::full_join(case, test, by = c("Date", "Region_ID", "Region", "RowType"))
  #Add in Hospitalization
  out <- dplyr::full_join(out, hosp, by = c("Date", "Region_ID", "Region", "RowType"))
  #Add in CLI
  out <- dplyr::full_join(out, cli, by = c("Date", "Region_ID", "Region", "RowType"))
  #Add in ILI
  out <- dplyr::full_join(out, ili, by = c("Date", "Region_ID", "Region", "RowType"))
  #Add in Testing Targets
  out <- dplyr::full_join(out, ili, by = c("Date", "Region_ID", "Region", "RowType"))

  #Any data cleaning necessary?

  #ADD FIELD DESCRIBING TIME PERIOD OF DATA
  out$Data_Period <- paste(format(min(out$Date), "%x"), "-", format(max(hosp$Date), "%x"))

  #ADD IN SOME BASIC CHECKS/REPORTING SO PEOPLE CAN GET SUMMARY
  file_checks <- tinytest::run_test_file(system.file("check-combined-metric-file.R", package = "ohiCovidMetrics"))
  checks_df <- as.data.frame(file_checks)
  checks_df$info <- sapply(file_checks, function(x) attr(x, which = "info"))

  #Write out a .csv
  message("Writing file to ", outfile, na = "")
  write_csv(out, outfile, na = "")

  return(
    invisible(
      list(merged_file = out,
           file_checks = checks_df)
    )
  )
}

#' Customize ESSENCE API query
#'
#' @param url ESSENCE API url to pull either ILI or CLI data
#' @param start_date start date for daily time series
#' @param end_date end date for daily time series5
#'
#' @return a list of API calls with corrected dates
#'
#' @importFrom stringr str_replace
#'
#' @examples
#' \dontrun{
#'   #example here
#' }
essence_query <- function(url, start_date, end_date){
  url %>%
    stringr::str_replace(pattern = 'endDate=.+?&',
                         replacement = paste('endDate=', trimws(format(end_date, "%e%b%Y")), "&", sep = "")) %>%
    stringr::str_replace(pattern = 'startDate=.+?&',
                         replacement = paste('startDate=', trimws(format(start_date, "%e%b%Y")), "&", sep = ""))
}

#' Call ESSENCE API and ingest response as data.frame
#'
#' Requires proper setup of ESSENCE credentials via the
#' \code{\link[keyring]{keyring}} package. See internal
#' DHS documentation about how to do that.
#'
#' @inheritParams essence_query
#'
#' @return a data.frame from ESSENCE
#'
#' @importFrom keyring key_list
#' @importFrom keyring key_get
#' @importFrom httr GET
#' @importFrom httr authenticate
#' @importFrom httr content
#' @importFrom dplyr %>%
#' @importFrom readr cols
#' @importFrom readr col_character
#' @importFrom readr col_time
#' @importFrom readr col_datetime
#' @importFrom readr col_double
#'
#' @examples
#' \dontrun{
#'   #example here
#' }
essence_data <- function(url) {
  essence_cols <- readr::cols(
    Time = col_time(format = ""),
    C_Visit_Date_Time = col_datetime(format = ""),
    Age = col_double(),
    HospitalZip = col_double(),
    Arrived_Date_Time = col_datetime(format = ""),
    Initial_Pulse_Oximetry_Calc = col_double(),
    Hospital = col_double(),
    HalfHour = col_double(),
    SiteID = col_double(),
    Year = col_double(),
    StagingRowID = col_double(),
    Message_ID = col_double(),
    Create_Processed_Date_Time = col_datetime(format = ""),
    Create_Raw_Date_Time = col_datetime(format = ""),
    Update_Processed_Date_Time = col_datetime(format = ""),
    Create_ER_Import_Date_Time = col_datetime(format = ""),
    Site_ID = col_double(),
    Recorded_Date_Time = col_datetime(format = ""),
    HasBeenE = col_double(),
    HasBeenI = col_double(),
    HasBeenO = col_double(),
    DDAvailable = col_double(),
    DDInformative = col_double(),
    CCAvailable = col_double(),
    CCInformative = col_double(),
    Create_ER_Base_Date_Time = col_datetime(format = ""),
    Create_Cache_ER_Base_Date_Time_Web = col_datetime(format = ""),
    CCOrig_Length = col_double(),
    CCParsed_Length = col_double(),
    DD_Length = col_double(),
    .default = col_character()
  )

  httr::GET(url, httr::authenticate(keyring::key_list("essence")$username,
                                    keyring::key_get("essence", key_list("essence")$username))) %>%
    httr::content(type = "text/csv", col_types = essence_cols)
}
