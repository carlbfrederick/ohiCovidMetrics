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
#' @param test_targets data.frame produced by \code{process_test_targets}
#' @param outfile  file name (including path) for output data file
#'
#' @return invisibly returns the combined data
#' @export
#'
#' @importFrom readr write_csv
#' @importFrom dplyr full_join
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate_if
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr between
#' @importFrom dplyr if_else
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
  out <- dplyr::full_join(out, test_targets, by = c("Date", "Region_ID", "Region", "RowType"))

  #Any data cleaning necessary?
  ##convert factors to character for file check
  out <- out %>%
    dplyr::mutate_if(is.factor, as.character)

  ##ADD FIELD DESCRIBING TIME PERIOD OF DATA
  out$Data_Period <- paste(format(min(out$Date), "%x"), "-", format(max(hosp$Date), "%x"))

  #Force ILI_Moving_Avg to 0 instead of slightly zero
  out <- out %>%
    dplyr::mutate(
      ILI_Moving_Avg = dplyr::if_else(dplyr::between(ILI_Moving_Avg, left = -1e-6, right = 1e-6), 0, ILI_Moving_Avg)
    )

  ##Only keep necessary variables
  out <- out %>%
    dplyr::select(Data_Period, Date, Region_ID, Region, RowType,
           Conf_Case_Count, Conf_Case_Burden,
           Conf_Case_Burden_Class, Conf_Case_Burden_Critical_Flag,
           Conf_Case_Trajectory, Conf_Case_Trajectory_P, Conf_Case_Trajectory_Class,
           Conf_Case_Composite_Class,
           Testing_Total_Specimens, Testing_Positive_Specimens,
           Testing_Nonpositive_Specimens, Testing_Percent_Positive,
           Testing_Composite_Class,
           Testing_Case, Testing_ARI, Testing_Case_Gap,
           Testing_Target_0.2, Testing_Target_0.4, Testing_Target_0.6, Testing_Target_0.8, Testing_Target_1,
           Hosp_dailyCOVID_px, Hosp_COVID_px_Trajectory, Hosp_COVID_px_Trajectory_Class,
           Hosp_dailyCOVID_ICUpx, Hosp_COVID_ICUpx_Trajectory, Hosp_COVID_ICUpx_Trajectory_Class,
           Hosp_totalbeds, Hosp_beds_IBA, Hosp_PrctBeds_Used, Hosp_Beds_moving_avg,
           Hosp_totalICU, Hosp_ICU_IBA, Hosp_PrctICU_Used, Hosp_ICU_moving_avg,
           Hosp_total_vents, Hosp_num_px_vent, Hosp_PrctVent_Used, Hosp_Vent_moving_avg,
           CLI_Count, CLI_Burden, CLI_Burden_Class,
           CLI_Trajectory, CLI_Trajectory_P, CLI_Trajectory_Class, CLI_Composite_Class,
           ILI_Total_Visits, ILI_Visits, ILI_Percent, ILI_Moving_Avg,
           ILI_Baseline, ILI_Threshold, ILI_Status,
           ED_flag, Mayo_flag)

  ##make sure ED_flag and Mayo_flags are correct
  out <- out %>%
    dplyr::group_by(Region) %>%
    dplyr::mutate(
      across(c("ED_flag", "Mayo_flag"), ~ first(.x[!is.na(.x)]))
    )

  #ADD IN SOME BASIC CHECKS/REPORTING SO PEOPLE CAN GET SUMMARY
  tdir <- tempdir()
  save(out, file = file.path(tdir, "__tmp_metric_file.RData"))
  file_checks <- tinytest::run_test_file(system.file("check-combined-metric-file.R", package = "ohiCovidMetrics"),
                                         set_env = list("LOADCOMBOMETRICFILE" = file.path(tdir,  "__tmp_metric_file.RData")))
  checks_df <- as.data.frame(file_checks)
  checks_df$info <- sapply(file_checks, function(x) attr(x, which = "info"))

  #Write out a .csv
  message("Writing file to ", outfile, na = "")
  write_csv(out, outfile, na = "")

  if (sum(!checks_df$result) == 0) {
    message("You are good to go, all file checks passed!")
  } else if (sum(!checks_df$result) > 0) {
    warning("Oh no! ", sum(!checks_df$result), " data file checks failed. See list below for conditions that failed:\n - ",
            paste0(checks_df$info[!checks_df$result], collapse = "\n - "))
  }

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

#' Append combined metric data onto the data source
#'
#' @param current_combo_file path to .csv file produced by \code{\link{merge_metric_files}}
#' @param existing_combo_file path to .csv file of all passed combo files
#' @param overwrite logical indicating if function should overwrite existing_combo_file with new combined file
#'
#' @return invisibly returns the combined data. Also it saves existing_combo_file with a datestamp
#'         for the archives and saves the output over existing_combo_file
#' @export
#'
#' @importFrom readr write_csv
#' @importFrom readr read_csv
#' @importFrom dplyr bind_rows
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr arrange
#' @importFrom dplyr summarize
#' @importFrom dplyr across
#' @importFrom dplyr select
#' @importFrom dplyr left_join
#' @importFrom dplyr filter
#' @importFrom dplyr intersect
#' @importFrom dplyr setdiff
#' @importFrom dplyr all_of
#' @importFrom stringr str_extract
#' @importFrom zoo rollapply
#' @importFrom readr cols
#' @importFrom readr col_character
#' @importFrom readr col_date
#' @importFrom readr col_double
#'
#' @examples
#' \dontrun{
#'   #add examples to me please,
#' }
append_metric_files <- function(current_combo_file, existing_combo_file, overwrite = FALSE) {

  colspec_c <- readr::col_cols(
    Data_Period = readr::col_character(),
    Date = readr::col_date(format = ""),
    Region_ID = readr::col_character(),
    Region = readr::col_character(),
    RowType = readr::col_character(),
    Conf_Case_Count = readr::col_double(),
    Conf_Case_Burden = readr::col_double(),
    Conf_Case_Burden_Class = readr::col_character(),
    Conf_Case_Burden_Critical_Flag = readr::col_double(),
    Conf_Case_Trajectory = readr::col_character(),
    Conf_Case_Trajectory_P = readr::col_double(),
    Conf_Case_Trajectory_Class = readr::col_character(),
    Conf_Case_Composite_Class = readr::col_character(),
    Testing_Total_Specimens = readr::col_double(),
    Testing_Positive_Specimens = readr::col_double(),
    Testing_Nonpositive_Specimens = readr::col_double(),
    Testing_Percent_Positive = readr::col_double(),
    Testing_Composite_Class = readr::col_character(),
    Testing_Case = readr::col_double(),
    Testing_ARI = readr::col_double(),
    Testing_Case_Gap = readr::col_double(),
    Testing_Target_0.2 = readr::col_double(),
    Testing_Target_0.4 = readr::col_double(),
    Testing_Target_0.6 = readr::col_double(),
    Testing_Target_0.8 = readr::col_double(),
    Testing_Target_1 = readr::col_double(),
    Hosp_dailyCOVID_px = readr::col_double(),
    Hosp_COVID_px_Trajectory = readr::col_character(),
    Hosp_COVID_px_Trajectory_Class = readr::col_character(),
    Hosp_dailyCOVID_ICUpx = readr::col_double(),
    Hosp_COVID_ICUpx_Trajectory = readr::col_character(),
    Hosp_COVID_ICUpx_Trajectory_Class = readr::col_character(),
    Hosp_totalbeds = readr::col_double(),
    Hosp_beds_IBA = readr::col_double(),
    Hosp_PrctBeds_Used = readr::col_double(),
    Hosp_Beds_moving_avg = readr::col_double(),
    Hosp_totalICU = readr::col_double(),
    Hosp_ICU_IBA = readr::col_double(),
    Hosp_PrctICU_Used = readr::col_double(),
    Hosp_ICU_moving_avg = readr::col_double(),
    Hosp_total_vents = readr::col_double(),
    Hosp_num_px_vent = readr::col_double(),
    Hosp_PrctVent_Used = readr::col_double(),
    Hosp_Vent_moving_avg = readr::col_double(),
    CLI_Count = readr::col_double(),
    CLI_Burden = readr::col_double(),
    CLI_Burden_Class = readr::col_character(),
    CLI_Trajectory = readr::col_character(),
    CLI_Trajectory_P = readr::col_double(),
    CLI_Trajectory_Class = readr::col_character(),
    CLI_Composite_Class = readr::col_character(),
    ILI_Total_Visits = readr::col_double(),
    ILI_Visits = readr::col_double(),
    ILI_Percent = readr::col_double(),
    ILI_Moving_Avg = readr::col_double(),
    ILI_Baseline = readr::col_double(),
    ILI_Threshold = readr::col_double(),
    ILI_Status = readr::col_character(),
    ED_flag = readr::col_double(),
    Mayo_flag = readr::col_double()
  )

  colspec_e <- cols(
    Data_Period = readr::col_character(),
    Date = readr::col_date(format = ""),
    Region_ID = readr::col_character(),
    Region = readr::col_character(),
    RowType = readr::col_character(),
    Conf_Case_Count = readr::col_double(),
    Conf_Case_Count_moving_avg = readr::col_double(),
    Conf_Case_Burden = readr::col_double(),
    Conf_Case_Burden_Class = readr::col_character(),
    Conf_Case_Burden_Critical_Flag = readr::col_double(),
    Conf_Case_Trajectory = readr::col_character(),
    Conf_Case_Trajectory_P = readr::col_double(),
    Conf_Case_Trajectory_Class = readr::col_character(),
    Conf_Case_Composite_Class = readr::col_character(),
    Testing_Total_Specimens = readr::col_double(),
    Testing_Tot_Spec_moving_avg = readr::col_double(),
    Testing_Positive_Specimens = readr::col_double(),
    Testing_Nonpositive_Specimens = readr::col_double(),
    Testing_Percent_Positive = readr::col_double(),
    Testing_Perc_Pos_moving_avg = readr::col_double(),
    Testing_Composite_Class = readr::col_character(),
    Testing_Case = readr::col_double(),
    Testing_ARI = readr::col_double(),
    Testing_Case_Gap = readr::col_double(),
    Testing_Target_0.2 = readr::col_double(),
    Testing_Target_0.4 = readr::col_double(),
    Testing_Target_0.6 = readr::col_double(),
    Testing_Target_0.8 = readr::col_double(),
    Testing_Target_1 = readr::col_double(),
    Hosp_dailyCOVID_px = readr::col_double(),
    Hosp_DailyCOVID_PX_moving_avg = readr::col_double(),
    Hosp_COVID_px_Trajectory = readr::col_character(),
    Hosp_COVID_px_Trajectory_Class = readr::col_character(),
    Hosp_dailyCOVID_ICUpx = readr::col_double(),
    Hosp_DailyCOVID_ICU_moving_avg = readr::col_double(),
    Hosp_COVID_ICUpx_Trajectory = readr::col_character(),
    Hosp_COVID_ICUpx_Trajectory_Class = readr::col_character(),
    Hosp_totalbeds = readr::col_double(),
    Hosp_beds_IBA = readr::col_double(),
    Hosp_PrctBeds_Used = readr::col_double(),
    Hosp_Beds_moving_avg = readr::col_double(),
    Hosp_totalICU = readr::col_double(),
    Hosp_ICU_IBA = readr::col_double(),
    Hosp_PrctICU_Used = readr::col_double(),
    Hosp_ICU_moving_avg = readr::col_double(),
    Hosp_total_vents = readr::col_double(),
    Hosp_num_px_vent = readr::col_double(),
    Hosp_PrctVent_Used = readr::col_double(),
    Hosp_Vent_moving_avg = readr::col_double(),
    CLI_Count = readr::col_double(),
    CLI_Count_moving_avg = readr::col_double(),
    CLI_Burden = readr::col_double(),
    CLI_Burden_Class = readr::col_character(),
    CLI_Trajectory = readr::col_character(),
    CLI_Trajectory_P = readr::col_double(),
    CLI_Trajectory_Class = readr::col_character(),
    CLI_Composite_Class = readr::col_character(),
    ILI_Total_Visits = readr::col_double(),
    ILI_Visits = readr::col_double(),
    ILI_Percent = readr::col_double(),
    ILI_Moving_Avg = readr::col_double(),
    ILI_Baseline = readr::col_double(),
    ILI_Threshold = readr::col_double(),
    ILI_Status = readr::col_character(),
    ED_flag = readr::col_double(),
    Mayo_flag = readr::col_double()
  )

  #Read and combine files
  message("  Reading files and calculating moving averages ...")
  combo <- dplyr::bind_rows(
    readr::read_csv(current_combo_file, col_types = colspec_c),
    readr::read_csv(existing_combo_file, col_type = colspec_e)
  )

  ma_tmp <- combo %>%
    dplyr::filter(RowType == "Daily") %>%
    dplyr::mutate(
      pervar = as.Date(stringr::str_extract(Data_Period, "[0-9]+\\/[0-9]+\\/[0-9]+$"), format = "%m/%d/%Y")
    ) %>%
    dplyr::group_by(RowType, Region, Date) %>%
    dplyr::arrange(RowType, Region, Date, desc(pervar)) %>%
    dplyr::summarize(
      across(c(Hosp_beds_IBA, Hosp_totalbeds,
               Hosp_ICU_IBA, Hosp_totalICU,
               Hosp_num_px_vent, Hosp_total_vents,
               Hosp_dailyCOVID_px, Hosp_dailyCOVID_ICUpx,
               Testing_Positive_Specimens,
               Testing_Total_Specimens,
               CLI_Count,
               Conf_Case_Count,
               ILI_Visits, ILI_Total_Visits), first),
      .groups = "drop"
    ) %>%
    dplyr::group_by(RowType, Region) %>%
    dplyr::mutate(
      dplyr::across(c(Hosp_beds_IBA, Hosp_totalbeds,
                      Hosp_ICU_IBA, Hosp_totalICU,
                      Hosp_num_px_vent, Hosp_total_vents,
                      Hosp_dailyCOVID_px, Hosp_dailyCOVID_ICUpx,
                      Testing_Positive_Specimens,
                      Testing_Total_Specimens,
                      CLI_Count,
                      Conf_Case_Count),
             zoo::rollapply, width = 7, FUN = sum, fill = NA, align = "right"),
      dplyr::across(c(ILI_Visits, ILI_Total_Visits),
             zoo::rollapply, width = 3, FUN = sum, fill = NA, align = "right")
    ) %>%
    dplyr::mutate(
      Hosp_Beds_moving_avg           = 100 * (1 - (Hosp_beds_IBA/Hosp_totalbeds)),
      Hosp_ICU_moving_avg            = 100 * (1 - (Hosp_ICU_IBA/Hosp_totalICU)),
      Hosp_Vent_moving_avg           = 100 * (Hosp_num_px_vent/Hosp_total_vents),
      Hosp_DailyCOVID_PX_moving_avg  = Hosp_dailyCOVID_px / 7,
      Hosp_DailyCOVID_ICU_moving_avg = Hosp_dailyCOVID_ICUpx / 7,
      Testing_Perc_Pos_moving_avg    = 100 * (Testing_Positive_Specimens/Testing_Total_Specimens),
      Testing_Tot_Spec_moving_avg    = Testing_Total_Specimens / 7,
      CLI_Count_moving_avg           = CLI_Count / 7,
      Conf_Case_Count_moving_avg     = Conf_Case_Count / 7,
      ILI_Moving_Avg                 = 100 * (ILI_Visits / ILI_Total_Visits)
    ) %>%
    dplyr::select(RowType, Region, Date,
                  Hosp_Beds_moving_avg,
                  Hosp_ICU_moving_avg,
                  Hosp_Vent_moving_avg,
                  Hosp_DailyCOVID_PX_moving_avg,
                  Hosp_DailyCOVID_ICU_moving_avg,
                  Testing_Perc_Pos_moving_avg,
                  Testing_Tot_Spec_moving_avg,
                  CLI_Count_moving_avg,
                  Conf_Case_Count_moving_avg,
                  ILI_Moving_Avg)

  cols2rm <- dplyr::setdiff(
    dplyr::intersect(names(ma_tmp),
                     names(combo)),
    c("RowType", "Region", "Date"))

  out <- combo %>%
    dplyr::select(-all_of(cols2rm)) %>%
    dplyr::left_join(ma_tmp, by = c("RowType", "Region", "Date")) %>%
    dplyr::mutate(
      ILI_Status = dplyr::case_when(
        ILI_Moving_Avg >= ILI_Threshold ~ "Elevated",
        ILI_Moving_Avg >= ILI_Baseline & ILI_Moving_Avg < ILI_Threshold ~ "Moderate",
        ILI_Moving_Avg <  ILI_Baseline ~ "Low",
        TRUE ~ "NA"
      )

    ) %>%
    dplyr::select(Data_Period, Date, Region_ID, Region, RowType,
                  Conf_Case_Count, Conf_Case_Count_moving_avg, Conf_Case_Burden,
                  Conf_Case_Burden_Class, Conf_Case_Burden_Critical_Flag,
                  Conf_Case_Trajectory, Conf_Case_Trajectory_P, Conf_Case_Trajectory_Class,
                  Conf_Case_Composite_Class,
                  Testing_Total_Specimens, Testing_Tot_Spec_moving_avg, Testing_Positive_Specimens,
                  Testing_Nonpositive_Specimens, Testing_Percent_Positive, Testing_Perc_Pos_moving_avg,
                  Testing_Composite_Class,
                  Testing_Case, Testing_ARI, Testing_Case_Gap,
                  Testing_Target_0.2, Testing_Target_0.4, Testing_Target_0.6, Testing_Target_0.8, Testing_Target_1,
                  Hosp_dailyCOVID_px, Hosp_DailyCOVID_PX_moving_avg, Hosp_COVID_px_Trajectory, Hosp_COVID_px_Trajectory_Class,
                  Hosp_dailyCOVID_ICUpx, Hosp_DailyCOVID_ICU_moving_avg, Hosp_COVID_ICUpx_Trajectory, Hosp_COVID_ICUpx_Trajectory_Class,
                  Hosp_totalbeds, Hosp_beds_IBA, Hosp_PrctBeds_Used, Hosp_Beds_moving_avg,
                  Hosp_totalICU, Hosp_ICU_IBA, Hosp_PrctICU_Used, Hosp_ICU_moving_avg,
                  Hosp_total_vents, Hosp_num_px_vent, Hosp_PrctVent_Used, Hosp_Vent_moving_avg,
                  CLI_Count, CLI_Count_moving_avg, CLI_Burden, CLI_Burden_Class,
                  CLI_Trajectory, CLI_Trajectory_P, CLI_Trajectory_Class, CLI_Composite_Class,
                  ILI_Total_Visits, ILI_Visits, ILI_Percent, ILI_Moving_Avg,
                  ILI_Baseline, ILI_Threshold, ILI_Status,
                  ED_flag, Mayo_flag)

  tdir <- tempdir()
  save(out, file = file.path(tdir, "__tmp_append_file.RData"))
  file_checks <- tinytest::run_test_file(system.file("check-appended-file.R", package = "ohiCovidMetrics"),
                                         set_env = list("LOADAPPENDEDFILE" = file.path(tdir,  "__tmp_append_file.RData")))
  checks_df <- as.data.frame(file_checks)
  checks_df$info <- sapply(file_checks, function(x) attr(x, which = "info"))

  if (sum(!checks_df$result) == 0) {
    message("You are good to go, all file checks passed!")
  } else if (sum(!checks_df$result) > 0) {
    warning("Oh no! ", sum(!checks_df$result), " data file checks failed. See list below for conditions that failed:\n - ",
            paste0(checks_df$info[!checks_df$result], collapse = "\n - "))
  }

  if (overwrite) {
    message("  Overwriting ", existing_combo_file, "...")
    readr::write_csv(out, existing_combo_file, na = "")
  } else {
    message("Not writing file because overwrite = FALSE")
  }

  return(
    invisible(
      list(merged_file = out,
           file_checks = checks_df)
    )
  )

}

#' Utility to standardize geographic names
#'
#' @param reg vector of geographic names
#'
#' @return a vector of standardized geographic names
std_region <- function(reg) {
  out <- ifelse(reg == "Saint Croix", "St. Croix", sub("HERC\\|", "", reg))
  out <- ifelse(out == "Fond Du Lac", "Fond du Lac", out)
  out <- ifelse(grepl("La\\s?[Cc]rosse", out), "La Crosse", out)
  out <- ifelse(out == "Fox valley", "Fox Valley Area", out)
  out <- ifelse(out == "NorthCentral", "North Central", out)
  out <- ifelse(out == "SouthCentral", "South Central", out)

}

#' Utility to standardize geographic id's
#'
#' @param reg_id vector of geographic ids
#'
#' @return a vector of standardized geographic ids
std_region_id <- function(reg_id) {
  sub("HERC\\|", "", reg_id)
}
