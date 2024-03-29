#' Reshape confirmed case data for producing Tableau extracts
#'
#' take case data (from WEDSS or historical data table) and put it in proper
#' shape for metric tables to feed tableau
#'
#' @inheritParams process_confirmed_cases
#'
#' @return a list of data.frames. The "summary" data.frame has one row per
#' county, state, and HERC regions with the following columns
#' \describe{
#'   \item{fips}{FIPS Code and/or region identifier}
#'   \item{geo_name}{Name of geography}
#'   \item{pop_2020}{2020 Population from NCHS}
#'   \item{case_weekly_1}{Total cases for \strong{current} 7 day period}
#'   \item{case_weekly_2}{Total cases for \strong{prior} 7 day period}
#'   \item{week_end_1}{End date for \strong{current} 7 day period}
#'   \item{week_end_2}{End date for \strong{prior} 7 day period}
#' }
#' and the "daily" data.frame has one row per
#' county, state, and HERC region per day for the two week period with
#' the following columns
#' \describe{
#'   \item{fips}{FIPS Code and/or region identifier}
#'   \item{geo_name}{Name of geography}
#'   \item{post_date}{Date cases were confirmed}
#'   \item{case_daily}{Count of cases confirmed that day}
#' }
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr summarize
#' @importFrom dplyr filter
#' @importFrom dplyr summarize_at
#' @importFrom dplyr select
#' @importFrom tidyr pivot_wider
#' @importFrom rlang .data
#' @importFrom lubridate days
#'
#' @examples
#' \dontrun{
#'   hdt <- pull_histTable()
#'   hdt_clean <- shape_case_data(hdt)
#' }
shape_case_data <- function(case_df) {
  #Alter date to reflect date cases were confirmed rather than posted.
  case_df$post_date = case_df$post_date

  max_date <- max(case_df$post_date)

  cases_daily <- case_df %>%
    dplyr::filter(post_date >= (max_date - lubridate::days(13))) %>%
    dplyr::select(fips, geo_name, post_date, case_daily)

  cases_summary <- case_df %>%
    dplyr::group_by(.data$fips, .data$geo_name, .data$pop_2020) %>%
    dplyr::mutate(
      weeknum = rolling_week(date_vector = .data$post_date, end_date = max_date)
    ) %>%
    dplyr::group_by(.data$fips, .data$geo_name, .data$pop_2020, .data$weeknum) %>%
    dplyr::summarize(
      case_weekly = as.integer(sum(.data$case_daily)),
      week_end = max(.data$post_date),
      .groups = "drop_last"
    ) %>%
    dplyr::filter(.data$weeknum <= 2) %>%
    tidyr::pivot_wider(id_cols = c("fips", "geo_name", "pop_2020"),
                       values_from = c("case_weekly", "week_end"),
                       names_from = "weeknum")

  #Re-type pop to integer
  cases_summary$pop_2020 <- as.integer(cases_summary$pop_2020)

  list(summary = cases_summary,
       daily = cases_daily)
}


#' Process the shaped confirmed case data.frame into a Tableau ready format
#'
#' @param case_df Confirmed case data.frame (e.g. produced by \link{pull_histTable})
#' @param crit_cat flag indicating whether we want to include a Critically high
#'                 category in the output file (DEFAULT TRUE)
#'
#' @return a Tableau ready data.frame with the following columns:
#' \describe{
#'   \item{Date}{Date cases were confirmed }
#'   \item{Region_ID}{ID code for geographic unit (FIPS for county and state)}
#'   \item{Region}{Name of geographic unit (county, state, HERC region)}
#'   \item{RowType}{Are row values summary or daily values}
#'   \item{Conf_Case_Count}{Count of confirmed cases for period}
#'   \item{Conf_Case_Burden}{see \code{\link{score_burden}}}
#'   \item{Conf_Case_Trajectory}{see \code{\link{score_trajectory}}}
#'   \item{Conf_Case_Burden_Class}{see \code{\link{class_burden}}}
#'   \item{Conf_Case_Trajectory_Class}{see \code{\link{class_trajectory}}}
#'   \item{Conv_Case_Composite_Class}{see \code{\link{confirmed_case_composite}}}
#'   \item{Conf_Case_Trajectory_P}{see \code{\link{pval_trajectory}}}
#'   \item{Conf_Case_Trajectory_FDR}{see \code{\link{fdr_trajectory}}}
#' }
#'
#' @export
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr ungroup
#' @importFrom dplyr if_else
#' @importFrom dplyr bind_rows
#' @importFrom rlang .data
#'
#' @examples
#' library(dplyr)
#'
#' output <- pull_histTable() %>%
#'   process_confirmed_cases()
process_confirmed_cases <- function(case_df, crit_cat = TRUE) {
  clean_case_df <- shape_case_data(case_df)

  out_day <- clean_case_df$daily %>%
    dplyr::mutate(
      RowType = "Daily"
    ) %>%
    dplyr::select(Date = .data$post_date,
                  Region_ID = .data$fips,
                  Region = .data$geo_name,
                  RowType,
                  Conf_Case_Count = .data$case_daily)

  out_sum <- dplyr::ungroup(clean_case_df$summary) %>%
    dplyr::mutate(
      Count = .data$case_weekly_1 + .data$case_weekly_2,
      Burden = score_burden(curr = .data$case_weekly_1,
                            prev = .data$case_weekly_2,
                            pop = .data$pop_2020),
      Burden_Class = class_burden(.data$Burden),
      Trajectory = score_trajectory(curr = .data$case_weekly_1,
                                    prev = .data$case_weekly_2),
      Trajectory_P = pval_trajectory(curr = .data$case_weekly_1,
                                     prev = .data$case_weekly_2),
      Trajectory_Class = class_trajectory(traj = .data$Trajectory,
                                          pval = .data$Trajectory_P),
      Trajectory_FDR = fdr_trajectory(pval = .data$Trajectory_P),
      Composite_Class = confirmed_case_composite(traj_class = .data$Trajectory_Class,
                                                 burd_class = .data$Burden_Class)

    ) %>%
    dplyr::mutate(
      Trajectory = round(.data$Trajectory, 1),
      Trajectory = dplyr::if_else(.data$Trajectory_Class == "No significant change", "N/A",
                                  as.character(.data$Trajectory)),
      Burden_Very_high_Flag = dplyr::if_else(Burden >= 350, 1L, 0L),
      Burden = round(.data$Burden, 1),
      RowType = "Summary"
    ) %>%
    dplyr::select(
      Date = .data$week_end_1,
      Region_ID = .data$fips,
      Region = .data$geo_name,
      RowType = .data$RowType,
      Conf_Case_Count = .data$Count,
      Conf_Case_Burden = .data$Burden,
      Conf_Case_Burden_Very_high_Flag = .data$Burden_Very_high_Flag,
      Conf_Case_Trajectory = .data$Trajectory,
      Conf_Case_Burden_Class = .data$Burden_Class,
      Conf_Case_Trajectory_Class = .data$Trajectory_Class,
      Conf_Case_Composite_Class = .data$Composite_Class,
      Conf_Case_Trajectory_P = .data$Trajectory_P,
      Conf_Case_Trajectory_FDR = .data$Trajectory_FDR
    )

  #Remove Critically high category unless it is wanted
  ccbc_lvl <- levels(out_sum$Conf_Case_Burden_Class)
  cccc_lvl <- levels(out_sum$Conf_Case_Composite_Class)

  if (!crit_cat) {
    out_sum <- out_sum %>%
      dplyr::mutate(
        Conf_Case_Burden_Class = dplyr::case_when(
          Conf_Case_Burden_Class == "Critically high" ~ ordered(5, levels = 1:6, labels = ccbc_lvl),
          TRUE ~ Conf_Case_Burden_Class),
        Conf_Case_Composite_Class = dplyr::case_when(
          Conf_Case_Composite_Class == "Critically high" ~ ordered(4, levels = 1:5, labels = cccc_lvl),
          TRUE ~ Conf_Case_Composite_Class)
      )
  }

  dplyr::bind_rows(out_sum, out_day)
}

#' Shape EM Resource summary data for metric calculations
#'
#' @inheritParams process_hospital
#'
#' @return a list of data.frames. The "summary" data.frame has one row per
#' county, state, and HERC regions with the following columns
#' \describe{
#'   \item{Run_Date}{Date hospital report was run}
#'   \item{RowType}{Are row values summary or daily values}
#'   \item{fips}{FIPS Code and/or region identifier}
#'   \item{geo_name}{Name of geography}
#'   \item{pop_2020}{2020 Population from NCHS}
#'   \item{covid_reg_weekly_1}{Hospitalized Covid cases for \strong{current} 7 day period}
#'   \item{covid_reg_weekly_2}{Hospitalized Covid cases for \strong{prior} 7 day period}
#'   \item{covid_icu_weekly_1}{ICU Covid cases for \strong{current} 7 day period}
#'   \item{covid_icu_weekly_2}{ICU Covid cases for \strong{prior} 7 day period}
#'   \item{week_end_1}{End date for \strong{current} 7 day period}
#'   \item{week_end_2}{End date for \strong{prior} 7 day period}
#' }
#' and the "daily" data.frame has one row per state and HERC region (\emph{not county})
#' per day for the two week period with the following columns
#' \describe{
#'   \item{County}{Name of geography}
#'   \item{Report_Date}{Date hospital information was reported}
#'   \item{dailyCOVID_px}{}
#'   \item{dailyCOVID_ICUpx}{}
#'   \item{geo_type}{Type of geography - all missing should be removed}
#'   \item{fips}{FIPS Code and/or region identifier}
#'   \item{pop_2020}{2020 Population from NCHS}
#'   \item{RowType}{Are row values summary or daily values}
#'   \item{totalbeds}{Total Beds (ICU, Intermediate, Med/Surg, Neg. Flow)}
#'   \item{beds_IBA}{Total Immediate Beds Available (ICU, Intermediate, Med/Surg, Neg. Flow)}
#'   \item{totalICU}{Total ICU Beds}
#'   \item{ICU_IBA}{Immediate ICU Beds Available}
#'   \item{num_px_vent}{Current Number of Ventilated Patients (Induvated and mechanically ventilated)}
#'   \item{total_vents}{Total rented/owned/demoed general use ventilators on hand}
#'   \item{intermed_beds_IBA}{Immediate Intermediate Care Beds Available}
#'   \item{negflow_beds_IBA}{Immediate Negative Airflow Isolation Beds Available}
#'   \item{medsurg_beds_IBA}{Immediate Medical/Surgical Beds Available}
#'   \item{PrctBeds_IBA}{Hosp_beds_IBA / Hosp_totalbeds}
#'   \item{PrctICU_IBA}{Hosp_ICU_IBA / Hosp_ICU_IBA}
#'   \item{PrctVent_Used}{Hosp_num_px_vent / Hosp_total_vents}
#'   \item{Run_Date}{Date hospital report was run}
#' }
#'
#' @importFrom lubridate days
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#' @importFrom dplyr group_by
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr summarize
#' @importFrom dplyr rename
#'
#' @examples
#' \dontrun{
#'   #Add examples here
#' }
shape_hospital_data <- function(hosp_df) {
  #Find max date for weekly calculations
  max_date <- max(hosp_df$Report_Date)

  hosp_daily <- dplyr::filter(hosp_df,
                              RowType == "Daily",
                              Report_Date >= max_date - lubridate::days(13))

  hosp_summary <- hosp_df %>%
    dplyr::filter(RowType == "Summary") %>%
    dplyr::group_by(Run_Date, RowType, fips, County, pop_2020) %>%
    dplyr::arrange(Report_Date) %>%
    dplyr::mutate(
      weeknum = rolling_week(date_vector = Report_Date, end_date = max_date)
    ) %>%
    dplyr::group_by(Run_Date, RowType, fips, County, pop_2020, weeknum) %>%
    dplyr::summarize(
      covid_reg_weekly = as.integer(sum(dailyCOVID_px)),
      covid_icu_weekly = as.integer(sum(dailyCOVID_ICUpx)),
      week_end = max(Report_Date)
    ) %>%
    dplyr::filter(weeknum <= 2) %>%
    tidyr::pivot_wider(id_cols = c("Run_Date", "RowType", "fips", "County", "pop_2020"),
                       values_from = c("covid_reg_weekly", "covid_icu_weekly", "week_end"),
                       names_from = c("weeknum")) %>%
    dplyr::rename(geo_name = "County")

  out <- list(summary = hosp_summary,
              daily = hosp_daily)

  return(out)
}

#' Process the shaped hospital data into a Tableau ready format
#'
#' @param hosp_df data.frame output by \code{\link{pull_hospital}}
#'
#' @return a Tableau ready data.frame with the following columns:
#' \describe{
#'   \item{Hosp_RunDate}{Date hospital report was run}
#'   \item{Date}{Date hospital information was reported}
#'   \item{Region}{Name of geographic unit (county, state, HERC region)}
#'   \item{Region_ID}{ID code for geographic unit (FIPS for county and state)}
#'   \item{RowType}{Are row values summary or daily values}
#'   \item{Hosp_dailyCOVID_px}{Daily count of COVID patients}
#'   \item{Hosp_dailyCOVID_ICUpx}{Daily count of COVID patients in the ICU}
#'   \item{Hosp_totalbeds}{Total Beds (ICU, Intermediate, Med/Surg, Neg. Flow)}
#'   \item{Hosp_beds_IBA}{Total Immediate Beds Available (ICU, Intermediate, Med/Surg, Neg. Flow)}
#'   \item{Hosp_totalICU}{Total ICU Beds}
#'   \item{Hosp_ICU_IBA}{Immediate ICU Beds Available}
#'   \item{Hosp_num_px_vent}{Current Number of Ventilated Patients (Induvated and mechanically ventilated)}
#'   \item{Hosp_total_vents}{Total rented/owned/demoed general use ventilators on hand}
#'   \item{Hosp_intermed_beds_IBA}{Immediate Intermediate Care Beds Available}
#'   \item{Hosp_negflow_beds_IBA}{Immediate Negative Airflow Isolation Beds Available}
#'   \item{Hosp_medsurg_beds_IBA}{Immediate Medical/Surgical Beds Available}
#'   \item{Hosp_PrctBeds_IBA}{Hosp_beds_IBA / Hosp_totalbeds}
#'   \item{Hosp_PrctICU_IBA}{Hosp_ICU_IBA / Hosp_ICU_IBA}
#'   \item{Hosp_PrctVent_Used}{Hosp_num_px_vent / Hosp_total_vents}
#'   \item{Hosp_COVID_px_Trajectory}{Trajectory for Hospitalized COVID patient count (see \code{\link{score_trajectory}})}
#'   \item{Hosp_COVID_px_Trajectory_Class}{Trajectory for Hospitalized COVID patient count (see \code{\link{class_trajectory}})}
#'   \item{Hosp_COVID_ICUpx_Trajectory}{Trajectory for ICU COVID patient count (see \code{\link{score_trajectory}})}
#'   \item{Hosp_COVID_ICUpx_Trajectory_Class}{Trajectory for ICU COVID patient count (see \code{\link{class_trajectory}})}
#'   \item{Hosp_PrctBeds_Used}{Complement of Hosp_PrctBeds_IBA}
#'   \item{Hosp_PrctICU_Used}{Complement of Hosp_PrctICU_IBA}
#'   \item{Hosp_Beds_moving_avg}{right-aligned 7 day rolling average of Hosp_PrctBeds_Used}
#'   \item{Hosp_ICU_moving_avg}{right-aligned 7 day rolling average of Hosp_PrctICU_Used}
#'   \item{Hosp_Vent_moving_avg}{right-aligned 7 day rolling average of Hosp_PrctVent_Used}
#' }
#'
#' @export
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom dplyr ungroup
#' @importFrom dplyr if_else
#' @importFrom dplyr everything
#' @importFrom dplyr bind_rows
#' @importFrom rlang .data
#' @importFrom lubridate days
#' @importFrom zoo rollapply
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' output <- pull_hospital("path-to-hospital-extract") %>%
#'   shape_hospital_data() %>%
#'   process_hospital()
#' }
process_hospital <- function(hosp_df) {

  clean_hosp_df <- shape_hospital_data(hosp_df)

  hosp_daily <- clean_hosp_df$daily %>%
    dplyr::select(RunDate = Run_Date,
                  Date = Report_Date,
                  Region = County,
                  Region_ID = fips,
                  RowType,
                  dailyCOVID_px,
                  dailyCOVID_ICUpx,
                  totalbeds,
                  beds_IBA,
                  totalICU,
                  ICU_IBA,
                  num_px_vent,
                  total_vents,
                  intermed_beds_IBA,
                  negflow_beds_IBA,
                  medsurg_beds_IBA,
                  PrctBeds_Used,
                  PrctICU_Used,
                  PrctVent_Used)

  hosp_summary <- dplyr::ungroup(clean_hosp_df$summary) %>%
    dplyr::mutate(

      COVID_px_Trajectory = score_trajectory(curr = .data$covid_reg_weekly_1,
                                             prev = .data$covid_reg_weekly_2),
      COVID_px_Trajectory_P = pval_trajectory(curr = .data$covid_reg_weekly_1,
                                              prev = .data$covid_reg_weekly_2),
      COVID_px_Trajectory_Class = class_trajectory(traj = .data$COVID_px_Trajectory,
                                                   pval = .data$COVID_px_Trajectory_P),

      COVID_ICUpx_Trajectory = score_trajectory(curr = .data$covid_icu_weekly_1,
                                                prev = .data$covid_icu_weekly_2),
      COVID_ICUpx_Trajectory_P = pval_trajectory(curr = .data$covid_icu_weekly_1,
                                                 prev = .data$covid_icu_weekly_2),
      COVID_ICUpx_Trajectory_Class = class_trajectory(traj = .data$COVID_ICUpx_Trajectory,
                                                      pval = .data$COVID_ICUpx_Trajectory_P)
    ) %>%
    dplyr::mutate(
      COVID_px_Trajectory = round(.data$COVID_px_Trajectory, 1),
      COVID_ICUpx_Trajectory = round(.data$COVID_ICUpx_Trajectory, 1),
      COVID_px_Trajectory = dplyr::if_else(.data$COVID_px_Trajectory_Class == "No significant change", "N/A",
                                           as.character(.data$COVID_px_Trajectory)),
      COVID_ICUpx_Trajectory = dplyr::if_else(.data$COVID_ICUpx_Trajectory_Class == "No significant change", "N/A",
                                              as.character(.data$COVID_ICUpx_Trajectory))
    ) %>%
    dplyr::select(
      Date = .data$week_end_1,
      Region_ID = .data$fips,
      Region = .data$geo_name,
      .data$COVID_px_Trajectory,
      .data$COVID_px_Trajectory_Class,
      .data$COVID_ICUpx_Trajectory,
      .data$COVID_ICUpx_Trajectory_Class,
      .data$RowType,
      RunDate = .data$Run_Date
    )

  out <- dplyr::bind_rows(hosp_daily, hosp_summary) %>%
    dplyr::select(Hosp_RunDate = RunDate,
                  Date,
                  Region,
                  Region_ID,
                  RowType,
                  Hosp_dailyCOVID_px = dailyCOVID_px,
                  Hosp_dailyCOVID_ICUpx = dailyCOVID_ICUpx,
                  Hosp_totalbeds = totalbeds,
                  Hosp_beds_IBA = beds_IBA,
                  Hosp_totalICU = totalICU,
                  Hosp_ICU_IBA = ICU_IBA,
                  Hosp_num_px_vent = num_px_vent,
                  Hosp_total_vents = total_vents,
                  Hosp_intermed_beds_IBA = intermed_beds_IBA,
                  Hosp_negflow_beds_IBA = negflow_beds_IBA,
                  Hosp_medsurg_beds_IBA = medsurg_beds_IBA,
                  Hosp_PrctBeds_Used = PrctBeds_Used,
                  Hosp_PrctICU_Used = PrctICU_Used,
                  Hosp_PrctVent_Used = PrctVent_Used,
                  Hosp_COVID_px_Trajectory = COVID_px_Trajectory,
                  Hosp_COVID_px_Trajectory_Class = COVID_px_Trajectory_Class,
                  Hosp_COVID_ICUpx_Trajectory = COVID_ICUpx_Trajectory,
                  Hosp_COVID_ICUpx_Trajectory_Class = COVID_ICUpx_Trajectory_Class) %>%
    dplyr::mutate(
      Hosp_Beds_moving_avg = zoo::rollapply(Hosp_PrctBeds_Used, 7, mean, fill = NA, align = "right"),
      Hosp_ICU_moving_avg = zoo::rollapply(Hosp_PrctICU_Used, 7, mean, fill = NA, align = "right"),
      Hosp_Vent_moving_avg = zoo::rollapply(Hosp_PrctVent_Used, 7, mean, fill = NA, align = "right")
    )
}

#' Shape Testing summary data for metric calculations
#'
#' @inheritParams process_testing
#'
#' @return a list of data.frames. The "summary" data.frame has one row per
#' county, state, and HERC region with the following columns
#' \describe{
#'   \item{Region_ID}{FIPS Code and/or region identifier}
#'   \item{Region}{Name of geography}
#'   \item{RowType}{Are row values summary or daily values}
#'   \item{Positives}{Count of positive specimens}
#'   \item{Negatives}{Count of negative specimens}
#'   \item{Total}{Positives + Negatives}
#'   \item{Date}{Summary Date for 2 week period}
#' }
#' and the "daily" data.frame has one row per county, state, and HERC region
#' per day for the two week period with the following columns
#' \describe{
#'   \item{Date}{Date test results came in}
#'   \item{Region_ID}{FIPS Code and/or region identifier}
#'   \item{Region}{Name of geography}
#'   \item{RowType}{Are row values summary or daily values}
#'   \item{Positives}{Count of positive specimens}
#'   \item{Negatives}{Count of negative specimens}
#'   \item{Total}{Positives + Negatives}
#' }
#'
#' @importFrom lubridate days
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize_if
#' @importFrom utils data
#'
#' @examples
#' \dontrun{
#'   #Add examples here
#' }
shape_testing_data <- function(testing_df) {
  utils::data("county_data")

  max_date <- max(testing_df$resultdate2)

  testing_daily <- dplyr::filter(testing_df, resultdate2 >= max_date - lubridate::days(13)) %>%
    dplyr::mutate(RowType = "Daily") %>%
    dplyr::rename(county = Area) %>%
    dplyr::left_join(county_data, by = "county") %>%
    dplyr::mutate(
      fips = dplyr::case_when(
        county == "Wisconsin" ~ "55",
        is.na(fips) ~ county,
        TRUE ~ fips
      )
    ) %>%
    dplyr::rename(Date = resultdate2,
                  Region = county,
                  Region_ID = fips) %>%
    group_by(Date, Region_ID, Region, RowType) %>%
    summarize(across(c("Positives", "Negatives", "Total"), sum),
              .groups = "drop")


  testing_summary <- testing_daily %>%
    dplyr::mutate(
      RowType = "Summary"
    ) %>%
    dplyr::group_by(Region_ID, Region, RowType) %>%
    dplyr::summarize_if(is.numeric, sum, na.rm = TRUE) %>%
    dplyr::mutate(
      Date = max_date
    )

  out <- list(summary = testing_summary,
              daily = testing_daily)
}

#' Process the shaped testing data into a Tableau ready format
#'
#' @param testing_df data.frame output by \code{\link{pull_testing}}
#'
#' @return a Tableau ready data.frame with the following columns:
#' \describe{
#'   \item{Date}{Date hospital information was reported}
#'   \item{Region}{Name of geographic unit (county, state, HERC region)}
#'   \item{Region_ID}{ID code for geographic unit (FIPS for county and state)}
#'   \item{RowType}{Are row values summary or daily values}
#'   \item{Testing_Positive_Specimens}{Count of specimens positive for COVID-19}
#'   \item{Testing_Nonpositive_Specimens}{Count of specimens not positive for COVID-19}
#'   \item{Testing_Total_Specimens}{Count of specimens}
#'   \item{Testing_Percent_Positive}{Percent specimens positive for COVID-19}
#'   \item{Testing_Incident_Tests}{Total number of incidents tested}
#'   \item{Testing_Incident_Test_Target}{Target number of incidents to test}
#'   \item{Testing_Percent_of_Target}{Percent incidents tested out of target (Testing_Incident_Tests / Testing_Incident_Test_Target)}
#'   \item{Testing_Composite_Class}{Summary metric for testing based on Testing_Percent_Positive and Testing_Percent_of_Target}
#' }
#'
#' @export
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom dplyr ungroup
#' @importFrom dplyr if_else
#' @importFrom dplyr everything
#' @importFrom dplyr bind_rows
#'
#' @examples
#' \dontrun{
#'   #write me an example
#' }
process_testing <- function(testing_df) {
  clean_testing_df <- shape_testing_data(testing_df)

  test_daily <- clean_testing_df$daily %>%
    dplyr::mutate(
      percent_positive = 100 * (Positives / Total),
    ) %>%
    dplyr::select(Date,
                  Region_ID,
                  Region,
                  RowType,
                  Testing_Total_Encounters = Total,
                  Testing_Positive_Encounters = Positives,
                  Testing_Negative_Encounters = Negatives,
                  Testing_Percent_Positive = percent_positive)

  #Calculate 7 day average for summary percent positive
  pct_pos <- test_daily %>%
    dplyr::filter(Date >= max(Date) - lubridate::days(6)) %>%
    dplyr::group_by(Region_ID) %>%
    dplyr::summarize(
      percent_positive = 100 * sum(Testing_Positive_Encounters) / sum(Testing_Total_Encounters)
    )

  test_summary <- clean_testing_df$summary %>%
    dplyr::inner_join(pct_pos, by = "Region_ID") %>%
    dplyr::mutate(
      percent_positive_class = dplyr::case_when(
        percent_positive >= 10.0                          ~ "High (more than 10% positive)",
        percent_positive >= 5.0 & percent_positive < 10.0 ~ "Moderate (5% to 10% positive)",
        percent_positive >= 0.0 & percent_positive < 5.0  ~ "Low (less than 5% positive)",
        TRUE                                              ~ "ERROR"
      ),
      testing_composite = dplyr::case_when(
        percent_positive_class == "Low (less than 5% positive)"   ~ "Low",
        percent_positive_class == "Moderate (5% to 10% positive)" ~ "Medium",
        percent_positive_class == "High (more than 10% positive)" ~ "High",
        TRUE                                                      ~ "ERROR"
      )
    ) %>%
    dplyr::select(Date,
                  Region,
                  Region_ID,
                  RowType,
                  Testing_Total_Encounters = Total,
                  Testing_Positive_Encounters = Positives,
                  Testing_Negative_Encounters = Negatives,
                  Testing_Percent_Positive = percent_positive,
                  Testing_Composite_Class = testing_composite)

  dplyr::bind_rows(test_summary,
                   test_daily)

}

#' Shape CLI data
#'
#' @inheritParams process_cli
#'
#' @return a list of data.frames. The "summary" data.frame has one row per
#' county, state, and HERC region with the following columns
#' \describe{
#'   \item{Visit_Date}{Date of emergency dept visit}
#'   \item{fips}{FIPS Code and/or region identifier}
#'   \item{County}{Name of geography}
#'   \item{RowType}{Are row values summary or daily values}
#'   \item{DailyED}{Count of CLI ED visits for the day}
#'   \item{}{}
#'   \item{}{}
#'   \item{}{}
#' }
#' and the "daily" data.frame has one row per county, state, and HERC region
#' per day for the two week period with the following columns
#' \describe{
#'   \item{fips}{FIPS Code and/or region identifier}
#'   \item{County}{Name of geography}
#'   \item{pop_2020}{2020 Population from NCHS}
#'   \item{WeeklyED_1}{Total CLI visits for \strong{current} 7 day period}
#'   \item{WeeklyED_2}{Total CLI visits for \strong{previous} 7 day period}
#'   \item{week_end_1}{End date for \strong{current} 7 day period}
#'   \item{week_end_2}{End date for \strong{prior} 7 day period}
#'   \item{RowType}{Are row values summary or daily values}
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#' @importFrom dplyr summarize
#' @importFrom dplyr filter
#' @importFrom tidyr pivot_wider
#'
#' @examples
#' \dontrun{
#'   #write me an example
#' }
shape_cli_data <- function(cli_df) {
  max_date <- max(cli_df$Visit_Date, na.rm = TRUE)

  cli_daily <- dplyr::filter(cli_df, Visit_Date >= max_date - lubridate::days(13)) %>%
    dplyr::mutate(
      RowType = "Daily"
    )

  cli_summary <- cli_df %>%
    dplyr::group_by(fips, County, pop_2020) %>%
    dplyr::arrange(Visit_Date) %>%
    dplyr::mutate(
      weeknum = rolling_week(date_vector = Visit_Date, end_date = max_date)
    ) %>%
    dplyr::group_by(fips, County, pop_2020, weeknum) %>%
    dplyr::summarize(
      WeeklyED = as.integer(sum(DailyED)),
      week_end = max(Visit_Date)
    ) %>%
    dplyr::filter(weeknum <= 2) %>%
    tidyr::pivot_wider(id_cols = c("fips", "County", "pop_2020"),
                       values_from = c("WeeklyED", "week_end"),
                       names_from = "weeknum") %>%
    dplyr::mutate(
      RowType = "Summary"
    )

  list(daily = cli_daily,
       summary = cli_summary)
}

#' Process the shaped CLI data into a Tableau ready format
#'
#' @param cli_df data.frame produced by \code{\link{pull_essence}} for
#'               CLI metrics
#' @inheritParams process_confirmed_cases
#'
#' @return a Tableau ready data.frame with the following columns:
#' \describe{
#'   \item{Date}{Date hospital information was reported}
#'   \item{Region_ID}{ID code for geographic unit (FIPS for county and state)}
#'   \item{Region}{Name of geographic unit (county, state, HERC region)}
#'   \item{RowType}{Are row values summary or daily values}
#'   \item{CLI_Count}{Count of CLI emergency department visits}
#'   \item{CLI_Burden}{see \code{\link{score_burden}}}
#'   \item{CLI_Trajectory}{see \code{\link{score_trajectory}}}
#'   \item{CLI_Burden_Class}{see \code{\link{class_burden}}}
#'   \item{CLI_Trajectory_Class}{see \code{\link{class_trajectory}}}
#'   \item{CLI_Composite_Class}{see \code{\link{confirmed_case_composite}}}
#'   \item{CLI_Trajectory_P}{see \code{\link{pval_trajectory}}}
#'   \item{CLI_Trajectory_FDR}{see \code{\link{fdr_trajectory}}}
#' }
#'
#' @export
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom dplyr ungroup
#' @importFrom dplyr if_else
#' @importFrom dplyr everything
#' @importFrom dplyr bind_rows
#' @importFrom dplyr across
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#'   #write me an example
#' }
process_cli <- function(cli_df, crit_cat = TRUE){
  clean_cli_df <- shape_cli_data(cli_df)

  cli_daily <- clean_cli_df$daily %>%
    dplyr::select(Date = Visit_Date,
                  Region_ID = fips,
                  Region = County,
                  RowType,
                  CLI_Count = DailyED)

  cli_summary <- dplyr::ungroup(clean_cli_df$summary) %>%
    dplyr::mutate(dplyr::across(c("WeeklyED_1", "WeeklyED_2", "pop_2020"), as.integer)) %>%
    dplyr::mutate(
      Count = .data$WeeklyED_1 + .data$WeeklyED_2,
      Burden = score_burden(curr = .data$WeeklyED_1,
                            prev = .data$WeeklyED_2,
                            pop = .data$pop_2020),
      Burden_Class = class_burden(.data$Burden),
      Trajectory = score_trajectory(curr = .data$WeeklyED_1,
                                    prev = .data$WeeklyED_2),
      Trajectory_P = pval_trajectory(curr = .data$WeeklyED_1,
                                     prev = .data$WeeklyED_2),
      Trajectory_Class = class_trajectory(traj = .data$Trajectory,
                                          pval = .data$Trajectory_P),
      Trajectory_FDR = fdr_trajectory(pval = .data$Trajectory_P),
      Composite_Class = confirmed_case_composite(traj_class = .data$Trajectory_Class,
                                                 burd_class = .data$Burden_Class)

    ) %>%
    dplyr::mutate(
      Trajectory = round(.data$Trajectory, 1),
      Trajectory = dplyr::if_else(.data$Trajectory_Class == "No significant change", "N/A",
                                  as.character(.data$Trajectory)),
      Burden = round(.data$Burden, 1),
      Date = .data$week_end_1 + lubridate::days(1)
    ) %>%
    dplyr::select(
      Date,
      Region_ID = .data$fips,
      Region = .data$County,
      RowType = .data$RowType,
      CLI_Count = .data$Count,
      CLI_Burden = .data$Burden,
      CLI_Trajectory = .data$Trajectory,
      CLI_Burden_Class = .data$Burden_Class,
      CLI_Trajectory_Class = .data$Trajectory_Class,
      CLI_Composite_Class = .data$Composite_Class,
      CLI_Trajectory_P = .data$Trajectory_P,
      CLI_Trajectory_FDR = .data$Trajectory_FDR
    )

  #Remove Critically high category unless it is wanted
  ccbc_lvl <- levels(cli_summary$CLI_Burden_Class)
  cccc_lvl <- levels(cli_summary$CLI_Composite_Class)

  if (!crit_cat) {
    cli_summary <- cli_summary %>%
      dplyr::mutate(
        CLI_Burden_Class = dplyr::case_when(
          CLI_Burden_Class == "Critically high" ~ ordered(5, levels = 1:6, labels = ccbc_lvl),
          TRUE ~ CLI_Burden_Class),
        CLI_Composite_Class = dplyr::case_when(
          CLI_Composite_Class == "Critically high" ~ ordered(4, levels = 1:5, labels = cccc_lvl),
          TRUE ~ CLI_Composite_Class)
      )
  }

  dplyr::bind_rows(cli_summary, cli_daily)
}

#' Shape ILI data
#'
#' @inheritParams process_ili
#'
#' @return a list of data.frames. The "daily" data.frame has one row per
#' county, state, and HERC region with the following columns
#' \describe{
#'   \item{Region}{Name of geography}
#'   \item{Region_ID}{FIPS Code and/or region identifier}
#'   \item{Date}{Date of emergency dept visit}
#'   \item{RowType}{Are row values summary or daily values}
#'   \item{Total_Visits}{Total ED visits for the day}
#'   \item{ILI_Visits}{Count of ILI ED visits for the day}
#'   \item{ILI_perc}{Percent of ED visits related to ILI}
#' }
#'
#' @importFrom lubridate days
#' @importFrom dplyr %>%
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#' @importFrom dplyr summarize
#' @importFrom dplyr filter
#' @importFrom dplyr case_when
#' @importFrom dplyr select
#' @importFrom dplyr if_else
#'
#' @examples
#' \dontrun{
#'   #write me an example
#' }
shape_ili_data <- function(ili_df) {
  max_date <- max(ili_df$Visit_Date, na.rm = TRUE)

  ili_out <- dplyr::filter(ili_df, Visit_Date >= max_date - lubridate::days(13)) %>%
    dplyr::mutate(
      ILI_perc = if_else(Total_Visits > 0, 100 * (ILI_Visits / Total_Visits), 0),
      RowType = "Daily"
    ) %>%
    dplyr::select(Region = County,
                  Region_ID = fips,
                  Date = Visit_Date,
                  RowType,
                  Total_Visits,
                  ILI_Visits,
                  ILI_perc)

  list(daily = ili_out)
}

#' Process the shaped ILI data into a Tableau ready format
#'
#' @param ili_df data.frame produced by \code{\link{pull_essence}} for
#'               ILI metrics
#' @param ili_threshold_path path to .csv file containing ILI thresholds
#'
#' @return a Tableau ready data.frame with the following columns:
#' \describe{
#'   \item{Region}{Name of geography}
#'   \item{Region_ID}{FIPS Code and/or region identifier}
#'   \item{Date}{Date of emergency dept visit}
#'   \item{RowType}{Are row values summary or daily values}
#'   \item{ED_Total_Visits}{Total visits to ED}
#'   \item{ED_ILI_Visits}{Count of ILI ED visits}
#'   \item{ILI_Percent}{Percent of ED visits related to ILI}
#'   \item{ILI_Baseline}{Baseline value for ILI}
#'   \item{ILI_Threshold}{Threshold value for ILI}
#'   \item{ILI_Status}{Summary ILI metric}
#'   \item{ILI_Moving_Avg}{right-aligned 7 day rolling average of ILI_Percent}
#' }
#'
#' @export
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr arrange
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr if_else
#' @importFrom dplyr bind_rows
#' @importFrom dplyr case_when
#' @importFrom zoo rollapply
#' @importFrom readr read_csv
#' @importFrom readr cols
#'
#' @examples
#' \dontrun{
#'   #write me an example
#' }
process_ili <- function(ili_df, ili_threshold_path) {
  clean_ili_df <- shape_ili_data(ili_df)

  ili_thresh <- readr::read_csv(ili_threshold_path,
                                col_types = readr::cols(
                                  Region = col_character(),
                                  ILI_avg = col_double(),
                                  ILI_sd = col_double(),
                                  SD_2 = col_double(),
                                  SD_4 = col_double()
                                )) %>%
  dplyr::mutate(
    Region = dplyr::case_when(
      grepl("North[ Cc]+entral", Region) ~ "North Central",
      grepl("South[ Cc]+entral", Region) ~ "South Central",
      grepl("Fox", Region) ~ "Fox Valley Area",
      TRUE ~ Region
    )
  )

  ili_daily  <- clean_ili_df$daily %>%
    dplyr::left_join(ili_thresh, by = "Region")%>%
    dplyr::group_by(Region) %>%
    dplyr::arrange(Region, Date) %>%
    dplyr::mutate(
      ILI_baseline = ILI_avg + SD_2,
      ILI_threshold = ILI_avg + SD_4,
      Over_baseline = dplyr::if_else(ILI_perc >= ILI_baseline, 1L, 0L),
      Over_threshold = dplyr::if_else(ILI_perc >= ILI_threshold, 1L, 0L),
      moving_avg = zoo::rollapply(ILI_Visits, 3, mean, fill = NA, align = "right") /
                   zoo::rollapply(Total_Visits, 3, mean, fill = NA, align = "right"),
      Status = dplyr::case_when(
        moving_avg >= ILI_threshold ~ "Elevated",
        moving_avg >= ILI_baseline & moving_avg < ILI_threshold ~ "Moderate",
        moving_avg <  ILI_baseline ~ "Low",
        TRUE ~ "NA"
      )
    ) %>%
    dplyr::select(Region,
                  Region_ID,
                  Date,
                  RowType,
                  ED_Total_Visits = Total_Visits,
                  ED_ILI_Visits = ILI_Visits,
                  ILI_Percent = ILI_perc,
                  ILI_Baseline = ILI_baseline,
                  ILI_Threshold = ILI_threshold,
                  ILI_Status = Status,
                  ILI_Moving_Avg = moving_avg)

  ili_daily
}
