#' Trajectory Measure for Count Data
#'
#' Calculates the ratio of the most current to previous period
#' counts. Values greater than 1 indicate growing; less than one
#' indicate shrinking. \emph{NOTE: Counts must have the same period
#' (e.g. both summed over 7 consecutive days.)} In order to avoid
#' extreme values and dividing by zeros, the ratio uses an offset of 0.5
#' for both the numerator and denominator as pseudocounts and is
#' capped at 5 (i.e. a 400\% increase).
#'
#' The 0.5 offset was inspired by the \code{bayes.poisson.test} function from
#' the {BayesianFirstAid} packages uses a non-informative prior distribution
#' whose mean simplifies to approximately 0.5. For more information, see the
#' reproducibility paper on the OHI Surveillance Team's SharePoint site.
#'
#'
#' @param curr vector of counts for current period (must be non-negative integer)
#' @param prev vector of counts for previous period (must be non-negative integer)
#'
#' @return a numeric vector
#' @export
#'
#' @examples
#' traj <- score_trajectory(curr = 100L, prev = 80L)
score_trajectory <- function(curr, prev) {
  # requires: pair of non-negative integers, coerce if possible?
  curr <- check_nonneg(curr, "curr")
  prev <- check_nonneg(prev, "prev")

  # effects: returns an estimate of the ratio, moderated by pseudocounts and a cap (500%)
  # comment: A Bayesian estimate would be of higher quality but less transparent
  alpha <- 1

  traj <- ((curr + alpha)/(prev + alpha)) - 1
  100 * ifelse(traj <= 5, traj, 5)
}

#' Calculate p-value of trajectory current counts vs prior counts
#'
#' @inheritParams score_trajectory
#'
#' @return a p.value extracted from the results of \code{\link[stats]{poisson.test}}
#' @export
#'
#' @importFrom stats poisson.test
#' @importFrom purrr map2_dbl
#'
#' @examples
#' traj_pval <- pval_trajectory(curr = 100L, prev = 80L)
pval_trajectory <- function(curr, prev) {
  # requires: pair of non-negative integers, coerce if possible?
  curr <- check_nonneg(curr, "curr")
  prev <- check_nonneg(prev, "prev")

  pval <- purrr::map2_dbl(curr, prev, ~stats::poisson.test(c(.x, .y))$p.value)

  pval
}

#' Calculates the False Discovery Rate for trajectory p-values
#'
#' According to method of Benjamini & Hochberg (1995) via \code{\link[stats]{p.adjust}}
#'
#' @param pval a vector of p-values
#'
#' @return the false discovery rate
#' @export
#'
#' @importFrom stats p.adjust
#'
#' @examples
#' traj_pval <- pval_trajectory(curr = 100L, prev = 80L)
#' traj_fdr <- fdr_trajectory(traj_pval)
fdr_trajectory <- function(pval) {
  fdr <- stats::p.adjust(pval, method = "fdr")
  fdr
}

#' Classify Trajectory Measure for County Data
#'
#' This function tests to see if the count trajectory is statistically
#' distinct from zero with a Poisson test for the ratio of two counts
#' using \code{\link[stats]{poisson.test}} as.calculated by \code{\link{pval_trajectory}}
#' at the two-sided p < .05 level. If it is and the ratio as calculated by
#' \link{score_trajectory} is less than or equal to 0.9, the trajectory is
#' classified as shrinking. If it is and the ration is greather than or equal
#' to 1.1, the trajectory is classified as growing. If the test fails to
#' reject the null hypothesis, the trajectory is classified as not statistically
#' significant.
#'
#'
#' @param traj trajectory as calculated by \code{\link{score_trajectory}}
#' @param pval p-value as calculated by \code{\link{pval_trajectory}}
#'
#' @return an ordered factor "Shrinking" < "No statistically significant change" < "Growing"
#' @export
#'
#' @examples
#' traj <- score_trajectory(curr = 100L, prev = 80L)
#' traj_pval  <- pval_trajectory(curr = 100L, prev = 80L)
#'
#' traj_class <- class_trajectory(traj, traj_pval)
class_trajectory <- function(traj, pval) {
  out <- ifelse(traj <= 0.9 & pval < 0.025, 1,
         ifelse(traj >=  1.1 & pval < 0.025, 3, 2))

  out <- factor(out,
                levels = 1:3,
                labels = c("Shrinking", "No significant change", "Growing"),
                ordered = TRUE)

  return(out)
}

#' calculate the burden
#'
#' @inheritParams score_trajectory
#' @param pop vector of population values to standardize counts
#'
#' @return a numeric vector of population adjusted burden
#' @export
#'
#' @examples
#' burd <- score_burden(curr = 100L, prev = 80L, pop = 65432L)
score_burden <- function(curr, prev, pop) {
  # requires: pair of non-negative integers
  curr <- check_nonneg(curr, "curr")
  prev <- check_nonneg(prev, "prev")
  pop <- check_nonneg(pop, "pop")

  #Currently not using pseudocount/offset for burden
  alpha <- 0

  burden <- (1e5 / pop) * (curr + prev + alpha)

  burden
}

#' Classify population adjusted burden
#'
#' Currently, cutoff values are hard coded and adopted straight from
#' the ones used in the CDC State Indicators report:
#' \itemize{
#'   \item "Low" is a burden less than or equal to 10 per 100,000
#'   \item "Moderate" is a burden greater than 10 and less than or equal to 50 per 100,000
#'   \item "Moderately High" is a burden greater than 50 and less than or equal to 100 per 100,000
#'   \item "High" is a burden greater than 100
#' }
#'
#' @param burden numeric vector as calculated by \code{\link{score_burden}}
#'
#' @return an ordered factor
#' @export
#'
#' @examples
#' burd <- score_burden(curr = 100L, prev = 80L, pop = 65432L)
#' burd_class <- class_burden(burd)
class_burden <- function(burden) {
  out <- ifelse(burden <= 10, 1,
         ifelse(burden <= 50, 2,
         ifelse(burden <= 100, 3, 4)))

  out <- factor(out,
                levels = 1:4,
                labels = c("Low", "Moderate", "Moderately High", "High"),
                ordered = TRUE)

  out
}

#' Calculate the confirmed case composite summary
#'
#' Source: Jeff's Design Doc as confirmed by SEOC
#'
#' @param traj_class trajectory classification as calculated by \code{\link{class_trajectory}}
#' @param burd_class burden classification as calculated by \code{\link{class_burden}}
#'
#' @return an ordered factor for the composite confirmed case summary
#' @export
#'
#' @importFrom dplyr case_when
#'
#' @examples
#' burd <- score_burden(curr = 100L, prev = 80L, pop = 65432L)
#' burd_class <- class_burden(burd)
#' traj <- score_trajectory(curr = 100L, prev = 80L)
#' traj_pval  <- pval_trajectory(curr = 100L, prev = 80L)
#' traj_class <- class_trajectory(traj, traj_pval)
#'
#' composite <- confirmed_case_composite(traj_class, burd_class)
confirmed_case_composite <- function(traj_class, burd_class) {
  out <- dplyr::case_when(
    burd_class == "Low" & traj_class < "Growing" ~ 1,
    burd_class == "Low" & traj_class == "Growing" ~ 2,
    burd_class == "Moderate" & traj_class < "Growing" ~ 2,
    burd_class == "Moderate" & traj_class == "Growing" ~ 3,
    burd_class == "Moderately High" & traj_class == "Shrinking" ~ 2,
    burd_class == "Moderately High" & traj_class > "Shrinking" ~ 3,
    burd_class == "High" & traj_class %in% c("Shrinking", "No significant change", "Growing") ~ 3,
    TRUE ~ NA_real_
  )

  out <- factor(out,
                levels = 1:3,
                labels = c("Low", "Medium", "High"),
                ordered = TRUE)

  out
}

#' Calculates upper control limits for the reverse cusum control chart based on a
#' 3\eqn{\sigma} significance level.
#'
#' @inheritParams score_trajectory
#' @param delta_t number of time periods back in time.
#'
#' @return the smallest count greater than curr that would produce a statistically
#'         significant \code{\link[stats]{poisson.test}}
#' @export
#'
#' @importFrom stats poisson.test
#' @importFrom stats pnorm
#'
#' @examples
#' #For 3 weeks into the past using 7-day binned counts
#' rev_cusum_ucl(curr = 100L, delta_t = 3L)
rev_cusum_ucl <- function(curr, delta_t) {
  # requires: non-negative integer count, curr,  and positive integer weight, delta_t
  curr <- check_nonneg(curr, "curr")
  delta_t <- check_nonneg(delta_t, "delta_t")

  if (delta_t == 0L) {
    return(NA_real_)
  }

  # effects: returns the smallest count greater than curr that would provide significance at 3-sigma probability
  cum_lim <- delta_t * curr + 1
  p <- stats::poisson.test( c( curr , cum_lim ) , T = c( 1 , delta_t ) )$p.value
  while( p > stats::pnorm(-3) ) {
    cum_lim <- cum_lim + 1
    p <- stats::poisson.test( c( curr , cum_lim ) , T = c( 1 , delta_t ) )$p.value
  }

  out <- cum_lim/delta_t - curr

  out
}

#' Calculates lower control limits for the reverse cusum control chart based on a
#' 3\eqn{\sigma} significance level.
#'
#' @inheritParams rev_cusum_ucl
#'
#' @return the greatest count smaller than curr that would produce a statistically
#'         significant \code{\link[stats]{poisson.test}}
#' @export
#'
#' @importFrom stats poisson.test
#' @importFrom stats pnorm
#'
#' @examples
#' #For 3 weeks into the past using 7-day binned counts
#' rev_cusum_lcl(curr = 100L, delta_t = 3L)
rev_cusum_lcl <- function( curr , delta_t = 1 ) {
  # requires: non-negative integer count, curr,  and positive integer weight, delta_t
  curr <- check_nonneg(curr, "curr")
  delta_t <- check_nonneg(delta_t, "delta_t")

  if (delta_t == 0L) {
    return(NA_real_)
  }

  # effects: returns the largest count less than curr that would provide significance at 3-sigma probability
  cum_lim <- NA


  if (curr == 0) {
      cum_lim <- 0
    } else {
      cum_lim <- delta_t * curr - 1L
      p <- stats::poisson.test( c( curr , cum_lim ) , T = c( 1 , delta_t ) ) $p.value
      while( p > stats::pnorm(-3) & cum_lim > 0) {
        cum_lim <- cum_lim - 1
        p <- stats::poisson.test( c( curr , cum_lim ) , T = c( 1 , delta_t ) )$p.value
      }
    }

  out <- cum_lim/delta_t - curr

  out
}

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
#'   \item{pop_2018}{2018 Population Numbers pulled from WISH}
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
  case_df$post_date = case_df$post_date - lubridate::days(1)

  max_date <- max(case_df$post_date)

  cases_daily <- case_df %>%
    dplyr::filter(post_date >= (max_date - lubridate::days(13))) %>%
    dplyr::select(fips, geo_name, post_date, case_daily)

  cases_summary <- case_df %>%
    dplyr::group_by(.data$fips, .data$geo_name, .data$pop_2018) %>%
    dplyr::mutate(
      weeknum = rolling_week(date_vector = .data$post_date, end_date = max_date)
    ) %>%
    dplyr::group_by(.data$fips, .data$geo_name, .data$pop_2018, .data$weeknum) %>%
    dplyr::summarize(
      case_weekly = as.integer(sum(.data$case_daily)),
      week_end = max(.data$post_date),
      .groups = "drop_last"
    ) %>%
    dplyr::filter(.data$weeknum <= 2) %>%
    tidyr::pivot_wider(id_cols = c("fips", "geo_name", "pop_2018"),
                       values_from = c("case_weekly", "week_end"),
                       names_from = "weeknum")

  #Re-type pop to integer
  cases_summary$pop_2018 <- as.integer(cases_summary$pop_2018)

  list(summary = cases_summary,
       daily = cases_daily)
}


#' Process the shaped confirmed case data.frame into a Tableau ready format
#'
#' @param case_df Confirmed case data.frame (e.g. produced by \link{pull_histTable})
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
#'   shape_case_data() %>%
#'   process_confirmed_cases()
process_confirmed_cases <- function(case_df) {
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
                            pop = .data$pop_2018),
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
      Trajectory = signif(.data$Trajectory, 2),
      Trajectory = dplyr::if_else(.data$Trajectory_Class == "No significant change", "N/A",
                                  as.character(.data$Trajectory)),
      Burden = signif(.data$Burden, 2),
      RowType = "Summary"
    ) %>%
    dplyr::select(
      Date = .data$week_end_1,
      Region_ID = .data$fips,
      Region = .data$geo_name,
      RowType = .data$RowType,
      Conf_Case_Count = .data$Count,
      Conf_Case_Burden = .data$Burden,
      Conf_Case_Trajectory = .data$Trajectory,
      Conf_Case_Burden_Class = .data$Burden_Class,
      Conf_Case_Trajectory_Class = .data$Trajectory_Class,
      Conf_Case_Composite_Class = .data$Composite_Class,
      Conf_Case_Trajectory_P = .data$Trajectory_P,
      Conf_Case_Trajectory_FDR = .data$Trajectory_FDR
    )

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
#'   \item{pop_2018}{2018 Population Numbers pulled from WISH}
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
#'   \item{pop_2018}{WISH 2018 population figures}
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
    dplyr::group_by(Run_Date, RowType, fips, County, pop_2018) %>%
    dplyr::arrange(Report_Date) %>%
    dplyr::mutate(
      weeknum = rolling_week(date_vector = Report_Date, end_date = max_date)
    ) %>%
    dplyr::group_by(Run_Date, RowType, fips, County, pop_2018, weeknum) %>%
    dplyr::summarize(
      covid_reg_weekly = as.integer(sum(dailyCOVID_px)),
      covid_icu_weekly = as.integer(sum(dailyCOVID_ICUpx)),
      week_end = max(Report_Date)
    ) %>%
    dplyr::filter(weeknum <= 2) %>%
    tidyr::pivot_wider(id_cols = c("Run_Date", "RowType", "fips", "County", "pop_2018"),
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
                  PrctBeds_IBA,
                  PrctICU_IBA,
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
      COVID_px_Trajectory = signif(.data$COVID_px_Trajectory, 2),
      COVID_ICUpx_Trajectory = signif(.data$COVID_ICUpx_Trajectory, 2),
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
                  Hosp_PrctBeds_IBA = PrctBeds_IBA,
                  Hosp_PrctICU_IBA = PrctICU_IBA,
                  Hosp_PrctVent_Used = PrctVent_Used,
                  Hosp_COVID_px_Trajectory = COVID_px_Trajectory,
                  Hosp_COVID_px_Trajectory_Class = COVID_px_Trajectory_Class,
                  Hosp_COVID_ICUpx_Trajectory = COVID_ICUpx_Trajectory,
                  Hosp_COVID_ICUpx_Trajectory_Class = COVID_ICUpx_Trajectory_Class)
}

#' Shape Testing summary data for metric calculations
#'
#' @inheritParams process_testing
#'
#' @return a list of data.frames (one summary and one daily)
#'
#' @importFrom lubridate days
#'
#' @examples
#' \dontrun{
#'   #Add examples here
#' }
shape_testing_data <- function(testing_df) {
  max_date <- max(testing_df$resultdateonly)

  testing_daily <- dplyr::filter(testing_df, resultdateonly >= max_date - lubridate::days(13)) %>%
    dplyr::mutate(RowType = "Daily")

  testing_summary <- testing_daily %>%
    dplyr::mutate(
      RowType = "Summary"
    ) %>%
    dplyr::group_by(RowType, Region_ID, Area, Testing_Volume) %>%
    dplyr::summarize_if(is.numeric, sum, na.rm = TRUE) %>%
    dplyr::mutate(
      resultdateonly = max_date
    )

  testing_daily$Testing_Volume <- NULL

  out <- list(summary = testing_summary,
              daily = testing_daily)
}

#' Process the shaped hospital data into a Tableau ready format
#'
#' @param testing_df data.frame output by \code{\link{pull_testing}}
#'
#' @return a Tableau ready data.frame with the following columns:
#' \describe{
#'   \item{}{}
#'   \item{}{}
#'   \item{}{}
#'   \item{}{}
#'   \item{}{}
#'   \item{}{}
#'   \item{}{}
#'   \item{}{}
#'   \item{}{}
#'   \item{}{}
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
      total_specimens = NotPositive + Positive,
      percent_positive = 100 * (Positive / total_specimens),
    ) %>%
    dplyr::select(Date = resultdateonly,
                  Region = Area,
                  Region_ID,
                  RowType,
                  Testing_Positive_Specimens = Positive,
                  Testing_Nonpositive_Specimens = NotPositive,
                  Testing_Total_Specimens = total_specimens,
                  Testing_Percent_Positive = percent_positive,
                  Testing_Incident_Tests = Tests)

  test_summary <- clean_testing_df$summary %>%
    dplyr::mutate(
      total_specimens = NotPositive + Positive,
      percent_positive = 100 * (Positive / total_specimens),
      percent_positive_class = dplyr::case_when(
        percent_positive >= 10.0                          ~ ">= 10% positive",
        percent_positive >= 5.0 & percent_positive < 10.0 ~ ">= 5% & < 10% positive",
        percent_positive >= 0.0 & percent_positive < 5.0  ~ "< 5% positive",
        TRUE                                              ~ "ERROR"
      ),
      percent_volume = 100 * Tests/Testing_Volume,
      percent_volume_class = dplyr::case_when(
        percent_volume >= 100.0                         ~ ">= 100% testing goal",
        percent_volume >= 75.0 & percent_volume < 100.0 ~ ">= 75% & < 100% testing goal",
        percent_volume >= 0.0 & percent_volume < 75.0   ~ "< 75% testing goal",
        TRUE                                            ~ "ERROR"
      ),
      testing_composite = dplyr::case_when(
        percent_positive_class == "< 5% positive" & percent_volume_class == ">= 100% testing goal"                   ~ "Robust",
        percent_positive_class == "< 5% positive" & percent_volume_class ==  ">= 75% & < 100% testing goal"          ~ "Adequate",
        percent_positive_class == "< 5% positive" & percent_volume_class ==  "< 75% testing goal"                    ~ "Inadequate",
        percent_positive_class == ">= 5% & < 10% positive" & percent_volume_class ==  ">= 100% testing goal"         ~ "Adequate",
        percent_positive_class == ">= 5% & < 10% positive" & percent_volume_class ==  ">= 75% & < 100% testing goal" ~ "Adequate",
        percent_positive_class == ">= 5% & < 10% positive" & percent_volume_class ==  "< 75% testing goal"           ~ "Inadequate",
        percent_positive_class == ">= 10% positive" & percent_volume_class ==  ">= 100% testing goal"                ~ "Inadequate",
        percent_positive_class == ">= 10% positive" & percent_volume_class ==  ">= 75% & < 100% testing goal"        ~ "Inadequate",
        percent_positive_class == ">= 10% positive" & percent_volume_class ==  "< 75% testing goal"                  ~ "Inadequate",
        TRUE                                                                                                         ~ "ERROR"
      )
    ) %>%
    dplyr::select(Date = resultdateonly,
                  Region = Area,
                  Region_ID,
                  RowType,
                  Testing_Positive_Specimens = Positive,
                  Testing_Nonpositive_Specimens = NotPositive,
                  Testing_Total_Specimens = total_specimens,
                  Testing_Percent_Positive = percent_positive,
                  Testing_Incident_Tests = Tests,
                  Testing_Incident_Test_Target = Testing_Volume,
                  Testing_Percent_of_Target = percent_volume,
                  Testing_Composite_Class = testing_composite)

  dplyr::bind_rows(test_summary,
                   test_daily)

}

#' Shape CLI data
#'
#' @inheritParams process_cli
#'
#' @return output
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
    dplyr::group_by(fips, County, pop_2018) %>%
    dplyr::arrange(Visit_Date) %>%
    dplyr::mutate(
      weeknum = rolling_week(date_vector = Visit_Date, end_date = max_date)
    ) %>%
    dplyr::group_by(fips, County, pop_2018, weeknum) %>%
    dplyr::summarize(
      WeeklyED = as.integer(sum(DailyED)),
      week_end = max(Visit_Date)
    ) %>%
    dplyr::filter(weeknum <= 2) %>%
    tidyr::pivot_wider(id_cols = c("fips", "County", "pop_2018"),
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
#'
#' @return a Tableau ready data.frame with the following columns:
#' \describe{
#'   \item{}{}
#'   \item{}{}
#'   \item{}{}
#'   \item{}{}
#'   \item{}{}
#'   \item{}{}
#'   \item{}{}
#'   \item{}{}
#'   \item{}{}
#'   \item{}{}
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
process_cli <- function(cli_df){
  clean_cli_df <- shape_cli_data(cli_df)

  cli_daily <- clean_cli_df$daily %>%
    dplyr::select(Date = Visit_Date,
                  Region_ID = fips,
                  Region = County,
                  RowType,
                  CLI_Count = DailyED)

  cli_summary <- dplyr::ungroup(clean_cli_df$summary) %>%
    dplyr::mutate(dplyr::across(c("WeeklyED_1", "WeeklyED_2", "pop_2018"), as.integer)) %>%
    dplyr::mutate(
      Count = .data$WeeklyED_1 + .data$WeeklyED_2,
      Burden = score_burden(curr = .data$WeeklyED_1,
                            prev = .data$WeeklyED_2,
                            pop = .data$pop_2018),
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
      Trajectory = signif(.data$Trajectory, 2),
      Trajectory = dplyr::if_else(.data$Trajectory_Class == "No significant change", "N/A",
                                  as.character(.data$Trajectory)),
      Burden = signif(.data$Burden, 2)
    ) %>%
    dplyr::select(
      Date = .data$week_end_1,
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

  dplyr::bind_rows(cli_summary, cli_daily)
}

#' Shape ILI data
#'
#' @inheritParams process_ili
#'
#' @return output
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

  ili_daily <- dplyr::filter(ili_df, Visit_Date >= max_date - lubridate::days(13)) %>%
    dplyr::mutate(
      RowType = "Daily"
    )  %>%
    dplyr::select(Region = County,
                  Region_ID = fips,
                  Date = Visit_Date,
                  RowType,
                  Total_Visits,
                  ILI_Visits)

  ili_summary <- ili_df %>%
    dplyr::mutate(
      ILI_perc = if_else(Total_Visits > 0, 100 * (ILI_Visits / Total_Visits), 0),
      RowType = "Summary"
    ) %>%
    dplyr::select(Region = County,
                  Region_ID = fips,
                  Date = Visit_Date,
                  RowType,
                  Total_Visits,
                  ILI_Visits,
                  ILI_perc)

  list(daily = ili_daily,
       summary = ili_summary)
}

#' Process the shaped ILI data into a Tableau ready format
#'
#' @param ili_df data.frame produced by \code{\link{pull_essence}} for
#'               ILI metrics
#' @param ili_threshold_path path to .csv file containing ILI thresholds
#'
#' @return a Tableau ready data.frame with the following columns:
#' \describe{
#'   \item{}{}
#'   \item{}{}
#'   \item{}{}
#'   \item{}{}
#'   \item{}{}
#'   \item{}{}
#'   \item{}{}
#'   \item{}{}
#'   \item{}{}
#'   \item{}{}
#' }
#'
#' @export
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr if_else
#' @importFrom dplyr bind_rows
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

  ili_daily <- clean_ili_df$daily %>%
    dplyr::select(Region,
                  Region_ID,
                  Date,
                  RowType,
                  ILI_Total_Visits = Total_Visits,
                  ILI_Visits)

  ili_summary <- clean_ili_df$summary %>%
    dplyr::left_join(readr::read_csv(ili_threshold_path,
                                     col_types = readr::cols(
                                       Region = col_character(),
                                       ILI_baseline = col_double(),
                                       ILI_threshold = col_double()
                                     )), by = "Region") %>%
    dplyr::mutate(
      Over_baseline = dplyr::if_else(ILI_perc >= ILI_baseline, 1L, 0L),
      Over_threshold = dplyr::if_else(ILI_perc >= ILI_threshold, 1L, 0L),
      moving_avg = zoo::rollapply(ILI_perc, 3, mean, fill = NA, align = "right"),
      Status = dplyr::case_when(
        moving_avg >= ILI_threshold ~ "Elevated Activity",
        moving_avg >= ILI_baseline ~ "Moderate Activity",
        moving_avg < ILI_baseline ~ "Low Activity",
        TRUE ~ "NA"
      )
    ) %>%
    dplyr::select(Region,
                  Region_ID,
                  Date,
                  RowType,
                  ILI_Total_Visits = Total_Visits,
                  ILI_Visits,
                  ILI_Percent = ILI_perc,
                  ILI_Baseline = ILI_baseline,
                  ILI_Threshold = ILI_threshold,
                  ILI_Status = Status,
                  ILI_Moving_Avg = moving_avg)

  dplyr::bind_rows(ili_summary, ili_daily)
}


#' Shape Total Emergency Department Visits data
#'
#' @inheritParams process_total_ed
#'
#' @return output
#'
#' @importFrom lubridate days
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @examples
#' \dontrun{
#'   #write me an example
#' }
shape_total_ed_data <- function(total_ed_df) {
  max_date <- max(total_ed_df$Visit_Date, na.rm = TRUE)

  total_ed_daily <- dplyr::filter(total_ed_df, Visit_Date >= max_date - lubridate::days(13)) %>%
    dplyr::mutate(
      RowType = "Daily"
    )  %>%
    dplyr::select(Region = County,
                  Region_ID = fips,
                  Date = Visit_Date,
                  RowType,
                  Total_ED_Visits = ED_Visit)

  list(daily = total_ed_daily)
}

#' Process the shaped Total Emergency Department Visit data into a Tableau ready format
#'
#' @param total_ed_df data.frame produced by \code{\link{pull_essence}} for
#'               Total ED metrics
#'
#' @return a Tableau ready data.frame with the following columns:
#' \describe{
#'   \item{}{}
#'   \item{}{}
#'   \item{}{}
#'   \item{}{}
#'   \item{}{}
#'   \item{}{}
#'   \item{}{}
#'   \item{}{}
#'   \item{}{}
#'   \item{}{}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   #write me an example
#' }
process_total_ed <- function(total_ed_df) {
  clean_total_ed_df <- shape_total_ed_data(total_ed_df)

  total_ed_daily <- clean_total_ed_df$daily

  total_ed_daily
}
