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

#' Process the shaped confirmed case data.frame into a Tableau ready format
#'
#' @param clean_case_df shaped case data produced by \code{\link{shape_case_data}}
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
#' @importFrom dplyr ungroup
#' @importFrom dplyr if_else
#' @importFrom rlang .data
#'
#' @examples
#' library(dplyr)
#'
#' output <- pull_histTable() %>%
#'   shape_case_data() %>%
#'   process_confirmed_cases()
process_confirmed_cases <- function(clean_case_df) {
  dplyr::ungroup(clean_case_df) %>%
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
}

#' Process the shaped hospital data into a Tableau ready format
#'
#' @param clean_hosp_df a list of shaped hospital daily and summary
#' data produced by \code{\link{shape_hospital_data}}
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
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' output <- pull_hospital("path-to-hospital-extract") %>%
#'   shape_hospital_data() %>%
#'   process_hospital()
#' }
process_hospital <- function(clean_hosp_df) {

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

#' Process the shaped hospital data into a Tableau ready format
#'
#' @param clean_testing_df a list shaped case data produced by \code{\link{shape_testing_data}}
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
process_testing <- function(clean_testing_df) {

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

#' Process the shaped CLI data into a Tableau ready format
#'
#' @param clean_cli_df ### produced by \code{\link{shape_cli_data}}
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
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#'   #write me an example
#' }
process_cli <- function(clean_cli_df){

  cli_daily <- clean_cli_df$daily %>%
    dplyr::select(Date = Visit_Date,
                  Region_ID = fips,
                  Region = County,
                  RowType,
                  CLI_Count = DailyED)

  cli_summary <- dplyr::ungroup(clean_cli_df$summary) %>%
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
