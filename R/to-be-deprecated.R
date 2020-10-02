#' DEPRECATED - VERSION Process the shaped confirmed case data.frame into a Tableau ready format
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
OLD_process_confirmed_cases <- function(clean_case_df) {
  warning("THIS FUNCTION IS DEPRECATED AND WILL BE REMOVED SHORTLY")

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

#' DEPRECATED - Clean ESSENCE data for Total ED metrics
#'
#' @param total_ed data.frame from \code{\link{pull_essence}}
#'
#' @return a cleaned data.frame
#'
#' @examples
#' \dontrun{
#'   #write me an example please
#' }
clean_total_ed <- function(total_ed) {
  warning("THIS FUNCTION IS DEPRECATED AND WILL BE REMOVED SHORTLY")

  total_ed_raw <- total_ed %>%
    dplyr::mutate(
      ED_Visit = dplyr::if_else(FacilityType == "Emergency Care", 1L, 0L),
      Visit_Date = as.Date(C_Visit_Date_Time, tz = "America/Chicago"),
      County = sub("WI_", "", Region)
    ) %>%
    dplyr::filter(ED_Visit == 1L)

  #retangularize the dates too so all counties are represented for all days
  total_ed_cty <- total_ed_raw %>%
    dplyr::group_by(County, Visit_Date) %>%
    dplyr::summarize(
      dplyr::across(c("ED_Visit"), sum),
      .groups = "drop"
    )  %>%
    dplyr::left_join(dplyr::select(county_data, County = county, fips, herc_region),
                     by = "County") %>%
    fill_dates(grouping_vars = c("County", "fips", "herc_region"),
               date_var = "Visit_Date")

  total_ed_herc <- total_ed_cty %>%
    dplyr::group_by(herc_region, Visit_Date) %>%
    dplyr::summarize(
      dplyr::across(c("ED_Visit"), sum),
      .groups = "drop"
    ) %>%
    dplyr::rename(fips = herc_region) %>%
    dplyr::mutate(
      County = fips
    )

  total_ed_state <- total_ed_cty %>%
    dplyr::group_by(Visit_Date) %>%
    dplyr::summarize(
      dplyr::across(c("ED_Visit"), sum),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      County = "Wisconsin",
      fips = "55"
    )

  dplyr::bind_rows(total_ed_herc, total_ed_state) %>%
    dplyr::arrange(County, Visit_Date) %>%
    dplyr::ungroup()
}

#' DEPRECATED: Shape Total Emergency Department Visits data
#'
#' @inheritParams process_total_ed
#'
#' @return a list of data.frames. Currently, only the daily version since
#' no summary metrics have been defined. The "daily" data.frame has one
#' row per county, state, and HERC region per day for the two week period
#' with the following columns
#' \describe{
#'   \item{Region}{Name of geography}
#'   \item{Region_ID}{FIPS Code and/or region identifier}
#'   \item{Date}{Date of emergency dept visit}
#'   \item{RowType}{Are row values summary or daily values}
#'   \item{Total_ED_Visits}{Total ED visits for the day}
#' }
#' \emph{Note}: The difference between Total_ED_Visits and ILI_Total_Visits
#' is that the former metrics includes all traffic into emergency departments
#' while the latter is restricted to Wisconsin residents.
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
  warning("THIS FUNCTION IS DEPRECATED AND WILL BE REMOVED SHORTLY")

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

#' DEPRECATED: Process the shaped Total Emergency Department Visit data into a Tableau ready format
#'
#' @param total_ed_df data.frame produced by \code{\link{pull_essence}} for
#'               Total ED metrics
#'
#' @return a Tableau ready data.frame with the following columns:
#' \describe{
#'   \item{Region}{Name of geography}
#'   \item{Region_ID}{FIPS Code and/or region identifier}
#'   \item{Date}{Date of emergency dept visit}
#'   \item{RowType}{Are row values summary or daily values}
#'   \item{Total_ED_Visits}{Total ED visits for the day}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   #write me an example
#' }
process_total_ed <- function(total_ed_df) {
  warning("THIS FUNCTION IS DEPRECATED AND WILL BE REMOVED SHORTLY")
  clean_total_ed_df <- shape_total_ed_data(total_ed_df)

  total_ed_daily <- clean_total_ed_df$daily

  total_ed_daily
}

#' DEPRECATED: Calculates upper control limits for the reverse cusum control chart based on a
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

#' DEPRECATED: Calculates lower control limits for the reverse cusum control chart based on a
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

#' DEPRECATED: INTERNAL function to calculate total tests per county per day
#'
#' @inheritParams clean_testing
#'
#' @return data.frame
#'
#' @importFrom dplyr inner_join
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr count
#' @importFrom dplyr pull
#' @importFrom dplyr arrange
#' @importFrom dplyr slice
#' @importFrom dplyr rename
#' @importFrom dplyr mutate
#' @importFrom dplyr bind_rows
#' @importFrom tidyr pivot_wider
#'
#' @examples
#' \dontrun{
#'   #write me an example
#' }
calc_num_tests <- function(bcd, lab, end_date) {
  max_date <- end_date
  min_date <- end_date - 13

  mergedf <- dplyr::inner_join(bcd, lab, by = "IncidentID")

  #NOT A CASE
  notacase <- mergedf[which(mergedf$ResolutionStatus=="Not A Case"),]

  ##Bucket 1 ----
  ##  incident ids that don't have a positive result
  bucket1.ids <- mergedf %>%
    dplyr::filter(ResolutionStatus == "Not A Case") %>%
    dplyr::group_by(IncidentID, result) %>%
    dplyr::count() %>%
    tidyr::pivot_wider(id_cols = c("IncidentID"),
                       names_from = c("result"),
                       values_from = c("n"), values_fill = 0) %>%
    dplyr::filter(Positive == 0) %>%
    dplyr::pull(IncidentID)

  bucket1 <- notacase %>%
    dplyr::filter(IncidentID %in% bucket1.ids) %>%
    dplyr::group_by(IncidentID) %>%
    dplyr::arrange(ResultDate) %>%
    dplyr::slice(1L) %>%
    dplyr::rename(date = ResultDate)

  ##Bucket 5 ----
  ##  incident ids that have a positive results
  bucket5.ids <- mergedf %>%
    dplyr::filter(ResolutionStatus == "Not A Case") %>%
    dplyr::group_by(IncidentID, result) %>%
    dplyr::count() %>%
    tidyr::pivot_wider(id_cols = c("IncidentID"),
                       names_from = c("result"),
                       values_from = c("n"), values_fill = 0) %>%
    dplyr::filter(Positive >= 1) %>%
    dplyr::pull(IncidentID)

  bucket5 <- notacase %>%
    dplyr::filter(IncidentID %in% bucket5.ids) %>%
    dplyr::group_by(IncidentID) %>%
    dplyr::arrange(ResultDate) %>%
    dplyr::slice(1L) %>%
    dplyr::rename(date = ResultDate)

  #CONFIRMED CASES
  confirmed <- mergedf[which(mergedf$ResolutionStatus=="Confirmed"),]

  ##Bucket 4 ----
  ##  Incident ids don't have a positive result at all
  bucket4.ids <- mergedf %>%
    dplyr::filter(ResolutionStatus == "Confirmed") %>%
    dplyr::group_by(IncidentID, result) %>%
    dplyr::count() %>%
    tidyr::pivot_wider(id_cols = c("IncidentID"),
                       names_from = c("result"),
                       values_from = c("n"), values_fill = 0) %>%
    dplyr::filter(Positive == 0) %>%
    dplyr::pull(IncidentID)

  bucket4 <- confirmed %>%
    dplyr::filter(IncidentID %in% bucket4.ids) %>%
    dplyr::mutate(
      date = DateSentCDC
    ) %>%
    dplyr::group_by(IncidentID) %>%
    dplyr::arrange(date) %>%
    dplyr::slice(1L)

  ##Buckets 2 and 3
  confirmed.positive <- confirmed %>%
    dplyr::filter(!IncidentID %in% bucket4.ids)

  confirmed.firstresult <- confirmed.positive %>%
    dplyr::group_by(IncidentID) %>%
    dplyr::arrange(ResultDate) %>%
    dplyr::slice(1L)

  ##Bucket 2 ----
  ##  incident ids have positive on first result date
  bucket2 <- confirmed.firstresult %>%
    dplyr::filter(result == "Positive") %>%
    dplyr::rename(date = ResultDate)

  ##Bucket 3 ----
  ##  incident ids have positive on subsequent result date
  bucket3.ids <- confirmed.firstresult %>%
    dplyr::filter(is.na(result) | result != "Positive") %>%
    dplyr::pull(IncidentID)

  bucket3 <- confirmed %>%
    dplyr::filter(IncidentID %in% bucket3.ids,
                  result == "Positive") %>%
    dplyr::group_by(IncidentID) %>%
    dplyr::arrange(ResultDate) %>%
    dplyr::slice(1L) %>%
    dplyr::rename(date = ResultDate)

  #Assemble buckets ----
  dplyr::bind_rows(bucket1, bucket2, bucket3, bucket4, bucket5) %>%
    dplyr::mutate(
      DerivedCounty = if_else(trimws(DerivedCounty)== "Fond Du Lac",
                              "Fond du Lac", trimws(DerivedCounty)),
      resultdateonly = as.Date(date, tz = "America/Chicago")
    ) %>%
    dplyr::filter(!is.na(DerivedCounty),
                  !DerivedCounty %in% c("Non-Wisconsin", "Unknown"),
                  resultdateonly >= min_date & resultdateonly <= max_date) %>%
    dplyr::group_by(resultdateonly, DerivedCounty) %>%
    dplyr::count(name = "Tests") %>%
    dplyr::rename(Area = DerivedCounty)
}
