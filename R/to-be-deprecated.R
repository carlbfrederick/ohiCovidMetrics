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
