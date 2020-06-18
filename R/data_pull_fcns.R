#' Pull down DHS COVID-19 Historical Data Table
#'
#' This function uses the REST API to pull down the data that are currently
#' posted at https://data.dhsgis.wi.gov/datasets/covid-19-historical-data-table/
#' using the GeoJSON REST API. \emph{Note: currently it does not pull down any
#' geometry data used to produce maps.}
#'
#' @return a cleaned version of the COVID-19 Historical Data Table with the
#'   following columns
#'   \describe{
#'     \item{fips}{renamed from GEOID}
#'     \item{geo_type}{renamed from GEO}
#'     \item{geo_name}{renamed from NAME}
#'     \item{post_date}{LoadDttm converted to Date format}
#'     \item{case_daily}{cleaned daily new positive cases from POS_NEW except for the first day which is from POSITIVE}
#'     \item{test_daily}{cleaned daily new total tests from TEST_NEW except for the first day which is from POSITIve + NEGATIVE}
#'     \item{death_daily}{cleaned daily new deaths from DTH_NEW except for the first day which is from DEATHS}
#'     \item{case_cum}{daily cumulative positive cases calculated from case_daily}
#'     \item{test_cum}{daily cumulative total tests calculated from test_daily}
#'     \item{death_cum}{daily cumulative deaths calculated from death_daily}
#'   }
#'   and possibly one or more of the following columns if applicable
#'   \describe{
#'     \item{case_daily_raw}{original daily new positive cases before cleaning}
#'     \item{test_daily_raw}{original daily new total tests before cleaning}
#'     \item{death_daily_raw}{cleaned daily new deaths before cleaning}
#'   }
#'
#' @export
#'
#' @importFrom sf st_read
#' @importFrom sf st_set_geometry
#' @importFrom dplyr group_by
#' @importFrom dplyr transmute
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#' @importFrom dplyr %>%
#' @importFrom dplyr if_else
#'
#' @examples
#' hdt <- pull_histTable()
pull_histTable <- function() {
  #Pull down the data
  #REsT API URL
  api_url <- "https://services1.arcgis.com/ISZ89Z51ft1G16OK/ArcGIS/rest/services/COVID19_WI/FeatureServer/10/query?where=GEO%20%3D%20'STATE'%20OR%20GEO%20%3D%20'COUNTY'&outFields=GEOID,GEO,NAME,LoadDttm,NEGATIVE,POSITIVE,DEATHS,TEST_NEW,POS_NEW,DTH_NEW&outSR=4326&f=geojson"
  message("Downloading data from DHS ...")
  hdt <- sf::st_set_geometry(sf::st_read(api_url, quiet = TRUE), NULL)

  #Basic Selection/wrangling
  hdt <- hdt %>%
    dplyr::arrange(GEOID, LoadDttm) %>%
    dplyr::rename(fips = GEOID) %>%
    dplyr::group_by(fips) %>%
    dplyr::transmute(
      geo_type = GEO,
      geo_name = NAME,
      post_date = as.Date(as.POSIXct(LoadDttm/1000, origin = "1970-01-01 00:00.000 UTC")),
      case_daily = dplyr::if_else(is.na(POS_NEW), POSITIVE, POS_NEW),
      test_daily = dplyr::if_else(is.na(TEST_NEW), POSITIVE + ifelse(is.na(NEGATIVE),0L,NEGATIVE), TEST_NEW),
      death_daily = dplyr::if_else(is.na(DTH_NEW), DEATHS, DTH_NEW)
    )

  if (any(hdt$case_daily < 0)) {
    message("Cleaning reversals in daily confirmed cases")
    hdt$case_daily_raw <- hdt$case_daily
    hdt <- hdt %>%
      dplyr::mutate(
        case_daily = clean_reversals(case_daily)
      )

    num_negs <- sum(hdt$case_daily < 0)
    i <- 1
    while (num_negs > 0 & i < 101) {
      hdt$case_daily <- suppressWarnings(clean_reversals(hdt$case_daily))
      num_negs <- sum(hdt$case_daily < 0)
      i <- i + 1
    }
    message("  I had to call clean_reversals() on case_daily ", i, " times.")
  }

  if (any(hdt$test_daily < 0)) {
    message("Cleaning reversals in daily tests")
    hdt$test_daily_raw <- hdt$test_daily
    hdt <- hdt %>%
      dplyr::mutate(
        test_daily = clean_reversals(test_daily)
      )

    num_negs <- sum(hdt$test_daily < 0)
    i <- 1
    while (num_negs > 0 & i < 101) {
      hdt$test_daily <- suppressWarnings(clean_reversals(hdt$test_daily))
      num_negs <- sum(hdt$test_daily < 0)
      i <- i + 1
    }
    message("  I had to call clean_reversals() on test_daily ", i, " times.")
  }

  if (any(hdt$death_daily < 0)) {
    message("Cleaning reversals in daily deaths")
    hdt$death_daily_raw <- hdt$death_daily
    hdt <- hdt %>%
      dplyr::mutate(
        death_daily = clean_reversals(death_daily)
      )

    num_negs <- sum(hdt$death_daily < 0)
    i <- 1
    while (num_negs > 0 & i < 101) {
      hdt$death_daily <- suppressWarnings(clean_reversals(hdt$death_daily))
      num_negs <- sum(hdt$death_daily < 0)
      i <- i + 1
    }
    message("  I had to call clean_reversals() on death_daily ", i, " times.")
  }

  hdt <- hdt %>%
    dplyr::mutate(
      case_cum = cumsum(case_daily),
      test_cum = cumsum(test_daily),
      death_cum = cumsum(death_daily)
    )

}

#' Reshape confirmed case data for producing Tableau extracts
#'
#' take case data (from WEDSS or historical data table) and put it in proper
#' shape for metric tables to feed tableau
#'
#' @param case_df Confirmed case data.frame (e.g. produced by \link{pull_histTable})
#'
#' @return a data.frame with the following with one row per county, state, and
#' HERC regions with the following columns
#' \describe{
#'   \item{fips}{FIPS Code and/or region identifier}
#'   \item{geo_name}{Name of geography}
#'   \item{case_weekly_1}{Total cases for \strong{current} 7 day period}
#'   \item{case_weekly_2}{Total cases for \strong{prior} 7 day period}
#'   \item{week_end_1}{End date for \strong{current} 7 day period}
#'   \item{week_end_2}{End date for \strong{prior} 7 day period}
#'   \item{pop_2018}{2018 Population Numbers pulled from WISH}
#' }
#'
#' @export
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr summarize
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom dplyr summarize_at
#' @importFrom dplyr select
#' @importFrom tidyr pivot_wider
#'
#' @examples
#' \dontrun{
#'   hdt <- pull_histTable()
#'   hdt_clean <- shape_case_data(hdt)
#' }
shape_case_data <- function(case_df) {
   max_date <- max(case_df$post_date)

   out <- case_df %>%
     dplyr::group_by(fips, geo_name) %>%
     dplyr::mutate(
       weeknum = rolling_week(date_vector = post_date, end_date = max_date)
     ) %>%
     dplyr::group_by(fips, geo_name, weeknum) %>%
     dplyr::summarize(
       case_weekly = as.integer(sum(case_daily)),
       week_end = max(post_date),
       .groups = "drop_last"
     ) %>%
     dplyr::filter(weeknum <= 2) %>%
     tidyr::pivot_wider(id_cols = c(fips, geo_name),
                        values_from = c(case_weekly, week_end),
                        names_from = weeknum) %>%
     dplyr::left_join(dplyr::select(county_data, fips, herc_region, pop_2018), by = "fips")


   herc_regions <- out %>%
     dplyr::filter(fips != "55") %>%
     dplyr::group_by(week_end_1, week_end_2, herc_region) %>%
     dplyr::summarize_at(vars(case_weekly_1, case_weekly_2, pop_2018), sum) %>%
     dplyr::mutate(
       fips = paste("HERC", herc_region, sep = "|"),
       geo_name = herc_region
     )

   out <- dplyr::bind_rows(out, herc_regions) %>%
     dplyr::select(fips, geo_name, case_weekly_1, case_weekly_2, week_end_1, week_end_2, pop_2018)

   out$pop_2018[out$fips == "55"] <- sum(county_data$pop_2018)
   out$pop_2018 <- as.integer(out$pop_2018)

   out
 }
