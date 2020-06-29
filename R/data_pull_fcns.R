#' Pull down DHS COVID-19 Historical Data Table
#'
#' This function uses the REST API to pull down the data that are currently
#' posted at https://data.dhsgis.wi.gov/datasets/covid-19-historical-data-table/
#' using the GeoJSON REST API. \emph{Note: currently it does not pull down any
#' geometry data used to produce maps.}
#'
#' @param end_date (default = NULL) If specified it is the end date of the
#'                 time series that you wish to analyze. It should be something
#'                 coercible to Date format with as.Date. If you want all data
#'                 leave it blank.
#'
#' @return a cleaned version of the COVID-19 Historical Data Table including
#' HERC regions with the following columns
#'   \describe{
#'     \item{fips}{renamed from GEOID}
#'     \item{geo_type}{renamed from GEO}
#'     \item{geo_name}{renamed from NAME}
#'     \item{post_date}{LoadDttm converted to Date format}
#'     \item{case_daily}{cleaned daily new positive cases from POS_NEW except for the first day which is from POSITIVE}
#'     \item{test_daily}{cleaned daily new total tests from TEST_NEW except for the first day which is from POSITIve + NEGATIVE}
#'     \item{death_daily}{cleaned daily new deaths from DTH_NEW except for the first day which is from DEATHS}
#'     \item{pop_2018}{2018 Population Numbers pulled from WISH}
#'     \item{case_cum}{daily cumulative positive cases calculated from case_daily}
#'     \item{test_cum}{daily cumulative total tests calculated from test_daily}
#'     \item{death_cum}{daily cumulative deaths calculated from death_daily}
#'   }
#'   and likely one or more of the following columns if applicable
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
#' @importFrom dplyr left_join
#' @importFrom dplyr filter
#' @importFrom dplyr bind_rows
#' @importFrom rlang .data
#' @importFrom utils data
#'
#' @examples
#' #for all available data
#' hdt <- pull_histTable()
#'
#' #for data through a certain date
#' hdt_old <- pull_histTable(end_date = "2020-06-17")
#'
pull_histTable <- function(end_date = NULL) {
  #Pull down the data
  #REsT API URL
  api_url <- "https://services1.arcgis.com/ISZ89Z51ft1G16OK/ArcGIS/rest/services/COVID19_WI/FeatureServer/10/query?where=GEO%20%3D%20'COUNTY'&outFields=GEOID,GEO,NAME,LoadDttm,NEGATIVE,POSITIVE,DEATHS,TEST_NEW,POS_NEW,DTH_NEW&outSR=4326&f=geojson"
  message("Downloading data from DHS ...")
  hdt <- sf::st_set_geometry(sf::st_read(api_url, quiet = TRUE, stringsAsFactors = FALSE), NULL)

  #Protect
  hdt$NEGATIVE <- as.integer(hdt$NEGATIVE)
  hdt$TEST_NEW <- as.integer(hdt$TEST_NEW)

  clean_histTable(hdt, end_date)
}

#' Pull Confirmed Case data from WEDSS
#'
#' This function supplies a wrapper to fetch the results from a SQL query
#' from WEDSS and then performs basic data cleaning to calculate the
#' confirmed case metrics.
#'
#' @param query query to pull data from WEDSS. This call should only pull
#'              counties or jurisdictions that will automatically be
#'              aggregated into HERC regions and Statewide.
#' @param conn connection to database (see \code{\link[RODBC]{odbcConnect}})
#' @inheritParams pull_histTable
#'
#' @return a cleaned version of the COVID-19 Historical Data Table including
#' HERC regions with the following columns
#'   \describe{
#'     \item{fips}{renamed from GEOID}
#'     \item{geo_type}{renamed from GEO}
#'     \item{geo_name}{renamed from NAME}
#'     \item{post_date}{LoadDttm converted to Date format}
#'     \item{case_daily}{cleaned daily new positive cases from POS_NEW except for the first day which is from POSITIVE}
#'     \item{test_daily}{cleaned daily new total tests from TEST_NEW except for the first day which is from POSITIve + NEGATIVE}
#'     \item{death_daily}{cleaned daily new deaths from DTH_NEW except for the first day which is from DEATHS}
#'     \item{pop_2018}{2018 Population Numbers pulled from WISH}
#'     \item{case_cum}{daily cumulative positive cases calculated from case_daily}
#'     \item{test_cum}{daily cumulative total tests calculated from test_daily}
#'     \item{death_cum}{daily cumulative deaths calculated from death_daily}
#'   }
#'   and likely one or more of the following columns if applicable
#'   \describe{
#'     \item{case_daily_raw}{original daily new positive cases before cleaning}
#'     \item{test_daily_raw}{original daily new total tests before cleaning}
#'     \item{death_daily_raw}{cleaned daily new deaths before cleaning}
#'   }
#'
#' @export
#'
#' @importFrom RODBC sqlQuery
#'
#' @examples
#' \dontrun{
#'    library(RODBC)
#'    channel <- odbcConnect("databasename", ...)
#'
#'    sql_query <- "SELECT * FROM TABLE WHERE GEO = 'COUNTY'"
#'
#'    hdt <- pull_wedss(query = sql_query, conn = channel, end_date = as.Date("2020-06-24"))
#' }
#'
pull_wedss <- function(query, conn, end_date) {
  hdt <- RODBC::sqlQuery(conn, query)

  #Might need to do some basic data cleaning in here

  clean_histTable(hdt, end_date)
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
#'   \item{pop_2018}{2018 Population Numbers pulled from WISH}
#'   \item{case_weekly_1}{Total cases for \strong{current} 7 day period}
#'   \item{case_weekly_2}{Total cases for \strong{prior} 7 day period}
#'   \item{week_end_1}{End date for \strong{current} 7 day period}
#'   \item{week_end_2}{End date for \strong{prior} 7 day period}
#' }
#'
#' @export
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr summarize
#' @importFrom dplyr filter
#' @importFrom dplyr summarize_at
#' @importFrom dplyr select
#' @importFrom tidyr pivot_wider
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#'   hdt <- pull_histTable()
#'   hdt_clean <- shape_case_data(hdt)
#' }
shape_case_data <- function(case_df) {
   max_date <- max(case_df$post_date)

   out <- case_df %>%
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


   out$pop_2018[out$fips == "55"] <- sum(county_data$pop_2018)
   out$pop_2018 <- as.integer(out$pop_2018)

   out
 }

#' INTERNAL function to clean case data for metrics
#'
#' @param hdt a data.frame from the historical data table
#' @inheritParams pull_histTable
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr transmute
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#' @importFrom dplyr %>%
#' @importFrom dplyr if_else
#' @importFrom dplyr left_join
#' @importFrom dplyr filter
#' @importFrom dplyr bind_rows
#' @importFrom rlang .data
#' @importFrom utils data
#'
#' @return raw historical case data ready for cleaning
clean_histTable <- function(hdt, end_date) {
  utils::data("county_data")

  #Basic Selection/wrangling
  hdt <- hdt %>%
    dplyr::arrange(.data$GEOID, .data$LoadDttm) %>%
    dplyr::rename(fips = .data$GEOID) %>%
    dplyr::group_by(.data$fips) %>%
    dplyr::transmute(
      geo_type = .data$GEO,
      geo_name = .data$NAME,
      post_date = as.Date(as.POSIXct(.data$LoadDttm/1000, origin = "1970-01-01 00:00.000 UTC")),
      case_daily = dplyr::if_else(is.na(.data$POS_NEW), .data$POSITIVE, .data$POS_NEW),
      test_daily = dplyr::if_else(is.na(.data$TEST_NEW), .data$POSITIVE + dplyr::if_else(is.na(.data$NEGATIVE), 0L, as.integer(.data$NEGATIVE)), as.integer(.data$TEST_NEW)),
      death_daily = dplyr::if_else(is.na(.data$DTH_NEW), .data$DEATHS, .data$DTH_NEW)
    ) %>%
    dplyr::left_join(dplyr::select(county_data, .data$fips, .data$herc_region, .data$pop_2018), by = "fips")

  if (!is.null(end_date)) {
    hdt <- dplyr::filter(hdt, .data$post_date <= as.Date(end_date))
  }

  #Clean reversals at county level
  if (any(hdt$case_daily < 0)) {
    message("Cleaning reversals in daily confirmed cases")
    hdt$case_daily_raw <- hdt$case_daily
    hdt <- hdt %>%
      dplyr::mutate(
        case_daily = clean_reversals(.data$case_daily, verbose = FALSE)
      )

    num_negs <- sum(hdt$case_daily < 0)
    i <- 1
    while (num_negs > 0 & i < 101) {
      hdt$case_daily <- clean_reversals(hdt$case_daily, verbose = FALSE)
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
        test_daily = clean_reversals(.data$test_daily, verbose = FALSE)
      )

    num_negs <- sum(hdt$test_daily < 0)
    i <- 1
    while (num_negs > 0 & i < 101) {
      hdt$test_daily <- clean_reversals(hdt$test_daily, verbose = FALSE)
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
        death_daily = clean_reversals(.data$death_daily, verbose = FALSE)
      )

    num_negs <- sum(hdt$death_daily < 0)
    i <- 1
    while (num_negs > 0 & i < 101) {
      hdt$death_daily <- clean_reversals(hdt$death_daily, verbose = FALSE)
      num_negs <- sum(hdt$death_daily < 0)
      i <- i + 1
    }
    message("  I had to call clean_reversals() on death_daily ", i, " times.")
  }

  #Add in HERC and STATE rows
  herc <- hdt %>%
    dplyr::group_by(.data$post_date, .data$herc_region) %>%
    dplyr::summarize_at(dplyr::vars("case_daily", "test_daily", "death_daily", "pop_2018"), sum) %>%
    dplyr::mutate(
      fips = paste("HERC", .data$herc_region, sep = "|"),
      geo_name = .data$herc_region,
      geo_type = "HERC Region"
    )

  state <- hdt %>%
    dplyr::group_by(.data$post_date) %>%
    dplyr::summarize_at(dplyr::vars("case_daily", "test_daily", "death_daily", "pop_2018"), sum) %>%
    dplyr::mutate(
      fips = "55",
      geo_name = "Wisconsin",
      geo_type = "State"
    )

  hdt <- dplyr::bind_rows(hdt, herc, state) %>%
    dplyr::mutate(
      case_cum = cumsum(.data$case_daily),
      test_cum = cumsum(.data$test_daily),
      death_cum = cumsum(.data$death_daily)
    ) %>%
    select(-.data$herc_region)

}
