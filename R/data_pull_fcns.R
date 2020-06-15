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
  api_url <- "https://services1.arcgis.com/ISZ89Z51ft1G16OK/ArcGIS/rest/services/COVID19_WI/FeatureServer/10/query?where=GEO%20%3D%20'STATE'%20OR%20GEO%20%3D%20'COUNTY'&outFields=GEOID,GEO,NAME,LoadDttm,NEGATIVE,POSITIVE,POS_FEM,DEATHS,TEST_NEW,POS_NEW,DTH_NEW&outSR=4326&f=geojson"
  message("Downloading data from DHS ...")
  hdt <- sf::st_set_geometry(sf::st_read(api_url, quiet = TRUE), NULL)

  #Basic Selection/wrangling
  hdt %>%
    dplyr::arrange(GEOID, LoadDttm) %>%
    dplyr::group_by(GEOID) %>%
    dplyr::transmute(
      fips = GEOID,
      geo_type = GEO,
      geo_name = NAME,
      post_date = as.Date(LoadDttm),
      case_daily = dplyr::if_else(is.na(POS_NEW), POSITIVE, POS_NEW),
      test_daily = dplyr::if_else(is.na(TEST_NEW), POSITIVE + NEGATIVE, TEST_NEW),
      death_daily = dplyr::if_else(is.na(DTH_NEW), DEATHS, DTH_NEW)
    )

  if (any(hdt$case_daily < 0)) {
    message("Cleaning reversals in daily confirmed cases")
    hdt$case_daily_raw <- hdt$case_daily
    hdt$case_daily <- clean_reversals(hdt$case_daily)
  }

  if (any(hdt$test_daily < 0)) {
    message("Cleaning reversals in daily tests")
    hdt$test_daily_raw <- hdt$test_daily
    hdt$test_daily <- clean_reversals(hdt$test_daily)
  }

  if (any(hdt$death_daily < 0)) {
    message("Cleaning reversals in daily deaths")
    hdt$death_daily_raw <- hdt$death_daily
    hdt$death_daily <- clean_reversals(hdt$death_daily)
  }

  hdt <- hdt %>%
    dplyr::mutate(
      case_cum = cumsum(case_daily),
      test_cum = cumsum(test_daily),
      death_cum = cumsum(death_daily)
    )

}
