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
#'     \item{post_date}{DATE converted to Date format}
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
  api_url <- "https://services1.arcgis.com/ISZ89Z51ft1G16OK/ArcGIS/rest/services/COVID19_WI/FeatureServer/10/query?where=GEO%20%3D%20'COUNTY'&outFields=GEOID,GEO,NAME,DATE,NEGATIVE,POSITIVE,DEATHS,TEST_NEW,POS_NEW,DTH_NEW&outSR=4326&f=geojson"
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
#'              or \code{\link[odbc]{OdbcConnection}}
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
#' @importFrom odbc dbGetQuery
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
pull_wedss <- function(query, conn, end_date = NULL) {
  if (inherits(conn, "RODBC")) {
    hdt <- RODBC::sqlQuery(conn, query)
  } else if (inherits(conn, "DBIConnection")) {
    hdt <- odbc::dbGetQuery(conn, query)
  }

  #Might need to do some basic data cleaning in here

  clean_histTable(hdt, end_date)
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
    dplyr::arrange(.data$GEOID, .data$DATE) %>%
    dplyr::rename(fips = .data$GEOID) %>%
    dplyr::group_by(.data$fips) %>%
    dplyr::transmute(
      geo_type = .data$GEO,
      geo_name = .data$NAME,
      post_date = .data$DATE,
      case_daily = dplyr::if_else(is.na(.data$POS_NEW), .data$POSITIVE, .data$POS_NEW),
      test_daily = dplyr::if_else(is.na(.data$TEST_NEW), .data$POSITIVE + dplyr::if_else(is.na(.data$NEGATIVE), 0L, as.integer(.data$NEGATIVE)), as.integer(.data$TEST_NEW)),
      death_daily = dplyr::if_else(is.na(.data$DTH_NEW), .data$DEATHS, .data$DTH_NEW)
    ) %>%
    dplyr::left_join(dplyr::select(county_data, .data$fips, .data$herc_region, .data$pop_2018), by = "fips")

  if (inherits(hdt$post_date, "POSIXt")) {
    hdt$post_date <- as.Date(hdt$post_date, tz = "America/Chicago")
  } else {
    hdt$post_date <- as.Date(as.POSIXct(hdt$post_date/1000, origin = "1970-01-01 00:00.000 UTC"), tz = "America/Chicago")
  }

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
      fips = .data$herc_region,
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

#' Imports Hospitalization file produced from EM Resource
#'
#' @param file path to EM Resource hospitalization summary .csv file
#' @inheritParams pull_histTable
#'
#'
#' @return a data.frame suitable for metric calculations
#' @export
#'
#' @importFrom readr read_csv
#' @importFrom readr cols
#' @importFrom readr col_date
#' @importFrom readr col_datetime
#' @importFrom readr col_character
#' @importFrom readr col_double
#'
#' @examples
#' \dontrun{
#'   pull_hospital("hospdtatafile.csv")
#' }
pull_hospital <- function(file, end_date = NULL) {
  #Enforce correct column types and names
  hosp_cols <- readr::cols(
    Report_Date = readr::col_date(format = "%m/%d/%Y"),
    BBB_Facility_Use_Status = readr::col_character(),
    BBB_Testing_Status = readr::col_character(),
    BBB_Crit_Supply_Status = readr::col_character(),
    BBB_Staff_Status = readr::col_character(),
    Total_ICU_Beds = readr::col_double(),
    Region = readr::col_double(),
    Most_Recent_Report_Date = readr::col_date(format = "%m/%d/%Y"),
    County = readr::col_character(),
    Run_Date = readr::col_datetime(format = "%m/%d/%Y %H:%M"),
    Hospital = readr::col_character(),
    `__Gen_Use_Bedside_Vent` = readr::col_double(),
    IBA__ICU = readr::col_double(),
    IBA__Intermediate_Care = readr::col_double(),
    IBA__Medical_Surgical = readr::col_double(),
    IBA__Neg_Flow_Isolation = readr::col_double(),
    Total___COVID_patients = readr::col_double(),
    `__ICU_COVID_patients` = readr::col_double(),
    Total_Intermediate_Care_Beds = readr::col_double(),
    Total_Medical_Surgical_Beds = readr::col_double(),
    Total_Neg_Flow_Isolation_Beds = readr::col_double(),
    Number_of_Ventilated_Patients = readr::col_double()
  )

  hosp_in <- readr::read_csv(file, col_types = hosp_cols)

  clean_hospital(hosp_in, end_date)
}

#' Clean Hospital Data for metric calculation
#'
#' @param hosp a data.frame from EMResource
#' @inheritParams pull_histTable
#'
#' @return a cleaned data.frame
#'
#' @examples
#' \dontrun{
#'   #example here
#' }
clean_hospital <- function(hosp, end_date) {
  #Grab Run date to append when we are finished
  run_date <- unique(as.Date(hosp$Run_Date, tz = "America/Chicago"))
  if (length(run_date) > 1) {
    stop("The input file has more than one Run_Date. Fix this and try again.")
  }

  #Basic Wrangling/Agg to County
  hosp <- hosp %>%
    mutate(
      Region = dplyr::case_when(
        Region == 1 ~ "Northwest",
        Region == 2 ~ "North Central",
        Region == 3 ~	"Northeast",
        Region == 4 ~ "Western" ,
        Region == 5 ~ "South Central",
        Region == 6 ~ "Fox Valley Area",
        Region == 7 ~ "Southeast"
      )
    )

  if (!is.null(end_date)) {
    hosp <- dplyr::filter(hosp, .data$Report_Date <= as.Date(end_date))
  }

  hosp_clean <- hosp  %>%
    dplyr::group_by(County, Report_Date) %>%
    dplyr::summarise(
      dailyCOVID_px = sum(Total___COVID_patients, na.rm=TRUE),
      dailyCOVID_ICUpx = sum(`__ICU_COVID_patients`, na.rm=TRUE)
    )

  hosp_cty <- fill_dates(hosp_clean, grouping_vars = "County", date_var = "Report_Date") %>%
    dplyr::mutate(
      dailyCOVID_px = dplyr::if_else(is.na(dailyCOVID_px), 0, dailyCOVID_px),
      dailyCOVID_ICUpx = dplyr::if_else(is.na(dailyCOVID_ICUpx), 0, dailyCOVID_ICUpx),
      geo_type = "County"
    ) %>%
    dplyr::left_join(select(county_data, fips, county, pop_2018, herc_region),
              by = c("County" = "county")) %>%
    dplyr::ungroup()

  hosp_herc <- hosp_cty %>%
    dplyr::group_by(herc_region, Report_Date) %>%
    dplyr::summarize_if(is.numeric, sum) %>%
    dplyr::mutate(
      County = herc_region,
      geo_type = "Region",
      fips = herc_region,
    )

  hosp_state <- hosp_cty %>%
    dplyr::group_by(Report_Date) %>%
    dplyr::summarize_if(is.numeric, sum) %>%
    dplyr::mutate(
      County = "Wisconsin",
      geo_type = "State",
      fips = "55",
    )

  hosp_summary <- bind_rows(hosp_cty, hosp_herc, hosp_state) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.data$herc_region)

  #HERC Daily Counts
  herc_daily <- hosp %>%
    dplyr::group_by(Report_Date, Region) %>%
    dplyr::summarize(
      totalbeds = sum(Total_Intermediate_Care_Beds
                      + Total_ICU_Beds
                      + Total_Neg_Flow_Isolation_Beds
                      + Total_Medical_Surgical_Beds, na.rm=TRUE),

      beds_IBA = sum(IBA__Intermediate_Care +
                       IBA__Medical_Surgical +
                       IBA__Neg_Flow_Isolation +
                       IBA__ICU, na.rm=TRUE),

      dailyCOVID_px = sum(Total___COVID_patients, na.rm=TRUE),

      totalICU = sum(Total_ICU_Beds, na.rm=TRUE),

      ICU_IBA = sum(IBA__ICU, na.rm=TRUE),

      dailyCOVID_ICUpx = sum(`__ICU_COVID_patients`, na.rm=TRUE),

      num_px_vent = sum(Number_of_Ventilated_Patients, na.rm=TRUE),

      total_vents = sum(`__Gen_Use_Bedside_Vent`, na.rm=TRUE),

      intermed_beds_IBA = sum(IBA__Intermediate_Care, na.rm=TRUE),

      negflow_beds_IBA = sum(IBA__Neg_Flow_Isolation, na.rm=TRUE),

      medsurg_beds_IBA = sum(IBA__Medical_Surgical, na.rm=TRUE),
    ) %>%
    ungroup() %>%
    rename(County = Region)

  #Agg HERC daily to State Daily
  state_daily <- herc_daily %>%
    dplyr::group_by(Report_Date) %>%
    dplyr::summarize_if(is.numeric, sum, na.rm=TRUE) %>%
    dplyr::mutate(
      County = "Wisconsin"
    )

  #Calc final vars for combined daily series
  hosp_daily <- dplyr::bind_rows(state_daily, herc_daily) %>%
    dplyr::mutate(
      PrctBeds_IBA = (beds_IBA/totalbeds)*100,
      PrctICU_IBA = (ICU_IBA/totalICU)*100,
      PrctVent_Used = (num_px_vent/total_vents)*100
    ) %>%
    dplyr::left_join(dplyr::select(county_data, fips, County = county), by = "County")

  rm(state_daily, herc_daily)

  #Bind summary and Daily
  out <- bind_rows(
    mutate(hosp_summary, RowType = "Summary"),
    mutate(hosp_daily, RowType = "Daily")
  ) %>%
  mutate(Run_Date = run_date) %>%
  group_by(County) %>%
  mutate(
    fips = unique(fips[!is.na(fips)])
  )
}

#' Pull data from WEDSS for Testing Metrics
#'
#' @param bcd_query SQL query string for data from BCD table
#' @param lab_query SQL query string for data from Lab table
#' @param test_vol_path .xlsx file containing testing volume target this
#'                      file must have a worksheet named weekly and it
#'                      pulls the second (Area) and third column (Current
#'                      Month Targets).
#'
#' @inheritParams pull_wedss
#'
#' @return a data.frame
#' @export
#'
#' @importFrom odbc dbGetQuery
#' @importFrom RODBC sqlQuery
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom readxl read_excel
#'
#' @examples
#' \dontrun{
#'   #write me an example please.
#' }
pull_testing <- function(bcd_query, lab_query, conn, test_vol_path, end_date = NULL) {
  if (inherits(conn, "RODBC")) {
    bcd <- RODBC::sqlQuery(conn, bcd_query)
    lab <- RODBC::sqlQuery(conn, lab_query)
  } else if (inherits(conn, "DBIConnection")) {
    bcd <- odbc::dbGetQuery(conn, bcd_query)
    lab <- odbc::dbGetQuery(conn, lab_query)
  }

  #read in test_volume
  test_vol <- readxl::read_excel(test_vol_path, sheet = "Weekly") %>%
    dplyr::select(2:3) %>%
    dplyr::mutate(
      Region = if_else(Region == "Saint Croix", "St. Croix", sub("HERC\\|", "", Region))
    )

  clean_testing(bcd, lab, test_vol, end_date)
}

#' Internal function to clean testing data
#'
#' @param bcd data.frame
#' @param lab data.frame
#' @param test_vol data.frame
#' @inheritParams pull_testing
#'
#' @return a data.frame
#'
#' @importFrom dplyr filter
#' @importFrom dplyr full_join
#' @importFrom dplyr arrange
#' @importFrom dplyr mutate
#' @importFrom dplyr across
#' @importFrom dplyr if_else
#' @importFrom tidyr pivot_wider
#'
#' @examples
#' \dontrun{
#'   #write me an example
#' }
clean_testing <- function(bcd, lab, test_vol, end_date) {
  message("  Counting the number of incident tests...")
  total_tests <- calc_num_tests(bcd, lab, as.Date(end_date))

  message("  Counting the number of positive and negative specimens...")
  specimens <- calc_pos_neg(lab, as.Date(end_date))

  message("  Final wrangling on the testing data ...")

  test_raw <- dplyr::full_join(total_tests, specimens, by = c("Area", "resultdateonly")) %>%
    dplyr::arrange(Area, resultdateonly) %>%
    dplyr::mutate(dplyr::across(c("Tests", "NotPositive", "Positive"),
                                ~ dplyr::if_else(is.na(.x), 0L, .x)))

  #filter up to end date and discard dates before Jan 01, 2020 and missing dates
  if (!is.null(end_date)) {
    test_raw <- dplyr::filter(test_raw, .data$resultdateonly <= as.Date(end_date),
                                        .data$resultdateonly >= as.Date(end_date) - 13,
                                        !is.na(.data$resultdateonly))
  } else {
    test_raw <- dplyr::filter(test_raw, .data$resultdateonly >= Sys.Date() - 13,
                                        !is.na(.data$resultdateonly))
  }

  #run thru fill_dates ... might not be necessary since we don't need daily or weekly counts
  test_raw <- fill_dates(test_raw, "Area", "resultdateonly")

  #add volume targets
  names(test_vol) <- c("Area", "Testing_Volume")

  test_vol <- test_vol %>%
    dplyr::mutate(
      Testing_Volume = 2 * Testing_Volume
    )

  test_cty <- dplyr::left_join(test_raw,
                               dplyr::select(county_data, Area = county,
                                             Region_ID = fips, herc_region),
                               by = "Area")

  #add on HERC rows and WI rows by date
  test_herc <- test_cty %>%
    dplyr::group_by(herc_region, resultdateonly) %>%
    dplyr::summarize_if(is.numeric, sum) %>%
    dplyr::mutate(
      Area = herc_region,
      Region_ID = herc_region
    )

  test_state <- test_cty %>%
    dplyr::group_by(resultdateonly) %>%
    dplyr::summarize_if(is.numeric, sum) %>%
    dplyr::mutate(
      Area = "Wisconsin",
      Region_ID = "55"
    )

  bind_rows(test_cty, test_herc, test_state) %>%
    dplyr::ungroup() %>%
    dplyr::select(-herc_region) %>%
    arrange(Area, resultdateonly) %>%
    dplyr::left_join(test_vol, by = "Area")
}

#' INTERNAL function to calculate total tests per county per day
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

#' INTERNAL function to calculate ingredients for percent positive per county per day
#'
#' @inheritParams clean_testing
#'
#' @return data.frame
#'
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr count
#' @importFrom dplyr pull
#' @importFrom dplyr if_else
#' @importFrom dplyr bind_rows
#' @importFrom dplyr full_join
#' @importFrom dplyr case_when
#' @importFrom dplyr select
#' @importFrom dplyr distinct
#' @importFrom dplyr rename
#' @importFrom tidyr pivot_wider
#'
#' @examples
#' \dontrun{
#'   #write me an example
#' }
calc_pos_neg <- function(lab, end_date) {
  max_date <- end_date
  min_date <- end_date - 13

  lab.result <- lab %>%
    dplyr::filter(!is.na(result)) %>%
    dplyr::mutate(
      scdflag = dplyr::if_else(is.na(SpecCollectedDate),
                                "missing.scd", "present.scd")
    )

  lab.cast <- lab.result %>%
    dplyr::group_by(IncidentID, scdflag) %>%
    dplyr::count() %>%
    tidyr::pivot_wider(id_cols = "IncidentID",
                names_from = "scdflag",
                values_from = "n",
                values_fill = 0)

  #Divide into three groups
  ##Basin 1 ----
  ##  incident ids that have all scds
  basin1.ids <- lab.cast %>%
    dplyr::filter(missing.scd == 0) %>%
    dplyr::pull(IncidentID)

  basin1 <- lab.result %>%
    dplyr::filter(IncidentID %in% basin1.ids) %>%
    dplyr::mutate(
      date = SpecCollectedDate
    )

  ##Basin 2 ----
  ##  incident ids that have at least one scd and is missing some other scds
  basin2.ids <- lab.cast %>%
    dplyr::filter(missing.scd >= 1, present.scd >= 1) %>%
    dplyr::pull(IncidentID)

  basin2 <- lab.result %>%
    dplyr::filter(IncidentID %in% basin2.ids) %>%
    dplyr::mutate(
      date = dplyr::if_else(is.na(ResultDate), SpecReceivedDate, ResultDate)
    ) %>%
    dplyr::filter(!is.na(date))

  ##Basin 3 ----
  ##  incident ids that don't have any scds
  basin3.ids <- lab.cast %>%
    dplyr::filter(present.scd == 0) %>%
    dplyr::pull(IncidentID)

  basin3 <- lab.result %>%
    dplyr::filter(IncidentID %in% basin3.ids) %>%
    dplyr::mutate(
      date = if_else(is.na(ResultDate), SpecReceivedDate, ResultDate)
    )%>%
    dplyr::filter(!is.na(date))

  #Assemble basins ----
  lab2 <- dplyr::bind_rows(basin1, basin2, basin3) %>%
    dplyr::mutate(
      dateonly = as.Date(date, tz = "America/Chicago"),
      newid = paste(IncidentID, dateonly, sep = ""),
      DerivedCounty = ifelse(trimws(DerivedCounty) == "Fond Du Lac",
                             "Fond du Lac", trimws(DerivedCounty)),
      resultdateonly = as.Date(ResultDate, tz = "America/Chicago")
    ) %>%
    dplyr::filter(!is.na(resultdateonly))

  #decided to assign first resultdateonly within the window to deduplicate rows
  #but keep the totals consistent with the original code.
  lab2 %>%
    dplyr::filter(resultdateonly >= min_date & resultdateonly <= max_date) %>%
    dplyr::group_by(newid) %>%
    dplyr::arrange(result, resultdateonly) %>%
    dplyr::mutate(
      first_positive = dplyr::first(resultdateonly[result == "Positive"]),
      first_negative = dplyr::first(resultdateonly[result %in% c("Inconclusive", "Indeterminate", "Negative")])
    ) %>%
    dplyr::count(newid, result, first_positive, first_negative) %>%
    tidyr::pivot_wider(id_cols = c("newid", "first_positive", "first_negative"),
                       names_from = "result",
                       values_from = "n",
                       values_fill = 0) %>%
    dplyr::mutate(
      notpositive = Inconclusive + Indeterminate + Negative,
    ) %>%
    dplyr::group_by(newid, first_positive, first_negative) %>%
    dplyr::mutate(
      anypos = sum(Positive),
      anyneg = sum(notpositive),
      result2 = dplyr::case_when(
        anyneg >= 1 & anypos == 0 ~ "NotPositive",
        anyneg == 0 & anypos >= 1 ~ "Positive",
        anyneg >= 1 & anypos >= 1 ~ "Positive",
        TRUE ~ "other"
      )
    ) %>%
    dplyr::full_join(lab2, by = c("newid")) %>%
    dplyr::filter(!is.na(result2)) %>%
    dplyr::mutate(
      DerivedCounty = if_else(trimws(DerivedCounty)== "Fond Du Lac",
                              "Fond du Lac", trimws(DerivedCounty))
    ) %>%
    dplyr::filter(!is.na(result2),
                  !is.na(DerivedCounty),
                  (!DerivedCounty %in% c("Non-Wisconsin", "Unknown")),
                  (resultdateonly >= min_date & resultdateonly <= max_date)) %>%
    dplyr::select(newid, result2, DerivedCounty, first_positive, first_negative) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      resultdateonly = dplyr::if_else(result2 == "NotPositive", first_negative, first_positive)
    ) %>%
    dplyr::group_by(DerivedCounty, resultdateonly) %>%
    dplyr::count(result2) %>%
    tidyr::pivot_wider(id_cols = c("DerivedCounty", "resultdateonly"),
                       names_from = "result2",
                       values_from = "n",
                       values_fill = 0) %>%
    dplyr::rename(Area = DerivedCounty)
}

#' Pulls Essence Data for 3 metrics: CLI, ILI, Total ED Visits
#'
#' @param api_url character string matching Essence API format
#' @param start_date Start date of the time series that you wish to analyze.
#'                   It should be something coercible to Date format with as.Date.
#' @inheritParams pull_histTable
#' @param metric one of "cli", "ili", "total_ed" depending on which metrics you
#'               wish to calculate.
#'
#' @return a data.frame
#' @export
#'
#' @importFrom dplyr tibble
#' @importFrom dplyr mutate
#' @importFrom dplyr rowwise
#' @importFrom purrr map2
#' @importFrom purrr map_dfr
#'
#' @examples
#' \dontrun{
#'   #write me an example please
#' }
pull_essence <- function(api_url, start_date, end_date = NULL, metric = c("cli", "ili", "total_ed")) {
  if (metric == "total_ed") {
    message("The TOTAL ED Metric has been removed from the dashboard, this function has been deprecated and will be phased out soon.")
  }

  start_date <- as.Date(start_date)
  if (is.null(end_date)) {
    end_date <- Sys.Date()
  } else {
    end_date <- as.Date(end_date)
  }

  chunk_dates <- dplyr::tibble(
    start = seq(start_date, end_date, by = 14)
  ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      end = min(start + 13, end_date)
    )

  out <- purrr::map2(chunk_dates$start, chunk_dates$end, essence_query, url = api_url) %>%
    purrr::map_dfr(essence_data)

  switch(metric,
         "cli" = clean_cli(out),
         "ili" = clean_ili(out),
         "total_ed" = clean_total_ed(out))
}

#' Clean ESSENCE data for CLI metrics
#'
#' @param cli data.frame from \code{\link{pull_essence}}
#'
#' @return a cleaned data.frame
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr if_else
#' @importFrom dplyr full_join
#' @importFrom dplyr select
#' @importFrom dplyr across
#' @importFrom dplyr group_by
#' @importFrom dplyr first
#' @importFrom dplyr rename
#' @importFrom dplyr summarize
#' @importFrom dplyr bind_rows
#' @importFrom dplyr ungroup
#' @importFrom dplyr arrange
#' @importFrom tidyr complete
#'
#' @examples
#' \dontrun{
#'   #write me an example please
#' }
clean_cli <- function(cli){
  cli_raw <- cli %>%
    dplyr::mutate(
      ED_Visit = dplyr::if_else(FacilityType == "Emergency Care", 1L, 0L),
      Non_Resident = dplyr::if_else(grepl("WI_", Region), 0L, 1L),
      Visit_Date = as.Date(C_Visit_Date_Time, tz = "America/Chicago"),
      Hx_County = sub("WI_", "", HospitalRegion),
      County = sub("WI_", "", Region)
    ) %>%
    dplyr::filter(ED_Visit == 1L, Non_Resident == 0L)

  #retangularize the dates too so all counties are represented for all days
  cli_cty <- cli_raw %>%
    dplyr::group_by(County, Visit_Date) %>%
    dplyr::summarize(
      DailyED = sum(ED_Visit),
      .groups = "drop"
    ) %>%
    dplyr::full_join(dplyr::select(county_data, County = county, fips, herc_region, pop_2018),
                     by = "County") %>%
    tidyr::complete(County, Visit_Date,
                    fill = list("DailyED" = 0L)) %>%
    dplyr::group_by(County) %>%
    dplyr::mutate(dplyr::across(fips:pop_2018, ~dplyr::first(.x[!is.na(.x)]))) %>%
    dplyr::filter(!is.na(Visit_Date))

  #Aggregate to HERC and State and Append
  cli_herc <- cli_cty %>%
    dplyr::group_by(herc_region, Visit_Date) %>%
    dplyr::summarize(
      DailyED = sum(DailyED),
      pop_2018 = sum(pop_2018),
      .groups = "drop"
    ) %>%
    dplyr::rename(fips = herc_region) %>%
    dplyr::mutate(
      County = fips
    )

  cli_state <- cli_cty %>%
    dplyr::group_by(Visit_Date) %>%
    dplyr::summarize(
      DailyED = sum(DailyED),
      pop_2018 = sum(pop_2018),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      County = "Wisconsin",
      fips = "55"
    )

  dplyr::bind_rows(cli_cty, cli_herc, cli_state) %>%
    dplyr::select(-herc_region) %>%
    dplyr::arrange(County, Visit_Date) %>%
    dplyr::ungroup()
}

#' Clean ESSENCE data for ILI metrics
#'
#' @param ili data.frame from \code{\link{pull_essence}}
#'
#' @return a cleaned data.frame
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr if_else
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr across
#' @importFrom dplyr left_join
#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom dplyr bind_rows
#' @importFrom dplyr arrange
#' @importFrom dplyr ungroup
#' @importFrom tidyr complete
#'
#' @examples
#' \dontrun{
#'   #write me an example please
#' }
clean_ili <- function(ili) {
  ili_raw <- ili %>%
    dplyr::mutate(
      ED_Visit = dplyr::if_else(FacilityType == "Emergency Care", 1L, 0L),
      Non_Resident = dplyr::if_else(grepl("WI_", Region), 0L, 1L),
      Visit_Date = as.Date(C_Visit_Date_Time, tz = "America/Chicago"),
      County = sub("WI_", "", Region),
      Total_Visits = ifelse(is.na(C_BioSense_ID),0,1),
      ILI_Visits = ifelse(grepl("ILI CCDD v1", CCDDCategory_flat),1,0),
      ILI_dx = ifelse(grepl("CDC Influenza DD v1", CCDDCategory_flat),1,0)
    ) %>%
    dplyr::filter(ED_Visit == 1L, Non_Resident == 0L)

  #retangularize the dates too so all counties are represented for all days
  ili_cty <- ili_raw %>%
    dplyr::group_by(County, Visit_Date) %>%
    dplyr::summarize(
      dplyr::across(c("Total_Visits", "ILI_Visits", "ILI_dx"), sum),
      .groups = "drop"
    )  %>%
    dplyr::full_join(dplyr::select(county_data, County = county, fips, herc_region),
                     by = "County") %>%
    tidyr::complete(County, Visit_Date,
                    fill = list("Total_Visits" = 0L, "ILI_Visits" = 0L, "ILI_dx" = 0L)) %>%
    dplyr::group_by(County) %>%
    dplyr::mutate(dplyr::across(fips:herc_region, ~dplyr::first(.x[!is.na(.x)]))) %>%
    dplyr::filter(!is.na(Visit_Date))

  #Aggregate to HERC and State and Append
  ili_herc <- ili_cty %>%
    dplyr::group_by(herc_region, Visit_Date) %>%
    dplyr::summarize(
      dplyr::across(c("Total_Visits", "ILI_Visits", "ILI_dx"), sum),
      .groups = "drop"
    ) %>%
    dplyr::rename(fips = herc_region) %>%
    dplyr::mutate(
      County = fips
    )

  ili_state <- ili_cty %>%
    dplyr::group_by(Visit_Date) %>%
    dplyr::summarize(
      dplyr::across(c("Total_Visits", "ILI_Visits", "ILI_dx"), sum),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      County = "Wisconsin",
      fips = "55"
    )

  dplyr::bind_rows(ili_cty, ili_herc, ili_state) %>%
    dplyr::select(-herc_region) %>%
    dplyr::arrange(County, Visit_Date) %>%
    dplyr::ungroup()
}

