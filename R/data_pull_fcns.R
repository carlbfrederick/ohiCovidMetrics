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
    hdt$post_date <- as.Date(hdt$post_date)
  } else {
    hdt$post_date <- as.Date(as.POSIXct(hdt$post_date/1000, origin = "1970-01-01 00:00.000 UTC"))
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
  run_date <- unique(as.Date(hosp$Run_Date))
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

#' Shape EM Resource summary data for metric calculations
#'
#' @param hosp_df data.frame output by \code{\link{pull_hospital}}
#'
#' @return a list of data.frames (one summary and one daily)
#' @export
#'
#' @importFrom lubridate days
#'
#' @examples
#' \dontrun{
#'   #Add examples here
#' }
shape_hospital_data <- function(hosp_df) {
  #Find max date for weekly calculations
  max_date <- max(hosp_df$Report_Date)

  hosp_daily <- filter(hosp_df,
                       RowType == "Daily",
                       Report_Date >= max_date - lubridate::days(13))

  hosp_summary <- hosp_df %>%
    filter(RowType == "Summary") %>%
    group_by(Run_Date, RowType, fips, County, pop_2018) %>%
    arrange(Report_Date) %>%
    mutate(
      weeknum = rolling_week(date_vector = Report_Date, end_date = max_date)
    ) %>%
    group_by(Run_Date, RowType, fips, County, pop_2018, weeknum) %>%
    summarize(
      covid_reg_weekly = as.integer(sum(dailyCOVID_px)),
      covid_icu_weekly = as.integer(sum(dailyCOVID_ICUpx)),
      week_end = max(Report_Date)
    ) %>%
    filter(weeknum <= 2) %>%
    pivot_wider(id_cols = c("Run_Date", "RowType", "fips", "County", "pop_2018"),
                values_from = c("covid_reg_weekly", "covid_icu_weekly", "week_end"),
                names_from = c("weeknum")) %>%
    rename(geo_name = "County")

  out <- list(summary = hosp_summary,
              daily = hosp_daily)

  return(out)
}

#' Pull data from WEDSS for Testing Metrics
#'
#' @param bcd_query SQL query string for data from BCD table
#' @param lab_query SQL query string for data from Lab table
#' @param test_Vol_path .xlsx file containing testing volume target this
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
    select(2:3)

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
#' @importFrom tidyr pivot_wider
#'
#' @examples
#' \dontrun{
#'   #write me an example
#' }
clean_testing <- function(bcd, lab, test_vol, end_date) {
  message("  Counting the number of incident tests...")
  total_tests <- calc_num_tests(bcd, lab)

  message("  Counting the number of positive and negative specimens...")
  specimens <- calc_pos_neg(lab)

  message("  Final wrangling on the testing data ...")

  test_raw <- dplyr::full_join(total_tests, specimens, by = c("Area", "resultdateonly")) %>%
    dplyr::arrange(Area, resultdateonly)

  #filter up to end date and discard dates before Jan 01, 2020 and missing dates
  if (!is.null(end_date)) {
    test_raw <- dplyr::filter(test_raw, .data$resultdateonly <= as.Date(end_date),
                                        .data$resultdateonly >= as.Date('2020-01-01'),
                                        !is.na(.data$resultdateonly))
  } else {
    test_raw <- dplyr::filter(test_raw, .data$resultdateonly >= as.Date('2020-01-01'),
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

  test_cty <- dplyr::left_join(test_raw, test_vol, by = "Area") %>%
    dplyr::left_join(dplyr::select(county_data, Area = county, Region_ID = fips, herc_region),
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
    arrange(Area, resultdateonly)

}

#' INTERNAL function to calculate total tests per county per day
#'
#' @inheritParams clean_testing
#'
#' @return data.frame
#'
#' @examples
#' \dontrun{
#'   #write me an example
#' }
calc_num_tests <- function(bcd, lab) {
  mergedf <- dplyr::inner_join(bcd, lab, by = "IncidentID")

  #NOT A CASE
  notacase <- mergedf[which(mergedf$ResolutionStatus=="Not A Case"),]

  ##Bucket 1 ----
  ##  incident ids that don't have a positive result
  bucket1.ids <- mergedf %>%
    filter(ResolutionStatus == "Not A Case") %>%
    group_by(IncidentID, result) %>%
    count() %>%
    pivot_wider(id_cols = c("IncidentID"),
                names_from = c("result"),
                values_from = c("n"), values_fill = 0) %>%
    filter(Positive == 0) %>%
    pull(IncidentID)

  bucket1 <- notacase %>%
    filter(IncidentID %in% bucket1.ids) %>%
    group_by(IncidentID) %>%
    arrange(ResultDate) %>%
    slice(1L) %>%
    rename(date = ResultDate)

  ##Bucket 5 ----
  ##  incident ids that have a positive results
  bucket5.ids <- mergedf %>%
    filter(ResolutionStatus == "Not A Case") %>%
    group_by(IncidentID, result) %>%
    count() %>%
    pivot_wider(id_cols = c("IncidentID"),
                names_from = c("result"),
                values_from = c("n"), values_fill = 0) %>%
    filter(Positive >= 1) %>%
    pull(IncidentID)

  bucket5 <- notacase %>%
    filter(IncidentID %in% bucket5.ids) %>%
    group_by(IncidentID) %>%
    arrange(ResultDate) %>%
    slice(1L) %>%
    rename(date = ResultDate)

  #CONFIRMED CASES
  confirmed <- mergedf[which(mergedf$ResolutionStatus=="Confirmed"),]

  ##Bucket 4 ----
  ##  Incident ids don't have a positive result at all
  bucket4.ids <- mergedf %>%
    filter(ResolutionStatus == "Confirmed") %>%
    group_by(IncidentID, result) %>%
    count() %>%
    pivot_wider(id_cols = c("IncidentID"),
                names_from = c("result"),
                values_from = c("n"), values_fill = 0) %>%
    filter(Positive == 0) %>%
    pull(IncidentID)

  bucket4 <- confirmed %>%
    filter(IncidentID %in% bucket4.ids) %>%
    mutate(
      date = DateSentCDC
    ) %>%
    group_by(IncidentID) %>%
    arrange(date) %>%
    slice(1L)

  ##Buckets 2 and 3
  confirmed.positive <- confirmed %>%
    filter(!IncidentID %in% bucket4.ids)

  confirmed.firstresult <- confirmed.positive %>%
    group_by(IncidentID) %>%
    arrange(ResultDate) %>%
    slice(1L)

  ##Bucket 2 ----
  ##  incident ids have positive on first result date
  bucket2 <- confirmed.firstresult %>%
    filter(result == "Positive") %>%
    rename(date = ResultDate)

  ##Bucket 3 ----
  ##  incident ids have positive on subsequent result date
  bucket3.ids <- confirmed.firstresult %>%
    filter(is.na(result) | result != "Positive") %>%
    pull(IncidentID)

  bucket3 <- confirmed %>%
    filter(IncidentID %in% bucket3.ids,
           result == "Positive") %>%
    group_by(IncidentID) %>%
    arrange(ResultDate) %>%
    slice(1L) %>%
    rename(date = ResultDate)

  #Assemble buckets ----
  bind_rows(bucket1, bucket2, bucket3, bucket4, bucket5) %>%
    mutate(
      DerivedCounty = if_else(trimws(DerivedCounty)== "Fond Du Lac",
                              "Fond du Lac", trimws(DerivedCounty)),
      resultdateonly = as.Date(date)
    ) %>%
    group_by(resultdateonly, DerivedCounty) %>%
    count(name = "Tests") %>%
    filter(!is.na(DerivedCounty), !DerivedCounty %in% c("Non-Wisconsin", "Unknown")) %>%
    rename(Area = DerivedCounty)
}

#' INTERNAL function to calculate ingredients for percent positive per county per day
#'
#' @inheritParams clean_testing
#'
#' @return data.frame
#'
#' @examples
#' \dontrun{
#'   #write me an example
#' }
calc_pos_neg <- function(lab) {
  lab.result <- lab %>%
    dplyr::filter(!is.na(result)) %>%
    dplyr::mutate(
      scdflag = dplyr::if_else(is.na(SpecCollectedDate),
                                "missing.scd", "present.scd")
    )

  lab.cast <- lab.result %>%
    group_by(IncidentID, scdflag) %>%
    count() %>%
    pivot_wider(id_cols = "IncidentID",
                names_from = "scdflag",
                values_from = "n",
                values_fill = 0)

  #Divide into three groups
  ##Basin 1 ----
  ##  incident ids that have all scds
  basin1.ids <- lab.cast %>%
    filter(missing.scd == 0) %>%
    pull(IncidentID)

  basin1 <- lab.result %>%
    filter(IncidentID %in% basin1.ids) %>%
    mutate(
      date = SpecCollectedDate
    )

  ##Basin 2 ----
  ##  incident ids that have at least one scd and is missing some other scds
  basin2.ids <- lab.cast %>%
    filter(missing.scd >= 1, present.scd >= 1) %>%
    pull(IncidentID)

  basin2 <- lab.result %>%
    filter(IncidentID %in% basin2.ids) %>%
    mutate(
      date = if_else(is.na(ResultDate), SpecReceivedDate, ResultDate)
    ) %>%
    filter(!is.na(date))

  ##Basin 3 ----
  ##  incident ids that don't have any scds
  basin3.ids <- lab.cast %>%
    filter(present.scd == 0) %>%
    pull(IncidentID)

  basin3 <- lab.result %>%
    filter(IncidentID %in% basin3.ids) %>%
    mutate(
      date = if_else(is.na(ResultDate), SpecReceivedDate, ResultDate)
    )%>%
    filter(!is.na(date))

  #Assemble basins ----
  lab2 <- bind_rows(basin1, basin2, basin3) %>%
    mutate(
      dateonly = as.Date(date),
      newid = paste(IncidentID, dateonly, sep = ""),
      DerivedCounty = trimws(DerivedCounty),
      resultdateonly = as.Date(ResultDate)
    ) %>%
    filter(!is.na(resultdateonly))

lab2 %>%
    count(newid, result) %>%
    pivot_wider(id_cols = "newid",
                names_from = "result",
                values_from = "n",
                values_fill = 0) %>%
    mutate(
      notpositive = Inconclusive + Indeterminate + Negative,
      result2 = case_when(
        notpositive >= 1 & Positive == 0 ~ "NotPositive",
        notpositive == 0 & Positive >= 1 ~ "Positive",
        notpositive >= 1 & Positive >= 1 ~ "Positive",
        TRUE ~ "other"
      )
    ) %>%
    full_join(lab2, by = "newid") %>%
    mutate(
      DerivedCounty = if_else(trimws(DerivedCounty)== "Fond Du Lac",
                              "Fond du Lac", trimws(DerivedCounty))
    ) %>%
    filter(!is.na(result2),
           !is.na(DerivedCounty),
           (!DerivedCounty %in% c("Non-Wisconsin", "Unknown"))) %>%
    select(newid, resultdateonly, result2, DerivedCounty) %>%
    distinct() %>%
    group_by(DerivedCounty, resultdateonly) %>%
    count(result2) %>%
    pivot_wider(id_cols = c("DerivedCounty", "resultdateonly"),
                names_from = "result2",
                values_from = "n",
                values_fill = 0) %>%
    rename(Area = DerivedCounty)
}

#' Shape Testing summary data for metric calculations
#'
#' @param testing_df data.frame output by \code{\link{pull_testing}}
#'
#' @return a list of data.frames (one summary and one daily)
#' @export
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
#' @importFrom dplyr map2
#' @importFrom dplyr map_dfr
#'
#' @examples
#' \dontrun{
#'   #write me an example please
#' }
pull_essence <- function(api_url, start_date, end_date = NULL, metric = c("cli", "ili", "total_ed")) {
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
#' @examples
#' \dontrun{
#'   #write me an example please
#' }
clean_cli <- function(cli){
  cli
}

#' Clean ESSENCE data for ILI metrics
#'
#' @param ili data.frame from \code{\link{pull_essence}}
#'
#' @return a cleaned data.frame
#'
#' @examples
#' \dontrun{
#'   #write me an example please
#' }
clean_ili <- function(ili) {
  ili
}

#' Clean ESSENCE data for Total ED metrics
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
  total_ed
}
