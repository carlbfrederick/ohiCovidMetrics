
pull_histTable_SG <- function(end_date = NULL,match_ohi = FALSE) {
  #Pull down the data
  #REsT API URL
  api_url <- "https://services1.arcgis.com/ISZ89Z51ft1G16OK/ArcGIS/rest/services/COVID19_WI/FeatureServer/10/query?where=GEO%20%3D%20'COUNTY'&outFields=GEOID,GEO,NAME,LoadDttm,NEGATIVE,POSITIVE,DEATHS,TEST_NEW,POS_NEW,DTH_NEW&outSR=4326&f=geojson"
  message("Downloading data from DHS ...")
  hdt <- sf::st_set_geometry(sf::st_read(api_url, quiet = TRUE,stringsAsFactors=FALSE), NULL)

  utils::data("county_data")
  
  ## Filtering by date must precede enforcing monotonicity
  if (!is.null(end_date)) {
    hdt <- dplyr::filter(hdt,as.Date(end_date) >= as.Date(as.POSIXct(.data$LoadDttm/1000, origin = "1970-01-01 00:00.000 UTC")))
  }  
  
  ## Replace NAs with 0 for NEGATIVE
  hdt <-  hdt %>% 
    dplyr::mutate(NEGATIVE = dplyr::if_else(is.na(.data$NEGATIVE), 0L, as.integer(.data$NEGATIVE))) 
  
  
  ## enforce monotonicity
  #### two versions: one to match the current pull_histTable() and one
  ###  that fixes issue #17
  if (match_ohi) {
    hdt <- hdt %>% 
      dplyr::mutate(TESTS=.data$POSITIVE + .data$NEGATIVE) %>% 
      dplyr::arrange(desc(.data$LoadDttm)) %>% 
      dplyr::group_by(.data$GEOID) %>% 
      dplyr::mutate(
        POSITIVE = cummin(.data$POSITIVE),
        TESTS = cummin(.data$TESTS),
        DEATHS   = cummin(.data$DEATHS)
      )
    
  } else {
    hdt <- hdt %>% 
      dplyr::arrange(desc(.data$LoadDttm)) %>% 
      dplyr::group_by(.data$GEOID) %>% 
      dplyr::mutate(
        POSITIVE = cummin(.data$POSITIVE),
        NEGATIVE = cummin(.data$NEGATIVE),
        DEATHS   = cummin(.data$DEATHS)
      ) %>% 
      dplyr::mutate(TESTS=.data$POSITIVE + .data$NEGATIVE) 
  } ##if/else (match_ohi)
  
  #Basic Selection/wrangling
  hdt <- hdt %>%
    dplyr::arrange(.data$GEOID, .data$LoadDttm) %>%
    dplyr::rename(fips = .data$GEOID) %>%
    dplyr::group_by(.data$fips) %>%
    dplyr::transmute(
      geo_type = .data$GEO,
      geo_name = .data$NAME,
      post_date = as.Date(as.POSIXct(.data$LoadDttm/1000, origin = "1970-01-01 00:00.000 UTC")),
      case_daily = .data$POSITIVE - lag(.data$POSITIVE, n=1L,default = 0),
      test_daily = .data$TESTS - lag(.data$TESTS,n=1L, default=0),
      death_daily = .data$DEATHS - lag(.data$DEATHS, n=1L,default=0) 
    ) %>%
    dplyr::left_join(dplyr::select(county_data, .data$fips, .data$herc_region, .data$pop_2018), by = "fips")

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
  
