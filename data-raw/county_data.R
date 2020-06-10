## code to prepare `county_data` dataset goes here
county_data <- readr::read_csv("data-raw/wi_dph_regions.csv", col_types = "ccncc")

usethis::use_data(county_data)
