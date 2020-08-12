#check_nonneg----
curr_int <- 123L
curr_dbl <- 123
curr_str <- " 123 "
curr_neg <- -123L
curr_na <- NA_integer_

##. . returns integer for proper use ----
expect_identical(ohiCovidMetrics:::check_nonneg(val = curr_int, arg.name = "curr"), 123L)

##. . returns integer for coercible values ----
### dbl
expect_identical(ohiCovidMetrics:::check_nonneg(val = curr_dbl, arg.name = "curr", verbose = FALSE), 123L)
expect_silent(ohiCovidMetrics:::check_nonneg(val = curr_dbl, arg.name = "curr", verbose = FALSE), 123L)
expect_message(ohiCovidMetrics:::check_nonneg(val = curr_dbl, arg.name = "curr"))
### str
expect_identical(ohiCovidMetrics:::check_nonneg(val = curr_str, arg.name = "curr", verbose = FALSE), 123L)
expect_silent(ohiCovidMetrics:::check_nonneg(val = curr_str, arg.name = "curr", verbose = FALSE), 123L)
expect_message(ohiCovidMetrics:::check_nonneg(val = curr_str, arg.name = "curr"))

#. . Fails correctly----
## NA value
expect_error(ohiCovidMetrics:::check_nonneg(curr_na, arg.name = "curr"), pattern = "missing")
## Negative
expect_error(ohiCovidMetrics:::check_nonneg(curr_neg, arg.name = "curr"), pattern = "negative")

#rolling_week----
#setup to test
suppressPackageStartupMessages(library(dplyr))

date_vector1 <- seq.Date(from = as.Date("2020-01-01"), to = as.Date("2020-06-09"), by = 1)
date_vector2 <- unique(as.Date("2020-01-01"), sample(date_vector1, 78, replace = FALSE))

vec_consec <- dplyr::tibble(my_date = date_vector1) %>%
  dplyr::mutate(
    week_consec = rolling_week(my_date, end_date = "2020-06-09")
  )

vec_miss <- dplyr::tibble(my_date = date_vector2) %>%
  dplyr::mutate(
    week_miss = rolling_week(my_date, end_date = "2020-06-09"),
    observed = 1
  )

combined <- dplyr::left_join(vec_consec, vec_miss, by = "my_date")

observed <- combined %>%
  dplyr::filter(observed == 1)

#Appears to work correctly.
# table(Observed = !is.na(combined$observed),
#       "Complete Vector = NA" = is.na(combined$week_consec),
#       "Missing Vector = NA" = is.na(combined$week_miss)) %>% ftable
#. . check that week calculations for missing dates = calculations for all dates for shared dates ----
expect_true(all(combined$week_consec[combined$observed == 1] == combined$week_miss[combined$observed == 1] |
                (is.na(combined$week_consec[combined$observed == 1]) & is.na(combined$week_miss[combined$observed == 1]))))
expect_identical(observed$week_consec, observed$week_miss)

#clean_reversals ----

#This seed makes a poorly behaved vector (i.e. has a negative value post cleaning)
#but the cleaned vector is a well behaved vector with a reversal
set.seed(321234)
testvec <- sample(-100:500, size = 90)

clean_testvec_bad <- clean_reversals(testvec, verbose = FALSE)
clean_testvec_good <- clean_reversals(clean_testvec_bad)

expect_equal(sum(testvec), sum(clean_testvec_bad))
expect_equal(sum(testvec), sum(clean_testvec_good))
expect_equal(length(testvec), length(clean_testvec_good))
expect_true(all(clean_testvec_good >= 0))
expect_message(clean_reversals(testvec))
expect_silent(clean_reversals(testvec, verbose = FALSE))
expect_error(clean_reversals(c(10, 2, 13, -4, 3, NA, 3, 18, -3, 3, 3, 3)))

#test that vector is of same length
#test that vector sums to same as original
#test that a known, well-behaved vector has no negative values
#test that a known, poorly behaved vector issues a warning
#check that it errors if there are any missing values

#std_region ----
badvec_reg <- c("Adams", "Dane", "Milwaukee", "Fond Du Lac", "Saint Croix", "Lacrosse",
                "HERC|Fox Valley Area", "HERC|Western",
                "HERC|North Central", "HERC|Northeast", "HERC|Northwest", "HERC|South Central",
                "HERC|Southeast", "Wisconsin", "NorthCentral", "SouthCentral", "Fox valley")
goodvec_reg <- c("Adams", "Dane", "Milwaukee", "Fond du Lac", "St. Croix", "La Crosse",
                 "Fox Valley Area", "Western", "North Central", "Northeast", "Northwest", "South Central",
                 "Southeast", "Wisconsin", "North Central", "South Central", "Fox Valley Area")

expect_identical(ohiCovidMetrics:::std_region(badvec_reg), goodvec_reg)

#std_region_ID ----
badvec_reg_id <- c("55", "55001", "55003", "55005", "55007", "55009", "55011", "55013", "55015", "55017",
                   "55019", "55021", "55023", "55025", "55027", "55029", "55031", "55033", "55035", "55037",
                   "55039", "55041", "55043", "55045", "55047", "55049", "55051", "55053", "55055", "55057",
                   "55059", "55061", "55063", "55065", "55067", "55069", "55071", "55073", "55075", "55077",
                   "55078", "55079", "55081", "55083", "55085", "55087", "55089", "55091", "55093", "55095",
                   "55097", "55099", "55101", "55103", "55105", "55107", "55109", "55111", "55113", "55115",
                   "55117", "55119", "55121", "55123", "55125", "55127", "55129", "55131", "55133", "55135",
                   "55137", "55139", "55141", "HERC|Fox Valley Area", "HERC|North Central", "HERC|Northeast",
                   "HERC|Northwest", "HERC|South Central", "HERC|Southeast", "HERC|Western")
goodvec_reg_id <- c("55", "55001", "55003", "55005", "55007", "55009", "55011", "55013", "55015", "55017",
                    "55019", "55021", "55023", "55025", "55027", "55029", "55031", "55033", "55035", "55037",
                    "55039", "55041", "55043", "55045", "55047", "55049", "55051", "55053", "55055", "55057",
                    "55059", "55061", "55063", "55065", "55067", "55069", "55071", "55073", "55075", "55077",
                    "55078", "55079", "55081", "55083", "55085", "55087", "55089", "55091", "55093", "55095",
                    "55097", "55099", "55101", "55103", "55105", "55107", "55109", "55111", "55113", "55115",
                    "55117", "55119", "55121", "55123", "55125", "55127", "55129", "55131", "55133", "55135",
                    "55137", "55139", "55141", "Fox Valley Area", "North Central", "Northeast", "Northwest",
                    "South Central", "Southeast", "Western")

expect_identical(ohiCovidMetrics:::std_region(badvec_reg_id), goodvec_reg_id)
