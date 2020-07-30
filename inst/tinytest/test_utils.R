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
