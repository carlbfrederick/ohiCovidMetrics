#check-appended-file.R
#
#Check Appended Files For Errors created by columns added
#

#Setup ----
library(tidyverse)
library(ohiCovidMetrics)
library(tinytest)

load(Sys.getenv("LOADAPPENDEDFILE"))

out$nocase <- as.Date(out$Data_Period, format = "%m/%d/%Y") - as.Date(out$Date) == 14
out$noessence <- as.Date(out$Data_Period, format = "%m/%d/%Y") - as.Date(out$Date) == 0 & out$RowType == "Daily"

non_cty <- c("Wisconsin", "Fox Valley Area", "North Central", "Northeast",
             "Northwest", "South Central", "Southeast", "Western")

##. . COLUMNS ADDED BY append_metric_files()----
macheck <- out %>%
  group_by(Region) %>%
  mutate(
    burn_obs = row_number(Date),
    ma_present = if_else(RowType == "Daily" & burn_obs > 6, TRUE, FALSE)
  )

###Hosp_DailyCOVID_PX_moving_avg
expect_true(inherits(macheck$Hosp_DailyCOVID_PX_moving_avg, 'numeric'),
            info = "Hosp_DailyCOVID_PX_moving_avg column is 'numeric' class")
expect_equal(sum(is.na(macheck$Hosp_DailyCOVID_PX_moving_avg[macheck$ma_present & macheck$Region %in% non_cty & !macheck$noessence])), 0,
             info = "Hosp_DailyCOVID_PX_moving_avg column has all expected nonmissing values")
expect_equal(sum(!is.na(macheck$Hosp_DailyCOVID_PX_moving_avg[!macheck$ma_present & !(out$Region %in% non_cty)])), 0,
             info = "Hosp_DailyCOVID_PX_moving_avg column is always NA/missing when expected")

###Hosp_DailyCOVID_ICU_moving_avg
expect_true(inherits(macheck$Hosp_DailyCOVID_ICU_moving_avg, 'numeric'),
            info = "Hosp_DailyCOVID_ICU_moving_avg column is 'numeric' class")
expect_equal(sum(is.na(macheck$Hosp_DailyCOVID_ICU_moving_avg[macheck$ma_present & out$Region %in% non_cty & !macheck$noessence])), 0,
             info = "Hosp_DailyCOVID_ICU_moving_avg column has all expected nonmissing values")
expect_equal(sum(!is.na(macheck$Hosp_DailyCOVID_ICU_moving_avg[!macheck$ma_present & !(out$Region %in% non_cty)])), 0,
             info = "Hosp_DailyCOVID_ICU_moving_avg column is always NA/missing when expected")

###Testing_Perc_Pos_moving_avg
expect_true(inherits(macheck$Testing_Perc_Pos_moving_avg, 'numeric'),
            info = "Testing_Perc_Pos_moving_avg column is 'numeric' class")
expect_equal(sum(is.na(macheck$Testing_Perc_Pos_moving_avg[macheck$ma_present & !macheck$noessence])), 0,
             info = "Testing_Perc_Pos_moving_avg column has all expected nonmissing values")
expect_equal(sum(!is.na(macheck$Testing_Perc_Pos_moving_avg[!macheck$ma_present])), 0,
             info = "Testing_Perc_Pos_moving_avg column is always NA/missing when expected")
expect_true(all(dplyr::between(macheck$Testing_Perc_Pos_moving_avg[macheck$ma_present & !macheck$noessence], left = 0.0, right = 100.0)),
            info = "Testing_Perc_Pos_moving_avg columns values are all between 0 and 100 inclusive.")
expect_true(any(macheck$Testing_Perc_Pos_moving_avg[macheck$ma_present] > 1.0),
            info = "Testing_Perc_Pos_moving_avg columns values are scaled between 0 and 100 (not 0 and 1).")

###Testing_Tot_Enc_moving_avg
expect_true(inherits(macheck$Testing_Tot_Enc_moving_avg, 'numeric'),
            info = "Testing_Tot_Enc_moving_avg column is 'numeric' class")
expect_equal(sum(is.na(macheck$Testing_Tot_Enc_moving_avg[macheck$ma_present & !macheck$noessence])), 0,
             info = "Testing_Tot_Enc_moving_avg column has all expected nonmissing values")
expect_equal(sum(!is.na(macheck$Testing_Tot_Enc_moving_avg[!macheck$ma_present])), 0,
             info = "Testing_Tot_Enc_moving_avg column is always NA/missing when expected")

###CLI_Count_moving_avg
expect_true(inherits(macheck$CLI_Count_moving_avg, 'numeric'),
            info = "CLI_Count_moving_avg column is 'numeric' class")
expect_equal(sum(is.na(macheck$CLI_Count_moving_avg[macheck$ma_present & !macheck$noessence])), 0,
             info = "CLI_Count_moving_avg column has all expected nonmissing values")
expect_equal(sum(!is.na(macheck$CLI_Count_moving_avg[!macheck$ma_present])), 0,
             info = "CLI_Count_moving_avg column is always NA/missing when expected")

###Conf_Case_Count_moving_avg
expect_true(inherits(macheck$Conf_Case_Count_moving_avg, 'numeric'),
            info = "Conf_Case_Count_moving_avg column is 'numeric' class")
expect_equal(sum(is.na(macheck$Conf_Case_Count_moving_avg[macheck$ma_present & !macheck$nocase & macheck$burn_obs > 7])), 0,
             info = "Conf_Case_Count_moving_avg column has all expected nonmissing values")
expect_equal(sum(!is.na(macheck$Conf_Case_Count_moving_avg[!macheck$ma_present])), 0,
             info = "Conf_Case_Count_moving_avg column is always NA/missing when expected")

