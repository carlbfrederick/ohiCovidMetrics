#check-combined-metric-file.R
#
#Check Combined Metric Files For Errors
#
#Things to confirm with the group
# - Testing_Percent_Positive is NA when denominator (Testing_Total_Specimens) is zero
# - Hospital Data is always missing for:
#   - Counties for daily rows
#   - The following 9 counties for summary rows:
#     - Bayfield
#     - Buffalo
#     - Florence
#     - Forest
#     - Iron
#     - Kewaunee
#     - Marquette
#     - Menominee
#     - Pierce

#Setup ----
library(tidyverse)
library(ohiCovidMetrics)
library(tinytest)

load(Sys.getenv("LOADCOMBOMETRICFILE"))

# out <- read_csv("F:/covid19-redeploy/merge-others-output/combined-metrics.csv",
#                   col_types = cols(
#                     .default = col_double(),
#                     Date = col_date(format = ""),
#                     Region_ID = col_character(),
#                     Region = col_character(),
#                     RowType = col_character(),
#                     Conf_Case_Trajectory = col_character(),
#                     Conf_Case_Burden_Class = col_character(),
#                     Conf_Case_Trajectory_Class = col_character(),
#                     Conf_Case_Composite_Class = col_character(),
#                     Testing_Composite_Class = col_character(),
#                     Hosp_RunDate = col_date(format = ""),
#                     Hosp_COVID_px_Trajectory = col_character(),
#                     Hosp_COVID_px_Trajectory_Class = col_character(),
#                     Hosp_COVID_ICUpx_Trajectory = col_character(),
#                     Hosp_COVID_ICUpx_Trajectory_Class = col_character(),
#                     CLI_Trajectory = col_character(),
#                     CLI_Burden_Class = col_character(),
#                     CLI_Trajectory_Class = col_character(),
#                     CLI_Composite_Class = col_character(),
#                     ILI_Status = col_character(),
#                     Data_Period = col_character()
#                   )) %>%
#   mutate(
#     Testing_Composite_Class = case_when(
#       is.na(Testing_Composite_Class) ~ NA_character_,
#       Testing_Composite_Class == "Robust" ~ "Close to",
#       Testing_Composite_Class == "Adequate" ~ "Higher than",
#       Testing_Composite_Class == "Inadequate" ~ "Substantially Higher than",
#       TRUE ~ "ERROR"
#     )
#   ) %>%
#   select(-Total_ED_Visits)



#Global Checks ----

###all periods have 1200 observation
expect_true(all(table(out$Data_Period) == 1200),
            info = "All Periods have 1200 Obs.")

###all periods have 80 summary rows and 1120 daily rows
expect_true(all(table(out$Data_Period[out$RowType == "Summary"]) == 80),
            info = "All periods have 80 summary rows.")
expect_true(all(table(out$Data_Period[out$RowType == "Daily"]) == 1120),
            info = "All periods have 1120 daily rows.")

###all periods have 14 consecutive days
expect_true(all(out %>%
                  group_by(Data_Period) %>%
                  summarize(
                    ndates = n_distinct(Date),
                    .groups = "drop"
                  ) %>%
                  pull(ndates) == 14),
            info = "All periods have 14 unique dates.")

consec <- out %>%
  mutate(
    Date = as.numeric(Date)
  ) %>%
  select(Data_Period, Date) %>%
  group_by(Data_Period) %>%
  arrange(Data_Period, Date) %>%
  distinct() %>%
  mutate(
    rownum = row_number(Date),
    lagdiffs = Date - lag(Date),
  ) %>%
  filter(rownum > 1)

expect_true(all(consec$lagdiffs == 1),
            info = "All periods have consecutive series of dates.")

#Column Specific Checks ----

##. . Common Variables ----
###Date
expect_true(inherits(out$Date, 'Date'),
            info = "Date column is 'Date' class")
expect_equal(sum(is.na(out$Date)), 0,
             info = "Date column has no NA/missings")

###Region_ID
expect_true(inherits(out$Region_ID, 'character'),
            info = "Region_ID column is 'character' class")
expect_equal(sum(is.na(out$Region_ID)), 0,
             info = "Region_ID column has no NA/missings")
expect_equal(sort(unique(out$Region_ID)),
                 c("55",
                   "55001", "55003", "55005", "55007", "55009", "55011", "55013", "55015",
                   "55017", "55019", "55021", "55023", "55025", "55027", "55029", "55031",
                   "55033", "55035", "55037", "55039", "55041", "55043", "55045", "55047",
                   "55049", "55051", "55053", "55055", "55057", "55059", "55061", "55063",
                   "55065", "55067", "55069", "55071", "55073", "55075", "55077", "55078",
                   "55079", "55081", "55083", "55085", "55087", "55089", "55091", "55093",
                   "55095", "55097", "55099", "55101", "55103", "55105", "55107", "55109",
                   "55111", "55113", "55115", "55117", "55119", "55121", "55123", "55125",
                   "55127", "55129", "55131", "55133", "55135", "55137", "55139", "55141",
                   "Fox Valley Area", "North Central", "Northeast",
                   "Northwest", "South Central", "Southeast", "Western"),
             info = "Region_ID column has only correct unique values")

###Region
expect_true(inherits(out$Region, 'character'),
            info = "Region column is 'character' class")
expect_equal(sum(is.na(out$Region)), 0,
             info = "Region column has no NA/missings")
expect_equal(sort(unique(out$Region)),
             c("Adams", "Ashland", "Barron", "Bayfield", "Brown", "Buffalo", "Burnett", "Calumet",
               "Chippewa", "Clark", "Columbia", "Crawford", "Dane", "Dodge", "Door", "Douglas",
               "Dunn", "Eau Claire", "Florence", "Fond du Lac", "Forest", "Fox Valley Area", "Grant", "Green",
               "Green Lake", "Iowa", "Iron", "Jackson", "Jefferson", "Juneau", "Kenosha", "Kewaunee",
               "La Crosse", "Lafayette", "Langlade", "Lincoln", "Manitowoc", "Marathon", "Marinette",
               "Marquette", "Menominee", "Milwaukee", "Monroe", "North Central", "Northeast", "Northwest", "Oconto",
               "Oneida", "Outagamie", "Ozaukee", "Pepin", "Pierce", "Polk", "Portage", "Price",
               "Racine", "Richland", "Rock", "Rusk", "Sauk", "Sawyer", "Shawano", "Sheboygan",
               "South Central", "Southeast", "St. Croix", "Taylor", "Trempealeau", "Vernon", "Vilas", "Walworth",
               "Washburn", "Washington", "Waukesha", "Waupaca", "Waushara", "Western", "Winnebago", "Wisconsin", "Wood"),
             info = "Region column has only correct unique values")

###RowType
expect_true(inherits(out$RowType, 'character'),
            info = "RowType column is 'character' class")
expect_equal(sum(is.na(out$RowType)), 0,
             info = "RowType column has no NA/missings")
expect_equal(sort(unique(out$RowType)),
             c("Daily", "Summary"),
             info = "RowType column has only correct unique values")

###Data_Period
expect_true(inherits(out$Data_Period, 'character'),
            info = "Data_Period column is 'character' class")
expect_equal(sum(is.na(out$Data_Period)), 0,
             info = "Data_Period column has no NA/missings")
expect_equal(length(unique(out$Data_Period)), nrow(out)/1200,
             info = "Data_Period column has the correct number of unique values")
#the regex patten below matches valid date patterns m/d/202y - m/d/2020y
expect_true(all(grepl("^([1][0-2]|[1-9])/([1-9]|[1-2][0-9]|3[0-1])/202[0-9] - ([1][0-2]|[1-9])/([1-9]|[1-2][0-9]|3[0-1])/202[0-9]$",
                      unique(out$Data_Period))),
            info = "Data_Period values all conform to the correct pattern (m/d/202y - m/d/202y)")

##. . Confirmed Cases ----
###Conf_Case_Count
expect_true(inherits(out$Conf_Case_Count, 'numeric'),
            info = "Conf_Case_Count column is 'numeric' class")
expect_equal(sum(is.na(out$Conf_Case_Count)), 0,
             info = "Conf_Case_Count column has no NA/missings")

###Conf_Case_Burden
expect_true(inherits(out$Conf_Case_Burden, 'numeric'),
            info = "Conf_Case_Burden column is 'numeric' class")
expect_equal(sum(is.na(out$Conf_Case_Burden[out$RowType == "Summary"])), 0,
             info = "Conf_Case_Burden column has no NA/missings for Summary Rows")
expect_equal(sum(!is.na(out$Conf_Case_Burden[out$RowType == "Daily"])), 0,
             info = "Conf_Case_Burden column has *all* NA/missings for Daily Rows")

###Conf_Case_Trajectory
expect_true(inherits(out$Conf_Case_Trajectory, 'character'),
            info = "Conf_Case_Trajectory column is 'character' class")
expect_equal(sum(is.na(out$Conf_Case_Trajectory[out$RowType == "Summary"])), 0,
             info = "Conf_Case_Trajectory column has no NA/missings for Summary Rows")
expect_equal(sum(!is.na(out$Conf_Case_Trajectory[out$RowType == "Daily"])), 0,
             info = "Conf_Case_Trajectory column has *all* NA/missings for Daily Rows")

###Conf_Case_Burden_Class
expect_true(inherits(out$Conf_Case_Burden_Class, 'character'),
            info = "Conf_Case_Burden_Class column is 'character' class")
expect_equal(sum(is.na(out$Conf_Case_Burden_Class[out$RowType == "Summary"])), 0,
             info = "Conf_Case_Burden_Class column has no NA/missings for Summary Rows")
expect_equal(sum(!is.na(out$Conf_Case_Burden_Class[out$RowType == "Daily"])), 0,
             info = "Conf_Case_Burden_Class column has *all* NA/missings for Daily Rows")
expect_equal(sort(unique(out$Conf_Case_Burden_Class)),
             c("High", "Low", "Moderate", "Moderately High"),
             info = "Conf_Case_Burden_Class column has only correct unique values")

###Conf_Case_Trajectory_Class
expect_true(inherits(out$Conf_Case_Trajectory_Class, 'character'),
            info = "Conf_Case_Trajectory_Class column is 'character' class")
expect_equal(sum(is.na(out$Conf_Case_Trajectory_Class[out$RowType == "Summary"])), 0,
             info = "Conf_Case_Trajectory_Class column has no NA/missings for Summary Rows")
expect_equal(sum(!is.na(out$Conf_Case_Trajectory_Class[out$RowType == "Daily"])), 0,
             info = "Conf_Case_Trajectory_Class column has *all* NA/missings for Daily Rows")
expect_equal(sort(unique(out$Conf_Case_Trajectory_Class)),
             c("Growing", "No significant change", "Shrinking"),
             info = "Conf_Case_Trajectory_Class column has only correct unique values")

###Conf_Case_Composite_Class
expect_true(inherits(out$Conf_Case_Composite_Class, 'character'),
            info = "Conf_Case_Composite_Class column is 'character' class")
expect_equal(sum(is.na(out$Conf_Case_Composite_Class[out$RowType == "Summary"])), 0,
             info = "Conf_Case_Composite_Class column has no NA/missings for Summary Rows")
expect_equal(sum(!is.na(out$Conf_Case_Composite_Class[out$RowType == "Daily"])), 0,
             info = "Conf_Case_Composite_Class column has *all* NA/missings for Daily Rows")
expect_equal(sort(unique(out$Conf_Case_Composite_Class)),
             c("High", "Low", "Medium"),
             info = "Conf_Case_Composite_Class column has only correct unique values")

###Conf_Case_Trajectory_P
expect_true(inherits(out$Conf_Case_Trajectory_P, 'numeric'),
            info = "Conf_Case_Trajectory_P column is 'numeric' class")
expect_equal(sum(is.na(out$Conf_Case_Trajectory_P[out$RowType == "Summary"])), 0,
             info = "Conf_Case_Trajectory_P column has no NA/missings for Summary Rows")
expect_equal(sum(!is.na(out$Conf_Case_Trajectory_P[out$RowType == "Daily"])), 0,
             info = "Conf_Case_Trajectory_P column has *all* NA/missings for Daily Rows")
expect_true(all(dplyr::between(out$Conf_Case_Trajectory_P[out$RowType == "Summary"], left = 0.0, right = 1.0)),
            info = "Conf_Case_Trajectory_P columns values are all between 0 and 1 inclusive.")

###Conf_Case_Trajectory_FDR
expect_true(inherits(out$Conf_Case_Trajectory_FDR, 'numeric'),
            info = "Conf_Case_Trajectory_FDR column is 'numeric' class")
expect_equal(sum(is.na(out$Conf_Case_Trajectory_FDR[out$RowType == "Summary"])), 0,
             info = "Conf_Case_Trajectory_FDR column has no NA/missings for Summary Rows")
expect_equal(sum(!is.na(out$Conf_Case_Trajectory_FDR[out$RowType == "Daily"])), 0,
             info = "Conf_Case_Trajectory_FDR column has *all* NA/missings for Daily Rows")
expect_true(all(dplyr::between(out$Conf_Case_Trajectory_FDR[out$RowType == "Summary"], left = 0.0, right = 1.0)),
            info = "Conf_Case_Trajectory_FDR columns values are all between 0 and 1 inclusive.")

##. . Testing/Case Detection ----
###Testing_Positive_Specimens
expect_true(inherits(out$Testing_Positive_Specimens, 'numeric'),
            info = "Testing_Positive_Specimens column is 'numeric' class")
expect_equal(sum(is.na(out$Testing_Positive_Specimens)), 0,
             info = "Testing_Positive_Specimens column has no NA/missings")

###Testing_Nonpositive_Specimens
expect_true(inherits(out$Testing_Nonpositive_Specimens, 'numeric'),
            info = "Testing_Nonpositive_Specimens column is 'numeric' class")
expect_equal(sum(is.na(out$Testing_Positive_Specimens)), 0,
             info = "Testing_Nonpositive_Specimens column has no NA/missings")

###Testing_Total_Specimens
expect_true(inherits(out$Testing_Total_Specimens, 'numeric'),
            info = "Testing_Total_Specimens column is 'numeric' class")
expect_equal(sum(is.na(out$Testing_Total_Specimens)), 0,
             info = "Testing_Total_Specimens column has no NA/missings")

###Testing_Percent_Positive
expect_true(inherits(out$Testing_Percent_Positive, 'numeric'),
            info = "Testing_Percent_Positive column is 'numeric' class")
expect_equal(sum(is.na(out$Testing_Percent_Positive[out$Testing_Total_Specimens > 0])), 0,
             info = "Testing_Percent_Positive column has no NA/missings")
expect_equal(sum(!is.na(out$Testing_Percent_Positive[out$Testing_Total_Specimens == 0])), 0,
             info = "Testing_Percent_Positive column is always NA/missing if denominator is zero")
expect_true(all(dplyr::between(out$Testing_Percent_Positive[out$Testing_Total_Specimens > 0], left = 0.0, right = 100.0)),
            info = "Testing_Percent_Positive columns values are all between 0 and 100 inclusive.")
expect_true(any(out$Testing_Percent_Positive[out$Testing_Total_Specimens > 0] > 1.0),
            info = "Testing_Percent_Positive columns values are scales between 0 and 100 (not 0 and 1).")


###Testing_Incident_Tests
expect_true(inherits(out$Testing_Incident_Tests, 'numeric'),
            info = "Testing_Incident_Tests column is 'numeric' class")
expect_equal(sum(is.na(out$Testing_Incident_Tests)), 0,
             info = "Testing_Incident_Tests column has no NA/missings")

###Testing_Incident_Test_Target
expect_true(inherits(out$Testing_Incident_Test_Target, 'numeric'),
            info = "Testing_Incident_Test_Target column is 'numeric' class")
expect_equal(sum(is.na(out$Testing_Incident_Test_Target[out$RowType == "Summary"])), 0,
             info = "Testing_Incident_Test_Target column has no NA/missings for Summary Rows")
expect_equal(sum(!is.na(out$Testing_Incident_Test_Target[out$RowType == "Daily"])), 0,
             info = "Testing_Incident_Test_Target column has *all* NA/missings for Daily Rows")

###Testing_Percent_of_Target
expect_true(inherits(out$Testing_Percent_of_Target, 'numeric'),
            info = "Testing_Percent_of_Target column is 'numeric' class")
expect_equal(sum(is.na(out$Testing_Percent_of_Target[out$RowType == "Summary"])), 0,
             info = "Testing_Percent_of_Target column has no NA/missings for Summary Rows")
expect_equal(sum(!is.na(out$Testing_Percent_of_Target[out$RowType == "Daily"])), 0,
             info = "Testing_Percent_of_Target column has *all* NA/missings for Daily Rows")
expect_true(any(out$Testing_Percent_of_Target[out$RowType == "Summary"] > 1.0),
            info = "Testing_Percent_of_Target columns values are scales between 0 and 100+ (not 0 and 1+).")

###Testing_Composite_Class
expect_true(inherits(out$Testing_Composite_Class, 'character'),
            info = "Testing_Composite_Class column is 'character' class")
expect_equal(sum(is.na(out$Testing_Composite_Class[out$RowType == "Summary"])), 0,
             info = "Testing_Composite_Class column has no NA/missings for Summary Rows")
expect_equal(sum(!is.na(out$Testing_Composite_Class[out$RowType == "Daily"])), 0,
             info = "Testing_Composite_Class column has *all* NA/missings for Daily Rows")
expect_equal(sort(unique(out$Testing_Composite_Class)),
             c("High (more than 10% positive)", "Low (less than 5% positive)", "Moderate (5% to 10% positive)"),
             info = "Testing_Composite_Class column has only correct unique values")

##. . Hospital ----
miss_hosp_cty <- c("Bayfield", "Buffalo", "Florence", "Forest",
                   "Iron", "Kewaunee", "Marquette", "Menominee",
                   "Pierce")
non_cty <- c("Wisconsin", "Fox Valley Area", "North Central", "Northeast",
             "Northwest", "South Central", "Southeast", "Western")

###Hosp_RunDate
expect_true(inherits(out$Hosp_RunDate, 'Date'),
            info = "Hosp_RunDate column is 'Date' class")
expect_equal(sum(is.na(out$Hosp_RunDate[out$RowType == "Summary" & !(out$Region %in% miss_hosp_cty)])), 0,
             info = "Hosp_RunDate column has no NA/missings for summary rows excluding: Bayfield, Buffalo, Florence, Forest, Iron, Kewaunee, Marquette, Menominee, Pierce")
expect_equal(sum(is.na(out$Hosp_RunDate[out$RowType == "Daily" & out$Region %in% non_cty])), 0,
             info = "Hosp_RunDate column has no NA/missings for daily rows for state and HERC regions")
expect_equal(sum(!is.na(out$Hosp_RunDate[out$RowType == "Summary" & out$Region %in% miss_hosp_cty])) +
             sum(!is.na(out$Hosp_RunDate[out$RowType == "Daily" & !(out$Region %in% non_cty)])), 0,
             info = "Hosp_RunDate column is always NA/missings when expected")

###Hosp_dailyCOVID_px
expect_true(inherits(out$Hosp_dailyCOVID_px, 'numeric'),
            info = "Hosp_dailyCOVID_px column is 'numeric' class")
expect_equal(sum(!is.na(out$Hosp_dailyCOVID_px[out$RowType == "Summary"])), 0,
             info = "Hosp_dailyCOVID_px column has only NA/missings for summary")
expect_equal(sum(is.na(out$Hosp_dailyCOVID_px[out$RowType == "Daily" & out$Region %in% non_cty])), 0,
             info = "Hosp_dailyCOVID_px column has no NA/missings for daily rows for state and HERC regions")
expect_equal(sum(!is.na(out$Hosp_dailyCOVID_px[out$RowType == "Daily" & !(out$Region %in% non_cty)])), 0,
             info = "Hosp_dailyCOVID_px column is always NA/missings for county daily rows")

###Hosp_dailyCOVID_ICUpx
expect_true(inherits(out$Hosp_dailyCOVID_ICUpx, 'numeric'),
            info = "Hosp_dailyCOVID_ICUpx column is 'numeric' class")
expect_equal(sum(!is.na(out$Hosp_dailyCOVID_ICUpx[out$RowType == "Summary"])), 0,
             info = "Hosp_dailyCOVID_ICUpx column has only NA/missings for summary")
expect_equal(sum(is.na(out$Hosp_dailyCOVID_ICUpx[out$RowType == "Daily" & out$Region %in% non_cty])), 0,
             info = "Hosp_dailyCOVID_ICUpx column has no NA/missings for daily rows for state and HERC regions")
expect_equal(sum(!is.na(out$Hosp_dailyCOVID_ICUpx[out$RowType == "Daily" & !(out$Region %in% non_cty)])), 0,
             info = "Hosp_dailyCOVID_ICUpx column is always NA/missings for county daily rows")

###Hosp_totalbeds
expect_true(inherits(out$Hosp_totalbeds, 'numeric'),
            info = "Hosp_totalbeds column is 'numeric' class")
expect_equal(sum(!is.na(out$Hosp_totalbeds[out$RowType == "Summary"])), 0,
             info = "Hosp_totalbeds column has only NA/missings for summary")
expect_equal(sum(is.na(out$Hosp_totalbeds[out$RowType == "Daily" & out$Region %in% non_cty])), 0,
             info = "Hosp_totalbeds column has no NA/missings for daily rows for state and HERC regions")
expect_equal(sum(!is.na(out$Hosp_totalbeds[out$RowType == "Daily" & !(out$Region %in% non_cty)])), 0,
             info = "Hosp_totalbeds column is always NA/missings for county daily rows")

###Hosp_beds_IBA
expect_true(inherits(out$Hosp_beds_IBA, 'numeric'),
            info = "Hosp_beds_IBA column is 'numeric' class")
expect_equal(sum(!is.na(out$Hosp_beds_IBA[out$RowType == "Summary"])), 0,
             info = "Hosp_beds_IBA column has only NA/missings for summary")
expect_equal(sum(is.na(out$Hosp_beds_IBA[out$RowType == "Daily" & out$Region %in% non_cty])), 0,
             info = "Hosp_beds_IBA column has no NA/missings for daily rows for state and HERC regions")
expect_equal(sum(!is.na(out$Hosp_beds_IBA[out$RowType == "Daily" & !(out$Region %in% non_cty)])), 0,
             info = "Hosp_beds_IBA column is always NA/missings for county daily rows")

###Hosp_totalICU
expect_true(inherits(out$Hosp_totalICU, 'numeric'),
            info = "Hosp_totalICU column is 'numeric' class")
expect_equal(sum(!is.na(out$Hosp_totalICU[out$RowType == "Summary"])), 0,
             info = "Hosp_totalICU column has only NA/missings for summary")
expect_equal(sum(is.na(out$Hosp_totalICU[out$RowType == "Daily" & out$Region %in% non_cty])), 0,
             info = "Hosp_totalICU column has no NA/missings for daily rows for state and HERC regions")
expect_equal(sum(!is.na(out$Hosp_totalICU[out$RowType == "Daily" & !(out$Region %in% non_cty)])), 0,
             info = "Hosp_totalICU column is always NA/missings for county daily rows")

###Hosp_ICU_IBA
expect_true(inherits(out$Hosp_ICU_IBA, 'numeric'),
            info = "Hosp_ICU_IBA column is 'numeric' class")
expect_equal(sum(!is.na(out$Hosp_ICU_IBA[out$RowType == "Summary"])), 0,
             info = "Hosp_ICU_IBA column has only NA/missings for summary")
expect_equal(sum(is.na(out$Hosp_ICU_IBA[out$RowType == "Daily" & out$Region %in% non_cty])), 0,
             info = "Hosp_ICU_IBA column has no NA/missings for daily rows for state and HERC regions")
expect_equal(sum(!is.na(out$Hosp_ICU_IBA[out$RowType == "Daily" & !(out$Region %in% non_cty)])), 0,
             info = "Hosp_ICU_IBA column is always NA/missings for county daily rows")

###Hosp_num_px_vent
expect_true(inherits(out$Hosp_num_px_vent, 'numeric'),
            info = "Hosp_num_px_vent column is 'numeric' class")
expect_equal(sum(!is.na(out$Hosp_num_px_vent[out$RowType == "Summary"])), 0,
             info = "Hosp_num_px_vent column has only NA/missings for summary")
expect_equal(sum(is.na(out$Hosp_num_px_vent[out$RowType == "Daily" & out$Region %in% non_cty])), 0,
             info = "Hosp_num_px_vent column has no NA/missings for daily rows for state and HERC regions")
expect_equal(sum(!is.na(out$Hosp_num_px_vent[out$RowType == "Daily" & !(out$Region %in% non_cty)])), 0,
             info = "Hosp_num_px_vent column is always NA/missings for county daily rows")

###Hosp_total_vents
expect_true(inherits(out$Hosp_total_vents, 'numeric'),
            info = "Hosp_total_vents column is 'numeric' class")
expect_equal(sum(!is.na(out$Hosp_total_vents[out$RowType == "Summary"])), 0,
             info = "Hosp_total_vents column has only NA/missings for summary")
expect_equal(sum(is.na(out$Hosp_total_vents[out$RowType == "Daily" & out$Region %in% non_cty])), 0,
             info = "Hosp_total_vents column has no NA/missings for daily rows for state and HERC regions")
expect_equal(sum(!is.na(out$Hosp_total_vents[out$RowType == "Daily" & !(out$Region %in% non_cty)])), 0,
             info = "Hosp_total_vents column is always NA/missings for county daily rows")

###Hosp_intermed_beds_IBA
expect_true(inherits(out$Hosp_intermed_beds_IBA, 'numeric'),
            info = "Hosp_intermed_beds_IBA column is 'numeric' class")
expect_equal(sum(!is.na(out$Hosp_intermed_beds_IBA[out$RowType == "Summary"])), 0,
             info = "Hosp_intermed_beds_IBA column has only NA/missings for summary")
expect_equal(sum(is.na(out$Hosp_intermed_beds_IBA[out$RowType == "Daily" & out$Region %in% non_cty])), 0,
             info = "Hosp_intermed_beds_IBA column has no NA/missings for daily rows for state and HERC regions")
expect_equal(sum(!is.na(out$Hosp_intermed_beds_IBA[out$RowType == "Daily" & !(out$Region %in% non_cty)])), 0,
             info = "Hosp_intermed_beds_IBA column is always NA/missings for county daily rows")

###Hosp_negflow_beds_IBA
expect_true(inherits(out$Hosp_negflow_beds_IBA, 'numeric'),
            info = "Hosp_negflow_beds_IBA column is 'numeric' class")
expect_equal(sum(!is.na(out$Hosp_negflow_beds_IBA[out$RowType == "Summary"])), 0,
             info = "Hosp_negflow_beds_IBA column has only NA/missings for summary")
expect_equal(sum(is.na(out$Hosp_negflow_beds_IBA[out$RowType == "Daily" & out$Region %in% non_cty])), 0,
             info = "Hosp_negflow_beds_IBA column has no NA/missings for daily rows for state and HERC regions")
expect_equal(sum(!is.na(out$Hosp_negflow_beds_IBA[out$RowType == "Daily" & !(out$Region %in% non_cty)])), 0,
             info = "Hosp_negflow_beds_IBA column is always NA/missings for county daily rows")

###Hosp_medsurg_beds_IBA
expect_true(inherits(out$Hosp_medsurg_beds_IBA, 'numeric'),
            info = "Hosp_medsurg_beds_IBA column is 'numeric' class")
expect_equal(sum(!is.na(out$Hosp_medsurg_beds_IBA[out$RowType == "Summary"])), 0,
             info = "Hosp_medsurg_beds_IBA column has only NA/missings for summary")
expect_equal(sum(is.na(out$Hosp_medsurg_beds_IBA[out$RowType == "Daily" & out$Region %in% non_cty])), 0,
             info = "Hosp_medsurg_beds_IBA column has no NA/missings for daily rows for state and HERC regions")
expect_equal(sum(!is.na(out$Hosp_medsurg_beds_IBA[out$RowType == "Daily" & !(out$Region %in% non_cty)])), 0,
             info = "Hosp_medsurg_beds_IBA column is always NA/missings for county daily rows")

###Hosp_PrctBeds_IBA
expect_true(inherits(out$Hosp_PrctBeds_IBA, 'numeric'),
            info = "Hosp_PrctBeds_IBA column is 'numeric' class")
expect_equal(sum(!is.na(out$Hosp_PrctBeds_IBA[out$RowType == "Summary"])), 0,
             info = "Hosp_PrctBeds_IBA column has only NA/missings for summary")
expect_equal(sum(is.na(out$Hosp_PrctBeds_IBA[out$RowType == "Daily" & out$Region %in% non_cty])), 0,
             info = "Hosp_PrctBeds_IBA column has no NA/missings for daily rows for state and HERC regions")
expect_equal(sum(!is.na(out$Hosp_PrctBeds_IBA[out$RowType == "Daily" & !(out$Region %in% non_cty)])), 0,
             info = "Hosp_PrctBeds_IBA column is always NA/missings for county daily rows")
expect_true(all(dplyr::between(out$Hosp_PrctBeds_IBA[out$RowType == "Daily" & out$Region %in% non_cty], left = 0.0, right = 100.0)),
            info = "Hosp_PrctBeds_IBA columns values are all between 0 and 100 inclusive.")
expect_true(any(out$Hosp_PrctBeds_IBA[out$RowType == "Daily" & out$Region %in% non_cty] > 1.0),
            info = "Hosp_PrctBeds_IBA columns values are scales between 0 and 100 (not 0 and 1).")

###Hosp_PrctICU_IBA
expect_true(inherits(out$Hosp_PrctICU_IBA, 'numeric'),
            info = "Hosp_PrctICU_IBA column is 'numeric' class")
expect_equal(sum(!is.na(out$Hosp_PrctICU_IBA[out$RowType == "Summary"])), 0,
             info = "Hosp_PrctICU_IBA column has only NA/missings for summary")
expect_equal(sum(is.na(out$Hosp_PrctICU_IBA[out$RowType == "Daily" & out$Region %in% non_cty])), 0,
             info = "Hosp_PrctICU_IBA column has no NA/missings for daily rows for state and HERC regions")
expect_equal(sum(!is.na(out$Hosp_PrctICU_IBA[out$RowType == "Daily" & !(out$Region %in% non_cty)])), 0,
             info = "Hosp_PrctICU_IBA column is always NA/missings for county daily rows")
expect_true(all(dplyr::between(out$Hosp_PrctICU_IBA[out$RowType == "Daily" & out$Region %in% non_cty], left = 0.0, right = 100.0)),
            info = "Hosp_PrctICU_IBA columns values are all between 0 and 100 inclusive.")
expect_true(any(out$Hosp_PrctICU_IBA[out$RowType == "Daily" & out$Region %in% non_cty] > 1.0),
            info = "Hosp_PrctICU_IBA columns values are scales between 0 and 100 (not 0 and 1).")

###Hosp_PrctVent_Used
expect_true(inherits(out$Hosp_PrctVent_Used, 'numeric'),
            info = "Hosp_PrctVent_Used column is 'numeric' class")
expect_equal(sum(!is.na(out$Hosp_PrctVent_Used[out$RowType == "Summary"])), 0,
             info = "Hosp_PrctVent_Used column has only NA/missings for summary")
expect_equal(sum(is.na(out$Hosp_PrctVent_Used[out$RowType == "Daily" & out$Region %in% non_cty])), 0,
             info = "Hosp_PrctVent_Used column has no NA/missings for daily rows for state and HERC regions")
expect_equal(sum(!is.na(out$Hosp_PrctVent_Used[out$RowType == "Daily" & !(out$Region %in% non_cty)])), 0,
             info = "Hosp_PrctVent_Used column is always NA/missings for county daily rows")
expect_true(all(dplyr::between(out$Hosp_PrctVent_Used[out$RowType == "Daily" & out$Region %in% non_cty], left = 0.0, right = 100.0)),
            info = "Hosp_PrctVent_Used columns values are all between 0 and 100 inclusive.")
expect_true(any(out$Hosp_PrctVent_Used[out$RowType == "Daily" & out$Region %in% non_cty] > 1.0),
            info = "Hosp_PrctVent_Used columns values are scales between 0 and 100 (not 0 and 1).")

###Hosp_COVID_px_Trajectory
expect_true(inherits(out$Hosp_COVID_px_Trajectory, 'character'),
            info = "Hosp_COVID_px_Trajectory column is 'character' class")
expect_equal(sum(is.na(out$Hosp_COVID_px_Trajectory[out$RowType == "Summary" & !(out$Region %in% miss_hosp_cty)])), 0,
             info = "Hosp_COVID_px_Trajectory column has no NA/missings for summary rows excluding: Bayfield, Buffalo, Florence, Forest, Iron, Kewaunee, Marquette, Menominee, Pierce")
expect_equal(sum(!is.na(out$Hosp_COVID_px_Trajectory[out$RowType == "Daily"])), 0,
             info = "Hosp_COVID_px_Trajectory column has *all* NA/missings for Daily Rows")
expect_equal(sum(!is.na(out$Hosp_COVID_px_Trajectory[out$RowType == "Summary" & out$Region %in% miss_hosp_cty])), 0,
             info = "Hosp_COVID_px_Trajectory column is always NA/missings when expected")

###Hosp_COVID_px_Trajectory_Class
expect_true(inherits(out$Hosp_COVID_px_Trajectory_Class, 'character'),
            info = "Hosp_COVID_px_Trajectory_Class column is 'character' class")
expect_equal(sum(is.na(out$Hosp_COVID_px_Trajectory_Class[out$RowType == "Summary" & !(out$Region %in% miss_hosp_cty)])), 0,
             info = "Hosp_COVID_px_Trajectory_Class column has no NA/missings for summary rows excluding: Bayfield, Buffalo, Florence, Forest, Iron, Kewaunee, Marquette, Menominee, Pierce")
expect_equal(sum(!is.na(out$Hosp_COVID_px_Trajectory_Class[out$RowType == "Daily"])), 0,
             info = "Hosp_COVID_px_Trajectory_Class column has *all* NA/missings for Daily Rows")
expect_equal(sum(!is.na(out$Hosp_COVID_px_Trajectory_Class[out$RowType == "Summary" & out$Region %in% miss_hosp_cty])), 0,
             info = "Hosp_COVID_px_Trajectory_Class column is always NA/missings when expected")
expect_equal(sort(unique(out$Hosp_COVID_px_Trajectory_Class[out$RowType == "Summary" & !(out$Region %in% miss_hosp_cty)])),
             c("Growing", "No significant change", "Shrinking"),
             info = "Hosp_COVID_px_Trajectory_Class column has only correct unique values")

###Hosp_COVID_ICUpx_Trajectory
expect_true(inherits(out$Hosp_COVID_ICUpx_Trajectory, 'character'),
            info = "Hosp_COVID_ICUpx_Trajectory column is 'character' class")
expect_equal(sum(is.na(out$Hosp_COVID_ICUpx_Trajectory[out$RowType == "Summary" & !(out$Region %in% miss_hosp_cty)])), 0,
             info = "Hosp_COVID_ICUpx_Trajectory column has no NA/missings for summary rows excluding: Bayfield, Buffalo, Florence, Forest, Iron, Kewaunee, Marquette, Menominee, Pierce")
expect_equal(sum(!is.na(out$Hosp_COVID_ICUpx_Trajectory[out$RowType == "Daily"])), 0,
             info = "Hosp_COVID_ICUpx_Trajectory column has *all* NA/missings for Daily Rows")
expect_equal(sum(!is.na(out$Hosp_COVID_ICUpx_Trajectory[out$RowType == "Summary" & out$Region %in% miss_hosp_cty])), 0,
             info = "Hosp_COVID_ICUpx_Trajectory column is always NA/missings when expected")

###Hosp_COVID_ICUpx_Trajectory_Class
expect_true(inherits(out$Hosp_COVID_ICUpx_Trajectory_Class, 'character'),
            info = "Hosp_COVID_ICUpx_Trajectory_Class column is 'character' class")
expect_equal(sum(is.na(out$Hosp_COVID_ICUpx_Trajectory_Class[out$RowType == "Summary" & !(out$Region %in% miss_hosp_cty)])), 0,
             info = "Hosp_COVID_ICUpx_Trajectory_Class column has no NA/missings for summary rows excluding: Bayfield, Buffalo, Florence, Forest, Iron, Kewaunee, Marquette, Menominee, Pierce")
expect_equal(sum(!is.na(out$Hosp_COVID_ICUpx_Trajectory_Class[out$RowType == "Daily"])), 0,
             info = "Hosp_COVID_ICUpx_Trajectory_Class column has *all* NA/missings for Daily Rows")
expect_equal(sum(!is.na(out$Hosp_COVID_ICUpx_Trajectory_Class[out$RowType == "Summary" & out$Region %in% miss_hosp_cty])), 0,
             info = "Hosp_COVID_ICUpx_Trajectory_Class column is always NA/missings when expected")
expect_equal(sort(unique(out$Hosp_COVID_ICUpx_Trajectory_Class[out$RowType == "Summary" & !(out$Region %in% miss_hosp_cty)])),
             c("Growing", "No significant change", "Shrinking"),
             info = "Hosp_COVID_ICUpx_Trajectory_Class column has only correct unique values")

###Hosp_PrctBeds_Used
expect_true(inherits(out$Hosp_PrctBeds_Used, 'numeric'),
            info = "Hosp_PrctBeds_Used column is 'numeric' class")
expect_equal(sum(!is.na(out$Hosp_PrctBeds_Used[out$RowType == "Summary"])), 0,
             info = "Hosp_PrctBeds_Used column has only NA/missings for summary")
expect_equal(sum(is.na(out$Hosp_PrctBeds_Used[out$RowType == "Daily" & out$Region %in% non_cty])), 0,
             info = "Hosp_PrctBeds_Used column has no NA/missings for daily rows for state and HERC regions")
expect_equal(sum(!is.na(out$Hosp_PrctBeds_Used[out$RowType == "Daily" & !(out$Region %in% non_cty)])), 0,
             info = "Hosp_PrctBeds_Used column is always NA/missings for county daily rows")
expect_true(all(dplyr::between(out$Hosp_PrctBeds_Used[out$RowType == "Daily" & out$Region %in% non_cty], left = 0.0, right = 100.0)),
            info = "Hosp_PrctBeds_Used columns values are all between 0 and 100 inclusive.")
expect_true(any(out$Hosp_PrctBeds_Used[out$RowType == "Daily" & out$Region %in% non_cty] > 1.0),
            info = "Hosp_PrctBeds_Used columns values are scales between 0 and 100 (not 0 and 1).")

###Hosp_PrctICU_Used
expect_true(inherits(out$Hosp_PrctICU_Used, 'numeric'),
            info = "Hosp_PrctICU_Used column is 'numeric' class")
expect_equal(sum(!is.na(out$Hosp_PrctICU_Used[out$RowType == "Summary"])), 0,
             info = "Hosp_PrctICU_Used column has only NA/missings for summary")
expect_equal(sum(is.na(out$Hosp_PrctICU_Used[out$RowType == "Daily" & out$Region %in% non_cty])), 0,
             info = "Hosp_PrctICU_Used column has no NA/missings for daily rows for state and HERC regions")
expect_equal(sum(!is.na(out$Hosp_PrctICU_Used[out$RowType == "Daily" & !(out$Region %in% non_cty)])), 0,
             info = "Hosp_PrctICU_Used column is always NA/missings for county daily rows")
expect_true(all(dplyr::between(out$Hosp_PrctICU_Used[out$RowType == "Daily" & out$Region %in% non_cty], left = 0.0, right = 100.0)),
            info = "Hosp_PrctICU_Used columns values are all between 0 and 100 inclusive.")
expect_true(any(out$Hosp_PrctICU_Used[out$RowType == "Daily" & out$Region %in% non_cty] > 1.0),
            info = "Hosp_PrctICU_Used columns values are scales between 0 and 100 (not 0 and 1).")

###Hosp_Beds_moving_avg
hosp_ma <- out %>%
  group_by(Data_Period, Region) %>%
  mutate(
    burn_obs = row_number(Date),
    ma_present = if_else(RowType == "Daily" & Region %in% non_cty & burn_obs > 6, TRUE, FALSE)
  )

expect_true(inherits(hosp_ma$Hosp_Beds_moving_avg, 'numeric'),
            info = "Hosp_Beds_moving_avg column is 'numeric' class")
expect_equal(sum(is.na(hosp_ma$Hosp_Beds_moving_avg[hosp_ma$ma_present])), 0,
             info = "Hosp_Beds_moving_avg column has all expected nonmissing values")
expect_equal(sum(!is.na(hosp_ma$Hosp_Beds_moving_avg[!hosp_ma$ma_present])), 0,
             info = "Hosp_Beds_moving_avg column is always NA/missing when expected")
expect_true(all(dplyr::between(hosp_ma$Hosp_Beds_moving_avg[hosp_ma$ma_present], left = 0.0, right = 100.0)),
            info = "Hosp_Beds_moving_avg columns values are all between 0 and 100 inclusive.")
expect_true(any(hosp_ma$Hosp_Beds_moving_avg[hosp_ma$ma_present] > 1.0),
            info = "Hosp_Beds_moving_avg columns values are scales between 0 and 100 (not 0 and 1).")

###Hosp_ICU_moving_avg
expect_true(inherits(hosp_ma$Hosp_ICU_moving_avg, 'numeric'),
            info = "Hosp_ICU_moving_avg column is 'numeric' class")
expect_equal(sum(is.na(hosp_ma$Hosp_ICU_moving_avg[hosp_ma$ma_present])), 0,
             info = "Hosp_ICU_moving_avg column has all expected nonmissing values")
expect_equal(sum(!is.na(hosp_ma$Hosp_ICU_moving_avg[!hosp_ma$ma_present])), 0,
             info = "Hosp_ICU_moving_avg column is always NA/missing when expected")
expect_true(all(dplyr::between(hosp_ma$Hosp_ICU_moving_avg[hosp_ma$ma_present], left = 0.0, right = 100.0)),
            info = "Hosp_ICU_moving_avg columns values are all between 0 and 100 inclusive.")
expect_true(any(hosp_ma$Hosp_ICU_moving_avg[hosp_ma$ma_present] > 1.0),
            info = "Hosp_ICU_moving_avg columns values are scales between 0 and 100 (not 0 and 1).")

###Hosp_Vent_moving_avg
expect_true(inherits(hosp_ma$Hosp_Vent_moving_avg, 'numeric'),
            info = "Hosp_Vent_moving_avg column is 'numeric' class")
expect_equal(sum(is.na(hosp_ma$Hosp_Vent_moving_avg[hosp_ma$ma_present])), 0,
             info = "Hosp_Vent_moving_avg column has all expected nonmissing values")
expect_equal(sum(!is.na(hosp_ma$Hosp_Vent_moving_avg[!hosp_ma$ma_present])), 0,
             info = "Hosp_Vent_moving_avg column is always NA/missing when expected")
expect_true(all(dplyr::between(hosp_ma$Hosp_Vent_moving_avg[hosp_ma$ma_present], left = 0.0, right = 100.0)),
            info = "Hosp_Vent_moving_avg columns values are all between 0 and 100 inclusive.")
expect_true(any(hosp_ma$Hosp_Vent_moving_avg[hosp_ma$ma_present] > 1.0),
            info = "Hosp_Vent_moving_avg columns values are scales between 0 and 100 (not 0 and 1).")

##. . CLI ----
###CLI_Count
expect_true(inherits(out$CLI_Count, 'numeric'),
            info = "CLI_Count column is 'numeric' class")
expect_equal(sum(is.na(out$CLI_Count)), 0,
             info = "CLI_Count column has no NA/missings")

###CLI_Burden
expect_true(inherits(out$CLI_Burden, 'numeric'),
            info = "CLI_Burden column is 'numeric' class")
expect_equal(sum(is.na(out$CLI_Burden[out$RowType == "Summary"])), 0,
             info = "CLI_Burden column has no NA/missings for Summary Rows")
expect_equal(sum(!is.na(out$CLI_Burden[out$RowType == "Daily"])), 0,
             info = "CLI_Burden column has *all* NA/missings for Daily Rows")

###CLI_Trajectory
expect_true(inherits(out$CLI_Trajectory, 'character'),
            info = "CLI_Trajectory column is 'character' class")
expect_equal(sum(is.na(out$CLI_Trajectory[out$RowType == "Summary"])), 0,
             info = "CLI_Trajectory column has no NA/missings for Summary Rows")
expect_equal(sum(!is.na(out$CLI_Trajectory[out$RowType == "Daily"])), 0,
             info = "CLI_Trajectory column has *all* NA/missings for Daily Rows")

###CLI_Burden_Class
expect_true(inherits(out$CLI_Burden_Class, 'character'),
            info = "CLI_Burden_Class column is 'character' class")
expect_equal(sum(is.na(out$CLI_Burden_Class[out$RowType == "Summary"])), 0,
             info = "CLI_Burden_Class column has no NA/missings for Summary Rows")
expect_equal(sum(!is.na(out$CLI_Burden_Class[out$RowType == "Daily"])), 0,
             info = "CLI_Burden_Class column has *all* NA/missings for Daily Rows")
expect_equal(sort(unique(out$CLI_Burden_Class)),
             c("High", "Low", "Moderate", "Moderately High"),
             info = "CLI_Burden_Class column has only correct unique values")

###CLI_Trajectory_Class
expect_true(inherits(out$CLI_Trajectory_Class, 'character'),
            info = "CLI_Trajectory_Class column is 'character' class")
expect_equal(sum(is.na(out$CLI_Trajectory_Class[out$RowType == "Summary"])), 0,
             info = "CLI_Trajectory_Class column has no NA/missings for Summary Rows")
expect_equal(sum(!is.na(out$CLI_Trajectory_Class[out$RowType == "Daily"])), 0,
             info = "CLI_Trajectory_Class column has *all* NA/missings for Daily Rows")
expect_equal(sort(unique(out$CLI_Trajectory_Class)),
             c("Growing", "No significant change", "Shrinking"),
             info = "CLI_Trajectory_Class column has only correct unique values")

###CLI_Composite_Class
expect_true(inherits(out$CLI_Composite_Class, 'character'),
            info = "CLI_Composite_Class column is 'character' class")
expect_equal(sum(is.na(out$CLI_Composite_Class[out$RowType == "Summary"])), 0,
             info = "CLI_Composite_Class column has no NA/missings for Summary Rows")
expect_equal(sum(!is.na(out$CLI_Composite_Class[out$RowType == "Daily"])), 0,
             info = "CLI_Composite_Class column has *all* NA/missings for Daily Rows")
expect_equal(sort(unique(out$CLI_Composite_Class)),
             c("High", "Low", "Medium"),
             info = "CLI_Composite_Class column has only correct unique values")

###CLI_Trajectory_P
expect_true(inherits(out$CLI_Trajectory_P, 'numeric'),
            info = "CLI_Trajectory_P column is 'numeric' class")
expect_equal(sum(is.na(out$CLI_Trajectory_P[out$RowType == "Summary"])), 0,
             info = "CLI_Trajectory_P column has no NA/missings for Summary Rows")
expect_equal(sum(!is.na(out$CLI_Trajectory_P[out$RowType == "Daily"])), 0,
             info = "CLI_Trajectory_P column has *all* NA/missings for Daily Rows")
expect_true(all(dplyr::between(out$CLI_Trajectory_P[out$RowType == "Summary"], left = 0.0, right = 1.0)),
            info = "CLI_Trajectory_P columns values are all between 0 and 1 inclusive.")

###CLI_Trajectory_FDR
expect_true(inherits(out$CLI_Trajectory_FDR, 'numeric'),
            info = "CLI_Trajectory_FDR column is 'numeric' class")
expect_equal(sum(is.na(out$CLI_Trajectory_FDR[out$RowType == "Summary"])), 0,
             info = "CLI_Trajectory_FDR column has no NA/missings for Summary Rows")
expect_equal(sum(!is.na(out$CLI_Trajectory_FDR[out$RowType == "Daily"])), 0,
             info = "CLI_Trajectory_FDR column has *all* NA/missings for Daily Rows")
expect_true(all(dplyr::between(out$CLI_Trajectory_FDR[out$RowType == "Summary"], left = 0.0, right = 1.0)),
            info = "CLI_Trajectory_FDR columns values are all between 0 and 1 inclusive.")

##. . ILI ----
###ILI_Total_Visits
expect_true(inherits(out$ILI_Total_Visits, 'numeric'),
            info = "ILI_Total_Visits column is 'numeric' class")
expect_equal(sum(is.na(out$ILI_Total_Visits[out$RowType == "Daily"])), 0,
             info = "ILI_Total_Visits column has no NA/missings for Daily rows")
expect_equal(sum(!is.na(out$ILI_Total_Visits[out$RowType == "Summary"])), 0,
             info = "ILI_Total_Visits column has ONLY NA/missings for Summary rows")

###ILI_Visits
expect_true(inherits(out$ILI_Visits, 'numeric'),
            info = "ILI_Visits column is 'numeric' class")
expect_equal(sum(is.na(out$ILI_Visits[out$RowType == "Daily"])), 0,
             info = "ILI_Visits column has no NA/missings for Daily rows")
expect_equal(sum(!is.na(out$ILI_Visits[out$RowType == "Summary"])), 0,
             info = "ILI_Visits column has ONLY NA/missings for Summary rows")

###ILI_Percent
expect_true(inherits(out$ILI_Percent, 'numeric'),
            info = "ILI_Percent column is 'numeric' class")
expect_equal(sum(is.na(out$ILI_Percent[out$RowType == "Daily"])), 0,
             info = "ILI_Percent column has no NA/missings for Daily rows")
expect_equal(sum(!is.na(out$ILI_Percent[out$RowType == "Summary"])), 0,
             info = "ILI_Percent column has ONLY NA/missings for Summary rows")
expect_true(all(dplyr::between(out$ILI_Percent[out$RowType == "Daily"], left = 0.0, right = 100.0)),
            info = "ILI_Percent columns values are all between 0 and 100 inclusive.")
expect_true(any(out$ILI_Percent[out$RowType == "Daily"] > 1.0),
            info = "ILI_Percent columns values are scales between 0 and 100 (not 0 and 1).")

###ILI_Baseline
expect_true(inherits(out$ILI_Baseline, 'numeric'),
            info = "ILI_Baseline column is 'numeric' class")
expect_equal(sum(is.na(out$ILI_Baseline[out$RowType == "Daily" & out$Region != "Florence"])), 0,
             info = "ILI_Baseline column has no NA/missings for Daily rows outside of Florence County")
expect_equal(sum(!is.na(out$ILI_Baseline[out$RowType == "Summary" | out$Region == "Florence"])), 0,
             info = "ILI_Baseline column has ONLY NA/missings for Summary rows and Florence County")
expect_true(all(dplyr::between(out$ILI_Baseline[out$RowType == "Daily"], left = 0.0, right = 100.0)),
            info = "ILI_Baseline columns values are all between 0 and 100 inclusive.")
expect_true(any(out$ILI_Baseline[out$RowType == "Daily"] > 1.0),
            info = "ILI_Baseline columns values are scales between 0 and 100 (not 0 and 1).")

###ILI_Threshold
expect_true(inherits(out$ILI_Threshold, 'numeric'),
            info = "ILI_Threshold column is 'numeric' class")
expect_equal(sum(is.na(out$ILI_Threshold[out$RowType == "Daily" & out$Region != "Florence"])), 0,
             info = "ILI_Threshold column has no NA/missings for Daily rows outside of Florence County")
expect_equal(sum(!is.na(out$ILI_Threshold[out$RowType == "Summary" | out$Region == "Florence"])), 0,
             info = "ILI_Threshold column has ONLY NA/missings for Summary rows and Florence County")
expect_true(all(dplyr::between(out$ILI_Threshold[out$RowType == "Daily"], left = 0.0, right = 100.0)),
            info = "ILI_Threshold columns values are all between 0 and 100 inclusive.")
expect_true(any(out$ILI_Threshold[out$RowType == "Daily"] > 1.0),
            info = "ILI_Threshold columns values are scales between 0 and 100 (not 0 and 1).")


###ILI_Moving_Avg
ili_ma <- out %>%
  group_by(Data_Period, Region) %>%
  mutate(
    burn_obs = row_number(Date),
    ma_present = if_else(RowType == "Daily" & Region != "Florence" & burn_obs > 2, TRUE, FALSE)
  )

expect_true(inherits(ili_ma$ILI_Moving_Avg, 'numeric'),
            info = "ILI_Moving_Avg column is 'numeric' class")
expect_equal(sum(is.na(ili_ma$ILI_Moving_Avg[ili_ma$RowType == "Daily" & ili_ma$Region != "Florence" & ili_ma$burn_obs > 2])), 0,
             info = "ILI_Moving_Avg column has no NA/missings for Daily rows outside of Florence County and first 2 dates of period per geo-unit")
expect_equal(sum(!is.na(ili_ma$ILI_Moving_Avg[ili_ma$RowType == "Summary" | ili_ma$Region == "Florence" | ili_ma$burn_obs <= 2])), 0,
             info = "ILI_Moving_Avg column has ONLY NA/missings for Summary rows and Florence County and first 2 dates of period per geo-unit")
expect_true(all(dplyr::between(ili_ma$ILI_Moving_Avg[ili_ma$RowType == "Daily" & ili_ma$Region != "Florence" & ili_ma$burn_obs > 2], left = 0.0, right = 100.0)),
            info = "ILI_Moving_Avg columns values are all between 0 and 100 inclusive.")
expect_true(any(ili_ma$ILI_Moving_Avg[ili_ma$RowType == "Daily" & ili_ma$Region != "Florence" & ili_ma$burn_obs > 2] > 1.0),
            info = "ILI_Moving_Avg columns values are scales between 0 and 100 (not 0 and 1).")

###ILI_Status
expect_true(inherits(ili_ma$ILI_Status, 'character'),
            info = "ILI_Status column is 'character' class")
expect_equal(sum(is.na(ili_ma$ILI_Status[ili_ma$RowType == "Daily" & ili_ma$Region != "Florence" & ili_ma$burn_obs > 2])), 0,
             info = "ILI_Status column has no NA/missings for Daily rows outside of Florence County and first 2 dates of period per geo-unit")
expect_equal(sum(!is.na(ili_ma$ILI_Status[ili_ma$RowType == "Summary" | ili_ma$Region == "Florence" | ili_ma$burn_obs <= 2])), 0,
             info = "ILI_Status column has ONLY NA/missings for Summary rows and Florence County and first 2 dates of period per geo-unit")
expect_true(all(dplyr::between(ili_ma$ILI_Status[ili_ma$RowType == "Daily" & ili_ma$Region != "Florence" & ili_ma$burn_obs > 2], left = 0.0, right = 100.0)),
            info = "ILI_Status columns values are all between 0 and 100 inclusive.")
expect_true(any(ili_ma$ILI_Status[ili_ma$RowType == "Daily" & ili_ma$Region != "Florence" & ili_ma$burn_obs > 2] > 1.0),
            info = "ILI_Status columns values are scales between 0 and 100 (not 0 and 1).")


##. . Testing Volume Targets----
###Testing_Case
expect_true(inherits(out$Testing_Case, 'numeric'),
            info = "Testing_Case column is 'numeric' class")
expect_equal(sum(is.na(out$Testing_Case[out$RowType == "Summary"])), 0,
             info = "Testing_Case column has no NA/missings for Summary rows")
expect_equal(sum(!is.na(out$Testing_Case[out$RowType == "Daily"])), 0,
             info = "Testing_Case column has ONLY NA/missings for Daily rows")

###Testing_ARI
expect_true(inherits(out$Testing_ARI, 'numeric'),
            info = "Testing_ARI column is 'numeric' class")
expect_equal(sum(is.na(out$Testing_ARI[out$RowType == "Summary"])), 0,
             info = "Testing_ARI column has no NA/missings for Summary rows")
expect_equal(sum(!is.na(out$Testing_ARI[out$RowType == "Daily"])), 0,
             info = "Testing_ARI column has ONLY NA/missings for Daily rows")

###Testing_Case_Gap
expect_true(inherits(out$Testing_Case_Gap, 'numeric'),
            info = "Testing_Case_Gap column is 'numeric' class")
expect_equal(sum(is.na(out$Testing_Case_Gap[out$RowType == "Summary"])), 0,
             info = "Testing_Case_Gap column has no NA/missings for Summary rows")
expect_equal(sum(!is.na(out$Testing_Case_Gap[out$RowType == "Daily"])), 0,
             info = "Testing_Case_Gap column has ONLY NA/missings for Daily rows")

###Testing_Target_0.2
expect_true(inherits(out$Testing_Target_0.2, 'numeric'),
            info = "Testing_Target_0.2 column is 'numeric' class")
expect_equal(sum(is.na(out$Testing_Target_0.2[out$RowType == "Summary"])), 0,
             info = "Testing_Target_0.2 column has no NA/missings for Summary rows")
expect_equal(sum(!is.na(out$Testing_Target_0.2[out$RowType == "Daily"])), 0,
             info = "Testing_Target_0.2 column has ONLY NA/missings for Daily rows")

###Testing_Target_0.4
expect_true(inherits(out$Testing_Target_0.4, 'numeric'),
            info = "Testing_Target_0.4 column is 'numeric' class")
expect_equal(sum(is.na(out$Testing_Target_0.4[out$RowType == "Summary"])), 0,
             info = "Testing_Target_0.4 column has no NA/missings for Summary rows")
expect_equal(sum(!is.na(out$Testing_Target_0.4[out$RowType == "Daily"])), 0,
             info = "Testing_Target_0.4 column has ONLY NA/missings for Daily rows")

###Testing_Target_0.6
expect_true(inherits(out$Testing_Target_0.6, 'numeric'),
            info = "Testing_Target_0.6 column is 'numeric' class")
expect_equal(sum(is.na(out$Testing_Target_0.6[out$RowType == "Summary"])), 0,
             info = "Testing_Target_0.6 column has no NA/missings for Summary rows")
expect_equal(sum(!is.na(out$Testing_Target_0.6[out$RowType == "Daily"])), 0,
             info = "Testing_Target_0.6 column has ONLY NA/missings for Daily rows")

###Testing_Target_0.8
expect_true(inherits(out$Testing_Target_0.8, 'numeric'),
            info = "Testing_Target_0.8 column is 'numeric' class")
expect_equal(sum(is.na(out$Testing_Target_0.8[out$RowType == "Summary"])), 0,
             info = "Testing_Target_0.8 column has no NA/missings for Summary rows")
expect_equal(sum(!is.na(out$Testing_Target_0.8[out$RowType == "Daily"])), 0,
             info = "Testing_Target_0.8 column has ONLY NA/missings for Daily rows")

###Testing_Target_1
expect_true(inherits(out$Testing_Target_1, 'numeric'),
            info = "Testing_Target_1 column is 'numeric' class")
expect_equal(sum(is.na(out$Testing_Target_1[out$RowType == "Summary"])), 0,
             info = "Testing_Target_1 column has no NA/missings for Summary rows")
expect_equal(sum(!is.na(out$Testing_Target_1[out$RowType == "Daily"])), 0,
             info = "Testing_Target_1 column has ONLY NA/missings for Daily rows")
