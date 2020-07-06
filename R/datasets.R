#' County Level Data to Facilitate Metric Calculation and Aggregation
#'
#' These data contain county FIPS codes, population estimates and region
#' mappings to facilitate sub-state calculations and aggregations across
#' all metrics.
#'
#' @format A data.frame with 72 observations (one per WI county) and 3 variables:
#' \describe{
#'   \item{fips}{5 digit FIPS county code}
#'   \item{county}{County name}
#'   \item{pop_2018}{WISH 2018 population figures}
#'   \item{dph_region}{DPH Region}
#'   \item{herc_region}{HERC Region}
#' }
#'
#' @source FIPS County codes taken from ACS downloads via \code{\link[tidycensus]{get_acs}}
#' @source 2018 Population Estimates from WISH
#' @source DPH Regions hand coded from \url{https://www.dhs.wisconsin.gov/lh-depts/counties.htm}
#' @source HERC Regions hand coded from \url{https://www.dhs.wisconsin.gov/preparedness/healthcare/index.htm}
#'
"county_data"

#' Fake hospitalization data inspired by the source data for hospitalization metrics
#'
#' These data are fake
#'
#' @format A data.frame with ??? observations ??? variables:
#' \describe{
#'   \item{}{}
#' }
#'
#'
"hosp_sim"
