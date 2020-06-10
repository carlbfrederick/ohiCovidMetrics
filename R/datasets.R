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
#' @source
#' \itemize{
#'   \item FIPS County codes taken from ACS downloads via \code{\link[tidycensus]{get_acs}}
#'   \item 2018 Population Estimates from WISH as saved in our
#'         \href{https://share.health.wisconsin.gov/ph/inf/drrb/Meetings/Information%20for%20sub-state%20analyses/WISH.xlsx}{SharePoint Site}.
#'         Accessed on 10 June 2020.
#'   \item DPH Regions hand coded from \url{https://www.dhs.wisconsin.gov/lh-depts/counties.htm}
#'         on 16 April 2020.
#'   \item HERC Regions hand coded from \url{https://www.dhs.wisconsin.gov/preparedness/healthcare/index.htm}
#' }
"county_data"
