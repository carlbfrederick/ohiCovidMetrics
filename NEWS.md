# ohiCovidMetrics 0.8.5.9

Bugfix to more robust way of truncating datetimes into dates.

## Bug fixes

- For ILI and CLI metrics, switched from as.Date(POSIXt, tz = "America/Chicago")
  to as.Date(trunc(POSIXt, units = "days"))


# ohiCovidMetrics 0.8.5

The major change in this update is on how we treat dates for confirmed cases.
Based on stakeholder feedback, we now present daily confirmed case numbers with
the same date that they are posted with in Historical Data Table and other 
displays.

## Enhancements

- Changed Burden and Composite labels from 'Critical' to 'Very high' and made
  capitalization consistent across all labels.
- Daily confirmed cases will now match dates on the rest of the DHS reporting
  websites.
- Streamlined data check tests so that the bulk of the tests in the appended
  files are not duplicates.

## Breaking Changes

- Merged files now contain 15 rows since we have 2 14-day windows that are off
  by a day.  This caused many headaches. Also, summary rows are now dated as the
  maximum date in the data period to ensure there is only one summary row per 
  geo-unit per data period.

## Bug fixes

- Many, vastly improved tests for data and calculating functions

# ohiCovidMetrics 0.7.9.9

This version corrects a critical bug in the calculation of the trajectory 
class.

## Enhancements

- [none so far]

## Breaking Changes

- [none so far]

## Bug fixes

- Fixed bug in `class_trajectory()` that was expecting a ratio (old) instead
  of a percent change (current).
- Fixed issue in ILI where some regions were not merging thresholds and 
  baselines correctly.
- Changed Testing Composite outcome labels

# ohiCovidMetrics 0.7.9

This version is intended to be the last clean up version before we go live 
for the 'Tier 2' local dashboard.

## Enhancements

- added some basic checks for merged metric file with basic reporting via
  a warning for failed tests.
- added function to aggregate new metrics onto source file. This function
  also re-calculates the moving average variables so that there are not 
  breaks at the beginning of each update period.
- changed rounding for case trajectory/burden so that they don't look like
  true counts

## Breaking Changes

- Have marked the Total Emergency Department visit functions as deprecated.  
  These functions will be deleted before the 'Tier 2' local dashboard version
  of the file.

## Bug fixes

- Cleaned up fix to end dates for testing sub-functions
- Fixed a couple of bugs in the ILI functions for calculating moving averages 
  and merge fail for some HERC regions due to name variants.

# ohiCovidMetrics 0.7.0

This large jump represents work such that all metrics topics are functional,
but still need to confirm that the output is identical for 1.0.0 version

## Enhancements

- added function to pull confirmed case metric directly from WEDSS
- fixed 'tests' in the Vignette introduced by output data format changes
- added functions to compute metrics for
    - Hospital Capacity 
    - CLI (Covid Like Illnesses)
    - ILI (Influenze Like Illnesses)
    - Testing
    - Total Emergency Department Visits
- Added function to merge metric output together to streamline Tableau build
- All functions now output summary and daily metrics for Tableau dashboard
- Updated documentation so that all output variables of the `process_*()`
  functions are described.

## Breaking Changes

- moved the `clean_*()` functions into the `pull_*()` functions
- moved the `shape_*_data()` functions into the `process_*()` functions
  to simplify user API
    - However, I have temporarily added the `OLD_process_confirmed_cases()`
      function to the package to ease transition to exclusive use of this
      package to produce the metrics.

## Bug fixes

- Fixed a bug in `rolling_weeks()` in which periods that were a multiple 
  of 7 days the earliest week was 7 NAs instead of a count.  Now there 
  can only be 0-6 NAs to start the week vector.
- Fixed bug in `pull_histTable()` where some fields were incorrectly 
  imported as character instead of integer

# ohiCovidMetrics 0.1.2

This patch fixes a small bug in the process_case_metrics() function that
wrote over the trajectory scores with trajectory classifications.

Also:

- Fixes bug where some numeric columns were downloaded as factors in 
  `pull_histTable()`
- Change license to GPL 3

# ohiCovidMetrics 0.1.1

Minor patch to make output from `process_case_metrics()` more suitable
for Tableau Ingestion.  

# ohiCovidMetrics 0.1.0

This is a minimally viable product that can download the Historical Data 
Table from the DHS website and produce the Confirmed Case Metric file for
export to Tableau for the Dashboard. It was tested against the file that
Jeff produced for the DHS dashboard for the period ending 17 June 2020 and
matches exactly (see the Producing Confirmed Case Metrics Vignette).

The only data source supported is the Historical Data Table that is posted 
on the DHS website. The calculations are the ones described in the OHI
Confirmed Case Metric Reproducibility document. *Note: they are the ones
described as 'defined as' not the ones that are 'proposed'.*

# ohiCovidMetrics 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
