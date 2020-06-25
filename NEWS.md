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
