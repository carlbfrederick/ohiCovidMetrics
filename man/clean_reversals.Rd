% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{clean_reversals}
\alias{clean_reversals}
\title{Clean up reversals in daily count time series}
\usage{
clean_reversals(daily_time_series, verbose = TRUE)
}
\arguments{
\item{daily_time_series}{a time series of count data in chronological order}

\item{verbose}{a logical where FALSE means messages are suppressed. Default = TRUE}
}
\value{
a vector of counts of the same length as the original
}
\description{
This function was written to overcome the fact that the Covid Historical
Data Table has negative counts on a few days in the time series because
of changes in the underlying data. The function fixes that by subtracting
the negative count from the prior positive day's count.
}
\details{
If there are two or more negative counts in a row, the function subtracts
the sum of the negative count from the most recent day with a positive
count. It will issue a warning if this cleaning algorithm results in a
daily time series that still has negative counts.

This algorithm ensures that the total cumulative count after the cleaning
is equal to what it was originally.
}
\examples{
\dontrun{
  suppressPackageStartupMessages(library(dplyr))

  pull_histTable() \%>\%
    group_by(fips) \%>\%
    mutate(
      tests_daily_clean = clean_reversals(tests_daily)
    )
}
}
