% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_pull_fcns.R
\name{pull_testing}
\alias{pull_testing}
\title{Pull data from WEDSS for Testing Metrics}
\usage{
pull_testing(lab_query, stg_query, conn, end_date = NULL)
}
\arguments{
\item{lab_query}{SQL query string for data from Lab table}

\item{stg_query}{SQL query string for data from Staging table}

\item{conn}{connection to database (see \code{\link[RODBC]{odbcConnect}})
or \code{\link[odbc]{OdbcConnection}}}

\item{end_date}{(default = NULL) If specified it is the end date of the
time series that you wish to analyze. It should be something
coercible to Date format with as.Date. If you want all data
leave it blank.}
}
\value{
a data.frame
}
\description{
Pull data from WEDSS for Testing Metrics
}
\examples{
\dontrun{
  #write me an example please.
}
}
