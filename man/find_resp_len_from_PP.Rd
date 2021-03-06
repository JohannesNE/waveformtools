% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gam_tools.R
\name{find_resp_len_from_PP}
\alias{find_resp_len_from_PP}
\title{Find resp length from Pulse Pressure data using GAM}
\usage{
find_resp_len_from_PP(
  data,
  PP_col = "PP",
  time_col = "time",
  search_interval = c(1.5, 7)
)
}
\arguments{
\item{data}{Data frame of timings and pulse pressures of beats}

\item{PP_col}{Column with pulse pressure values}

\item{time_col}{Columns with times}

\item{search_interval}{Respiratory cycle length intervals to search}
}
\value{
The respiratory length that minimizes the GAM REML score
}
\description{
Find resp length from Pulse Pressure data using GAM
}
