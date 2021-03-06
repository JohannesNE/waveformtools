% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/signal_processing.R
\name{filter_signal}
\alias{filter_signal}
\title{Filter signal, defaluts to 2nd order Butterworth low pass filter}
\usage{
filter_signal(
  signal,
  cutoff_frequency,
  signal_col = 2,
  sample_rate = NULL,
  type = "low",
  trim_ends = 0,
  postfix = "_filt",
  filter_func = signal::butter,
  filter_order = 2,
  ...
)
}
\arguments{
\item{signal}{Vector or signal dataframe with time column}

\item{cutoff_frequency}{Critical frequency of filter}

\item{signal_col}{column with signal to filter}

\item{sample_rate}{Sample rate of signal. If NULL, this is calculated from the time column.}

\item{type}{passed to \code{\link[signal:butter]{signal::butter()}}. One of "low", "high", "stop", "pass"}

\item{trim_ends}{Seconds to trim off each end after filtering. To remove tails towards 0}

\item{postfix}{Postfix to the new column}

\item{filter_func}{Signal function to use for filtering
e.g. signal::cheby2}

\item{filter_order}{Passed to \code{filter_func} as \code{n}}

\item{...}{Passed to \code{filter_func}}
}
\description{
Filter signal, defaluts to 2nd order Butterworth low pass filter
}
