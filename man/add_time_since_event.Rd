% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/annotate_waveforms.R
\name{add_time_since_event}
\alias{add_time_since_event}
\title{Add indexing of signal time in relation to some recurring event.}
\usage{
add_time_since_event(
  data,
  time_event,
  time_col = 1,
  prefix = "ann",
  incl_prev_lengths = FALSE
)
}
\arguments{
\item{data}{A data frame with at least a time column}

\item{time_event}{A vector of time stamps for some annotation}

\item{time_col}{Index or name of time column}

\item{prefix}{Prefix to new columns}

\item{incl_prev_lengths}{Include length of previous 3 cycles as variables}
}
\description{
Index is the time since the most recent occurrence of some event e.g. QRS complex
}
