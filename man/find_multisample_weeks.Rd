% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/find_multisample_weeks.R
\name{find_multisample_weeks}
\alias{find_multisample_weeks}
\title{Finds cases where a monitoring station has been sampled more than once in a week}
\usage{
find_multisample_weeks(
  data,
  mode = c("first", "last", "min", "max", "none")[1]
)
}
\arguments{
\item{data}{tibble of psp monitoring data}

\item{mode}{character specifying which sample to keep; mode "none" will not remove any samples}
}
\value{
a tibble of psp monitoring data with only one sample per station per week
}
\description{
Finds cases where a monitoring station has been sampled more than once in a week
}
