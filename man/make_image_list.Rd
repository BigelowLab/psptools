% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_image_list.R
\name{make_image_list}
\alias{make_image_list}
\title{Takes raw input data, filters for image criteria and creates images with dimensions (n_steps + forecast steps) x length(toxins + environmentals)}
\usage{
make_image_list(
  raw_data,
  tox_levels,
  forecast_steps,
  n_steps,
  minimum_gap,
  maximum_gap,
  toxins,
  environmentals
)
}
\arguments{
\item{raw_data}{database with toxin measurements with their date sampled, location, shellfish species and additional environmental data}

\item{tox_levels}{toxin level categories used for classifying total toxicity}

\item{forecast_steps}{the number of weeks ahead of the image the forecast is made for}

\item{n_steps}{the number of weeks of samples in an image}

\item{minimum_gap}{the smallest gap between samples allowed into an image}

\item{maximum_gap}{the largest gap between samples allowed into an image}

\item{toxins}{list of individual paralytic toxin names (12) for toxin columns}

\item{environmentals}{environmental variables}
}
\value{
each list is an image along with its associated data (location_id, date, etc.)
\itemize{
\item{status logical if the image passes the image gap criteria (gap >= minimum_gap & gap <= maximum_gap)}
\item{year the year the image is from}
\item{location_id the sampling station id}
\item{toxixty the total toxicity used to regress on instead of classify a binned toxicity}
\item{classification the classification (0:num_classes) of the final row in the image}
\item{date the date of the final row in the image (the forecast is for forecast_steps ahead of this date)}
\item{image a 2 dimensional array with the dimensions (n_steps + forecast steps) x length(toxins + environmentals)}
}
}
\description{
Takes raw input data, filters for image criteria and creates images with dimensions (n_steps + forecast steps) x length(toxins + environmentals)
}
