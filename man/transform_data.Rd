% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transform_data.R
\name{transform_data}
\alias{transform_data}
\title{Takes raw data and transforms it to be ready for model input
wraps `make_image_list()` and `pool_images_and_labels()` into one function}
\usage{
transform_data(cfg, input_data, forecast_mode = FALSE)
}
\arguments{
\item{cfg}{model configuration file}

\item{input_data}{a tibble of input samples where each row is a sample and each column is a feature}

\item{forecast_mode}{logical, if true, creates new dummy rows for each station to generate a forecast}
}
\value{
two lists for transformed forecast model input training and testing data as `pool_images_and_labels()` output.
}
\description{
Takes raw data and transforms it to be ready for model input
wraps `make_image_list()` and `pool_images_and_labels()` into one function
}
