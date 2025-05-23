% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_ensemble_forecast.R
\name{run_ensemble_forecast}
\alias{run_ensemble_forecast}
\title{Run the forecast model n times and generate a real-time ensemble forecast}
\usage{
run_ensemble_forecast(
  cfg,
  input_data,
  n = 10,
  model_mode = c("train", "load")[1],
  model_dir = NULL,
  past_predictions
)
}
\arguments{
\item{cfg}{name of configuration file for model parameters}

\item{input_data}{a tibble of raw input data}

\item{n}{the number of times to run the ensemble}

\item{model_mode}{character setting whether to train new models or load pre-trained ones}

\item{model_dir}{character path to directory containing pre-trained models}

\item{past_predictions}{tibble of predictions that have already been made this season}
}
\value{
a list containing an ensemble forecast and the predictions from each of the ensemble members
}
\description{
Run the forecast model n times and generate a real-time ensemble forecast
}
