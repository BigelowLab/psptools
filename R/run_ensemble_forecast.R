#' Run the forecast model n times and generate a real-time ensemble forecast
#' 
#' @param cfg name of configuration file for model parameters
#' @param input_data a tibble of raw input data
#' @param n the number of times to run the ensemble
#' @param model_mode character setting whether to train new models or load pre-trained ones
#' @param model_dir character path to directory containing pre-trained models
#' @param past_predictions tibble of predictions that have already been made this season
#' @return a list containing an ensemble forecast and the predictions from each of the ensemble members
#' 
#' @export
run_ensemble_forecast <- function(cfg, 
                                  input_data,
                                  n = 10, 
                                  model_mode = c("train", "load")[1],
                                  model_dir = NULL,
                                  past_predictions) {
  
  model_input <- transform_data(cfg, input_data, forecast_mode=TRUE)
  
  runs <- dplyr::tibble(version = character(),
                        location = character(),
                        date = Sys.Date(),
                        species = character(),
                        class_bins = character(),
                        forecast_start_date = Sys.Date(),
                        forecast_end_date = Sys.Date(),
                        predicted_class = integer())
  switch(model_mode,
         "train" = {
           for (i in 1:n) {
             run <- train_and_predict(cfg, model_input, forecast_mode=TRUE)
             
             runs <- dplyr::bind_rows(run$predictions, runs)
           }
         },
         "load" = {
           models <- list.files(model_dir)
           
           for (m in models) {
             run <- load_and_predict(cfg, model_input, model_dir = model_dir, model_name = m)
             
             runs <- dplyr::bind_rows(run, runs)
           }
         })
  
  ensemble_forecast <- make_ensemble_forecast(cfg, runs, forecast_mode = TRUE) |>
    dplyr::mutate(version = cfg$configuration,
                  ensemble_n = n,
                  .before="location")
  
  runs <- runs |>
    dplyr::mutate(f_id = paste(.data$location, .data$date, .data$species, sep="_"))
  
  if (!is.null(past_predictions)) {
    ensemble_forecast <- dplyr::filter(ensemble_forecast, format(date, format = "%Y") == format(Sys.Date(), format = "%Y") & !.data$f_id %in% past_predictions$f_id)
    runs <- dplyr::filter(runs, format(date, format = "%Y") == format(Sys.Date(), format = "%Y") & !.data$f_id %in% past_predictions$f_id)
  }
  
  ensemble_forecast_list <- list(ensemble_forecast = ensemble_forecast,
                                 ensemble_runs = runs)
  
  return(ensemble_forecast_list)
}
