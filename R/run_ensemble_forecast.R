#' Run the forecast model n times and generate an ensemble forecast
#' 
#' @param cfg name of configuration file for model parameters
#' @param input_data a tibble of raw input data
#' @param n the number of times to run the ensemble
#' @param f_mode logical whether to call functions in forecast mode
#' @param past_predictions tibble of predictions that have already been made this season
#' @return a list containing an ensemble forecast and the predictions from each of the ensemble members
#' 
#' @export
run_ensemble_forecast <- function(cfg, 
                                  input_data,
                                  n = 10, 
                                  f_mode=TRUE,
                                  past_predictions) {
  
  single_run <- function(cfg, model_input, f_mode) {
    
    model <- forecast_model(cfg, model_input, forecast_mode=TRUE)
    forecast_list <- make_forecast_list(cfg, model$forecast, forecast_mode=TRUE)
    
    return(forecast_list)
  }
  
  model_input <- transform_data(cfg, input_data, forecast_mode=TRUE)
  
  runs <- dplyr::tibble(version = character(),
                        location = character(),
                        date = Sys.Date(),
                        name = character(),
                        lat = numeric(),
                        lon = numeric(),
                        species = character(),
                        class_bins = character(),
                        forecast_start_date = Sys.Date(),
                        forecast_end_date = Sys.Date(),
                        predicted_class = integer())
  
  for (i in 1:n) {
    cat("run: ", i, "\n")
    
    run <- single_run(cfg, model_input, f_mode=TRUE)
    
    runs <- run |> 
      dplyr::bind_rows(runs)
  }
  
  #site_forecast <- function(tbl, key) {
  #  forecast_list <- dplyr::tibble(version = cfg$configuration,
  #                                 ensemble_n = n,
  #                                 location = tbl$location[1],
  #                                 date = tbl$date[1],
  #                                 class_bins = "0,10,30,80",
  #                                 forecast_start_date = as.Date(as.numeric(date), origin = as.Date("1970-01-01")) + cfg$image_list$forecast_steps*7-3,
  #                                 forecast_end_date = as.Date(as.numeric(date), origin = as.Date("1970-01-01")) + cfg$image_list$forecast_steps*7+3,
  #                                 p_0 = round(mean(tbl$prob_0)),
  #                                 p_1 = round(mean(tbl$prob_1)),
  #                                 p_2 = round(mean(tbl$prob_2)),
  #                                 p_3 = round(mean(tbl$prob_3)),
  #                                 p3_sd = sd(tbl$prob_3),
  #                                 p_3_min = min(tbl$prob_3),
  #                                 p_3_max = max(tbl$prob_3)) |> 
  #    dplyr::mutate(predicted_class = which.max(c(.data$p_0, .data$p_1, .data$p_2, .data$p_3)) - 1)
  #}
  #
  #ensemble_forecast <- runs |> 
  #  dplyr::group_by(.data$location, .data$date) |> 
  #  dplyr::group_map(site_forecast, .keep=TRUE) |> 
  #  dplyr::bind_rows() |>
  #  dplyr::mutate(f_id = paste(.data$location, .data$date, sep="_"))
  
  ensemble_forecast <- make_ensemble_forecast(cfg, runs, forecast_mode = TRUE) |>
    dplyr::mutate(version = cfg$configuration,
                  ensemble_n = n,
                  .before="location")
  
  runs <- runs |>
    dplyr::mutate(f_id = paste(.data$location, .data$date, sep="_"))
  
  if (!is.null(past_predictions)) {
    ensemble_forecast <- dplyr::filter(ensemble_forecast, format(date, format = "%Y") == format(Sys.Date(), format = "%Y") & !.data$f_id %in% past_predictions$f_id)
    runs <- dplyr::filter(runs, format(date, format = "%Y") == format(Sys.Date(), format = "%Y") & !.data$f_id %in% past_predictions$f_id)
  }
  
  ensemble_forecast_list <- list(ensemble_forecast = ensemble_forecast,
                                 ensemble_runs = runs)
  
  return(ensemble_forecast_list)
}
