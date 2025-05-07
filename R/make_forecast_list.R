#' Formats the output from the PSP forecast model in standard form to be added to forecast database
#' 
#' @param cfg the configuration object used to build and train the model
#' @param model a list of predictions made by the trained neural network and their metadata
#' @param forecast_mode logical if testing a model with unknown test targets (aka making a forecast with a trained model)
#' @return a list of forecast classifications with their metadata
#' 
#' @export
make_forecast_list <- function(cfg, 
                               model, 
                               forecast_mode=FALSE) {

  if (forecast_mode == TRUE) {
    forecast_list <- dplyr::tibble(version = cfg$configuration,
                                   location = model$locations,
                                   date = as.Date(as.numeric(model$dates), origin = as.Date("1970-01-01")),
                                   class_bins = "0,10,30,80",
                                   forecast_start_date = as.Date(as.numeric(model$dates), origin = as.Date("1970-01-01")) + cfg$image_list$forecast_steps*7-3,
                                   forecast_end_date = as.Date(as.numeric(model$dates), origin = as.Date("1970-01-01")) + cfg$image_list$forecast_steps*7+3)
    
  } else {
    forecast_list <- dplyr::tibble(version = cfg$configuration,
                                   location = model$locations,
                                   date = as.Date(as.numeric(model$dates), origin = as.Date("1970-01-01")),
                                   species = model$species,
                                   class_bins = "0,10,30,80",
                                   forecast_start_date = as.Date(as.numeric(model$dates), origin = as.Date("1970-01-01")) + cfg$image_list$forecast_steps*7-3,
                                   forecast_end_date = as.Date(as.numeric(model$dates), origin = as.Date("1970-01-01")) + cfg$image_list$forecast_steps*7+3,
                                   actual_class = model$test_classifications,
                                   actual_toxicity = model$test_toxicity)
  }
  
  if (length(cfg$image_list$tox_levels) == 2) {
    forecast_list <- forecast_list
  } else if (length(cfg$image_list$tox_levels) == 3) {
    forecast_list <- forecast_list |> 
      dplyr::mutate(prob_0 = model$predicted_probs[,1]*100,
                    prob_1 = model$predicted_probs[,2]*100,
                    prob_2 = model$predicted_probs[,3]*100) |> 
      dplyr::rowwise() |> 
      dplyr::mutate(predicted_class = which.max(c(.data$prob_0, .data$prob_1, .data$prob_2)) - 1)
  } else if (length(cfg$image_list$tox_levels) == 4) {
    forecast_list <- forecast_list |> 
      dplyr::mutate(prob_0 = model$predicted_probs[,1]*100,
                    prob_1 = model$predicted_probs[,2]*100,
                    prob_2 = model$predicted_probs[,3]*100,
                    prob_3 = model$predicted_probs[,4]*100) |> 
      dplyr::rowwise() |> 
      dplyr::mutate(predicted_class = which.max(c(.data$prob_0, .data$prob_1, .data$prob_2, .data$prob_3)) - 1)
  }
  

  return(forecast_list)
}
