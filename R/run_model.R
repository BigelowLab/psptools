#' Trains a forecast model and makes predictions
#' 
#' @param cfg a list containing model configuration arguments
#' @param model_input a model_input object containing lists for train and test sets
#' @param forecast_mode logical to define if testing on historical data or predicting in real-time
#' @return a tibble of predictions in forecast_list format along with metrics if testing
#' @export
train_and_predict <- function(cfg, 
                              model_input,
                              forecast_mode) {
  
  model <- forecast_model(cfg, model_input, forecast_mode=forecast_mode)
  
  forecast_list <- make_forecast_list(cfg, model$forecast, forecast_mode=forecast_mode)
  
  z <- list(predictions = forecast_list)
  
  if (!forecast_mode) {
    metrics = forecast_metrics(forecast_list)
    z$metrics = metrics
  }
  return(z)
}



#' Loads a pre-trained forecast model and makes predictions
#'
#' @param cfg a list containing model configuration arguments
#' @param model_input a model_input object containing lists for train and test sets
#' @param model_dir character path to directory containing pre-trained models
#' @param model_name character name of model to load
#' @return tibble of predictions in forecast_list format
#' @export
load_and_predict <- function(cfg, model_input, model_dir, model_name) {
  model <- keras::load_model_hdf5(file.path(model_dir, model_name))
  
  forecast <- list(year = cfg$train_test$test,
                   dates = model_input$test$dates,
                   locations = model_input$test$locations,
                   species = model_input$test$species,
                   predicted_probs = predict(model, model_input$test$image))
  
  forecast_list <- make_forecast_list(cfg, forecast, forecast_mode=TRUE)
  
  return(forecast_list)
}