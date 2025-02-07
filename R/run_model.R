#' Runs the multiclass script - can be used for testing multiple iterations of the model
#' 
#' @param cfg a list containing model configuration arguments
#' @param model_input a model_input object containing lists for train and test sets
#' @return a one row tibble containing metrics of model run
#' 
#' @export
run_model <- function(cfg, 
                      model_input) {
  
  model <- forecast_model(cfg, model_input, forecast_mode=FALSE)
  
  forecast_list <- make_forecast_list(cfg, model$forecast, forecast_mode = FALSE)
  
  metrics = forecast_metrics(forecast_list)
  
  z <- list(metrics = metrics,
            predictions = forecast_list)
  return(z)
}
