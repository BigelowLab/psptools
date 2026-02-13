#' Runs a model configuration n times and produces metrics/plots of results
#' 
#' @param cfg a list containing configuration params
#' @param input_data a tibble of raw input data (psp measurements, environmental data)
#' @param n_runs integer of times to run the model for the test
#' @param write_results logical if true then results/metrics get appended to results files
#' @param model_fun character string defining which predictive model function to use
#' @return a list containing predictions, metrics and plots
#' 
#' @export
run_ensemble_test <- function(cfg, 
                              input_data, 
                              n_runs = 3, 
                              write_results=FALSE,
                              model_fun = "forecast_model") {
  
  run_metrics <- dplyr::tibble()
  all_predictions <- dplyr::tibble()
  
  model_input <- transform_data(cfg, input_data, forecast_mode=FALSE)
  
  for (i in 1:n_runs) {
    
    test <- train_and_predict(cfg, model_input, forecast_mode=FALSE, model_fun=model_fun)
    
    run_metrics <- run_metrics |> 
      dplyr::bind_rows(test$metrics)
    
    all_predictions <- all_predictions |> 
      dplyr::bind_rows(test$predictions)
  }
  
  run_metrics <- dplyr::arrange(run_metrics, dplyr::desc(.data$tp))
  
  p3_v_tox <- ggplot2::ggplot(data=all_predictions, ggplot2::aes(x = .data$prob_3, y = .data$actual_toxicity)) +
    ggplot2::geom_point(ggplot2::aes(alpha = 0.4)) +
    ggplot2::geom_hline(yintercept = 80) +
    ggplot2::ggtitle("Probability of Class 3 vs Actual Toxicity - All Predictions (n runs)")
  
  p3_v_class <- ggplot2::ggplot(data=all_predictions, ggplot2::aes(x = .data$prob_3, y = .data$actual_class)) +
    ggplot2::geom_point(ggplot2::aes(alpha = 0.4)) +
    ggplot2::ggtitle("Probability of Class 3 vs Actual Class - All Predictions (n runs)")
  
  ensemble_forecast <- make_ensemble_forecast(cfg, all_predictions, forecast_mode = FALSE)
  
  ensemble_predictions <- dplyr::arrange(ensemble_forecast, dplyr::desc(.data$p3_sd)) |> 
    dplyr::mutate(p_diff = .data$predicted_class - .data$actual_class)
  
  confusion_matrix <- make_confusion_matrix(cfg, ensemble_forecast)
  
  p3_v_tox_ens <- ggplot2::ggplot(data=ensemble_forecast, ggplot2::aes(x = .data$p_3, y = .data$actual_toxicity)) +
    ggplot2::geom_point(alpha = 0.4) +
    ggplot2::geom_hline(yintercept = 80, linetype="dashed") +
    ggplot2::ggtitle("Probability of Class 3 vs Actual Toxicity - Ensemble Predictions") +
    ggplot2::labs(x = "Probability of closure-level toxicity",
                  y = "Measured PSP toxicity")
  
  
  p3_v_class_ens <- ggplot2::ggplot(data=ensemble_forecast, ggplot2::aes(x = .data$p_3, y = .data$actual_class)) +
    ggplot2::geom_hline(yintercept = 80, linetype="dashed") +
    ggplot2::ggtitle("Probability of Class 3 vs Actual Class - Ensemble Predictions") +
    ggplot2::labs(x = "Probability of closure-level toxicity",
                  y = "Measured PSP classification")
  
  if (write_results) {
    readr::write_csv(run_metrics, "individual_test_results.csv.gz", append=TRUE)
    readr::write_csv(ensemble_forecast$metrics, "ensemble_test_results.csv.gz", append=TRUE)
  }
  
  ensemble_metrics <- forecast_metrics(ensemble_forecast, positive_col = "p_3")
  
  z <- list(all_predictions = all_predictions,
            ensemble_predictions = ensemble_predictions,
            run_metrics = run_metrics,
            ensemble_metrics = ensemble_metrics,
            p3_v_tox = p3_v_tox,
            p3_v_class = p3_v_class,
            p3_v_tox_ens = p3_v_tox_ens,
            p3_v_class_ens = p3_v_class_ens,
            ensemble_cm = confusion_matrix)
  
  return(z)
}