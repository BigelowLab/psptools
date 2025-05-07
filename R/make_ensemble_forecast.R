#' Transforms predictions from ensemble model test runs and combines them into an ensemble forecast with metrics
#' 
#' @param cfg a configuration list
#' @param predictions a table of predictions where each site-date has multiple entries
#' @return a list containing a table of predictions where each site-date has one prediction and metrics
#' 
#' @export
make_ensemble_forecast <- function(cfg, predictions) {
  
  site_forecast <- function(tbl, key) {
    forecast_list <- dplyr::tibble(location = tbl$location[1],
                                   date = tbl$date[1],
                                   p_0 = round(mean(tbl$prob_0)),
                                   p_1 = round(mean(tbl$prob_1)),
                                   p_2 = round(mean(tbl$prob_2)),
                                   p_3 = round(mean(tbl$prob_3)),
                                   p3_variance = sd(tbl$prob_3),
                                   p_3_min = min(tbl$prob_3),
                                   p_3_max = max(tbl$prob_3),
                                   actual_class = tbl$actual_class[1],
                                   actual_toxicity = tbl$actual_toxicity[1]) |> 
      dplyr::mutate(predicted_class = which.max(c(.data$p_0, .data$p_1, .data$p_2, .data$p_3)) - 1)
  }
  
  ensemble_forecast <- predictions |> 
    dplyr::group_by(.data$location, .data$date, .data$species) |> 
    dplyr::group_map(site_forecast, .keep=TRUE) |> 
    dplyr::bind_rows() |> 
    dplyr::arrange(.data$date, .data$location)
  
  ensemble_metrics <- forecast_metrics(ensemble_forecast)
  
  z <- list(metrics = ensemble_metrics,
            predictions = ensemble_forecast)
}