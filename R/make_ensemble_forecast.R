#' Transforms predictions multiple model runs and combines them into an ensemble forecast
#' 
#' @param cfg a configuration list
#' @param predictions a table of predictions where each site-date has multiple entries
#' @param forecast_mode logical if running an ensemble of real-time predictions or testing a hindcast
#' @return a list containing a table of predictions where each site-date has one prediction and metrics
#' 
#' @export
make_ensemble_forecast <- function(cfg, predictions, forecast_mode) {
  
  site_forecast <- function(tbl, key) {
    forecast_list <- dplyr::tibble(location = key$location[1],
                                   date = key$date[1],
                                   species = key$species[1],
                                   p_0 = round(mean(tbl$prob_0)),
                                   p_1 = round(mean(tbl$prob_1)),
                                   p_2 = round(mean(tbl$prob_2)),
                                   p_3 = round(mean(tbl$prob_3)),
                                   p3_sd = sd(tbl$prob_3),
                                   p_3_min = min(tbl$prob_3),
                                   p_3_max = max(tbl$prob_3)) |> 
      dplyr::mutate(predicted_class = which.max(c(.data$p_0, .data$p_1, .data$p_2, .data$p_3)) - 1,
                    f_id = paste(.data$location, .data$date, .data$species, sep="_"))
    
    if(!forecast_mode) {
      forecast_list <- dplyr::mutate(forecast_list,
                                     actual_class = tbl$actual_class[1],
                                     actual_toxicity = tbl$actual_toxicity[1])
    }
    
    if(forecast_mode) {
      forecast_list <- dplyr::mutate(forecast_list,
                                     class_bins = paste(cfg$image_list$tox_levels, collapse = ","),
                                     forecast_start_date = as.Date(as.numeric(date), origin = as.Date("1970-01-01")) + cfg$image_list$forecast_steps*7-3,
                                     forecast_end_date = as.Date(as.numeric(date), origin = as.Date("1970-01-01")) + cfg$image_list$forecast_steps*7+3,
                                     .after="species")
    }
    return(forecast_list)
  }
  
  ensemble_forecast <- predictions |> 
    dplyr::group_by(.data$location, .data$date, .data$species) |> 
    dplyr::group_map(site_forecast, .keep=TRUE) |> 
    dplyr::bind_rows() |> 
    dplyr::arrange(.data$date, .data$location)
  
  return(ensemble_forecast)
}
