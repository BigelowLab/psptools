#' Finds cases where a monitoring station has been sampled more than once in a week
#' 
#' @param data tibble of psp monitoring data
#' @param mode character specifying which sample to keep; mode "none" will not remove any samples
#' @return a tibble of psp monitoring data with only one sample per station per week
#' 
#' @export
find_multisample_weeks <- function(data, 
                                   mode = c("first", "last", "min", "max", "none")[1]) {
  
  if (mode == "none") {
    return(data)
  }
  
  # fixes multisample weeks (msw) using specified method in mode
  fix_msw <- function(tbl, key, mode) {
    
    if (mode == "first") {
      tbl <- tbl |>
        dplyr::filter(.data$date == min(.data$date))
    } else if (mode == "last") {
      tbl <- tbl |> 
        dplyr::filter(.data$date == max(.data$date))
    } else if (mode == "min") {
      tbl <- tbl |> 
        dplyr::filter(.data$total_toxicity == min(.data$total_toxicity))
    } else if (mode == "max") {
      tbl <- tbl |> 
        dplyr::filter(.data$total_toxicity == max(.data$total_toxicity))
    }
    
    return(tbl)
  }
  
  if (!"year" %in% colnames(data)) {
    data <- data |> 
      dplyr::mutate(year = format(date, format = "%Y"))
  }
  
  r <- data |>
    dplyr::mutate(week = date_to_week(date), .after=date) |>
    dplyr::group_by(.data$location_id, .data$year, .data$week) |>
    dplyr::group_map(fix_msw, mode, .keep=TRUE) |>
    dplyr::bind_rows() |>
    compute_gap()
    
  return(r)
  
}