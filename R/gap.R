#' Finds gap between samples in the same season at the same site
#' 
#' @param x tibble of toxin measurement data 
#' @return the input tibble with a column `gap_days` containing the difference between samples in the same season
#' 
#' @export
compute_gap <- function(x) {
  
  find_gap <- function(tbl, key) {
    g <- tbl |>
      dplyr::mutate(gap_days = c(0, ceiling(diff(date))))
    return(g)
  }
  r <- x |> 
    dplyr::arrange(.data$location_id, .data$date) |>
    dplyr::group_by(.data$location_id, .data$year) |> 
    dplyr::group_map(find_gap, .keep = TRUE) |>
    dplyr::bind_rows()
  
  return(r)
}

#' Checks gap_days column to filter for only
#' 
#' @param x gap days column from raw data
#' @param minimum_gap gaps allowed into an image must be greater than
#' @param maximum_gap gaps allowed into an image must be less than
#' @return logical for each row in data - TRUE if gap meets gap criteria, FALSE if not
#' 
#' @export
check_gap <- function(x, minimum_gap, maximum_gap) {
  
  #gap_status <- lapply(x, function(gap) {if (minimum_gap < gap && gap < maximum_gap) {return(TRUE)} else {return(FALSE)}} )
  gap_status <- x >= minimum_gap & x <= maximum_gap
  
  return(gap_status)
}
