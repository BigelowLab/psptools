#' Takes raw input data, filters for image criteria and creates images with dimensions (n_steps + forecast steps) x length(toxins + environmentals)
#' 
#' @param input_data tibble containing PSP measurements with their date sampled, location, shellfish species and additional covariates
#' @param cfg psptools configuration
#' @return list of lists containing an image along with its associated data (location_id, date, etc.)
#' \itemize{
#' \item{status logical if the image passes the image gap criteria (gap >= minimum_gap & gap <= maximum_gap)}
#' \item{year the year the image is from}
#' \item{location_id the sampling station id}
#' \item{species the shellfish species the image was made for}
#' \item{date the date of the final row in the image (the forecast is for forecast_steps ahead of this date)}
#' \item{toxixty the total toxicity used to regress on instead of classify a binned toxicity}
#' \item{classification the classification (0:num_classes) of the final row in the image}
#' \item{image a 2 dimensional array with the dimensions (n_steps + forecast steps) x length(toxins + environmentals)}
#' }
#' 
#' @export
make_image_list <- function(input_data, 
                            cfg) {
  
  if (!"year" %in% colnames(input_data)) {
    input_data <- dplyr::mutate(input_data, year = format(date, "%Y"))
  }
  
  find_images <- function(tbl, key, forecast_steps, n_steps, minimum_gap, maximum_gap, toxins, environmentals) {
    
    make_images <- function(batch, tbl, forecast_steps, n_steps, minimum_gap, maximum_gap, toxins, environmentals) {
      
      image_batch <- dplyr::slice(tbl, batch)
      
      if (any(image_batch$meets_gap[2:(n_steps+forecast_steps)] == FALSE)) {
        z <- list(status=FALSE)
      } 
      else {
        image <- as.matrix(dplyr::ungroup(image_batch) |> 
                             dplyr::select(dplyr::all_of(c(toxins, environmentals))))
        
        if (image_batch$id[n_steps+forecast_steps] == "FORECAST_WEEK") {
          z <- list(status=          TRUE,
                    year =           "FORECAST_IMAGE",
                    location_id =    image_batch$location_id[1],
                    species =        image_batch$species[1],
                    date =           image_batch$date[n_steps],
                    classification = image_batch$classification[n_steps+forecast_steps],
                    toxicity =       image_batch$total_toxicity[n_steps+forecast_steps],
                    image =          image[1:n_steps,])
        } else {
          z <- list(status=          TRUE,
                    year =           image_batch$year[1],
                    location_id =    image_batch$location_id[1],
                    location_id_int = image_batch$location_id_int[1],
                    species =        image_batch$species[1],
                    species_id_int =    image_batch$species_id_int[1],
                    date =           image_batch$date[n_steps],
                    classification = image_batch$classification[n_steps+forecast_steps],
                    toxicity =       image_batch$total_toxicity[n_steps+forecast_steps],
                    image =          image[1:n_steps,])
        }
        return(z)
      }
    }
    
    if (nrow(tbl) < (n_steps+forecast_steps)) {
      return(NULL) 
    }
    
    tbl <- compute_gap(tbl) |>
      dplyr::mutate(meets_gap = check_gap(.data$gap_days, minimum_gap, maximum_gap))
    
    nbatches <- n_batches(nrow(tbl), (n_steps+forecast_steps))      # how many images can we make?
    batches <- compute_batches(nbatches, (n_steps+forecast_steps))  # define the indices of each image
    
    xx <- lapply(batches, make_images, tbl, forecast_steps, n_steps, minimum_gap, maximum_gap, toxins, environmentals)
    gap_verified <- sapply(xx, function(x){return(x$status)})
    
    xx <- xx[gap_verified]
    
    return(xx)
  }
  
  image_list <- input_data |>
    dplyr::group_by(.data$location_id, .data$year, .data$species) |>
    dplyr::group_map(find_images, cfg$image_list$forecast_steps, cfg$image_list$n_steps, cfg$image_list$minimum_gap, cfg$image_list$maximum_gap, cfg$image_list$toxins, cfg$image_list$environmentals, .keep=TRUE) |> 
    unlist(recursive = FALSE)
  
  return(image_list)
}
