#' Takes raw input data, filters for image criteria and creates images with dimensions (n_steps + forecast steps) x length(toxins + environmentals)
#' 
#' @param raw_data database with toxin measurements with their date sampled, location, shellfish species and additional environmental data
#' @param cfg psptools configuration
# @param tox_levels toxin level categories used for classifying total toxicity
# @param forecast_steps the number of weeks ahead of the image the forecast is made for
# @param n_steps the number of weeks of samples in an image
# @param minimum_gap the smallest gap between samples allowed into an image
# @param maximum_gap the largest gap between samples allowed into an image
# @param toxins list of individual paralytic toxin names (12) for toxin columns
# @param environmentals environmental variables
#' @return each list is an image along with its associated data (location_id, date, etc.)
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
make_image_list <- function(raw_data, 
                            cfg) {
  
  if (!"year" %in% colnames(raw_data)) {
    raw_data <- dplyr::mutate(raw_data, year = format(date, "%Y"))
  }
  
  normalized_data <- raw_data |> 
    dplyr::arrange(.data$location_id, .data$date) |> # make sure rows are ordered by site, date
    compute_gap() |> # gap should be updated anytime data enters this subroutine?
    dplyr::mutate(classification = recode_classification(.data$total_toxicity, cfg$image_list$tox_levels), # update classification
                  meets_gap = check_gap(.data$gap_days, cfg$image_list$minimum_gap, cfg$image_list$maximum_gap)) |> # check gap requirement
    normalize_input(cfg$image_list$toxins, cfg$image_list$environmentals) 
  
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
                    species =        image_batch$species[1],
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
    
    nbatches <- n_batches(nrow(tbl), (n_steps+forecast_steps))      # how many images can we make?
    batches <- compute_batches(nbatches, (n_steps+forecast_steps))  # define the indices of each image
    
    xx <- lapply(batches, make_images, tbl, forecast_steps, n_steps, minimum_gap, maximum_gap, toxins, environmentals)
    gap_verified <- sapply(xx, function(x){return(x$status)})
    
    xx <- xx[gap_verified]
    
    return(xx)
  }
  
  image_list <- normalized_data |>
    dplyr::group_by(.data$location_id, .data$year, .data$species) |>
    dplyr::group_map(find_images, cfg$image_list$forecast_steps, cfg$image_list$n_steps, cfg$image_list$minimum_gap, cfg$image_list$maximum_gap, cfg$image_list$toxins, cfg$image_list$environmentals, .keep=TRUE) |> 
    unlist(recursive = FALSE)
  
  return(image_list)
}
