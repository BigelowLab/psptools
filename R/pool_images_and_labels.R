#' Creates images and labels for input into neural net
#' Image takes all images in psp_lst and stretches them into an array
#' Labels takes classifications and categorizes them for nn input
#' 
#' @param image_list_subset subset of image list from make_image_list() for either training or testing data
#' @param cfg model configuration file 
#' @param missing_value value to replace na with
#' @param scaling_factors null if training data; training data scaling factors are passed to scale testing data
#' @param scaling selected method to scale input data
#' @param downsample logical indicating whether or not to balance the frequency of each class in the training images
#' @param upsample logical to call a function that balances class distribution by upsampling rare classes
#' @return list containing the formatted images and their labels as keras model input, and additional data
#' \itemize{
#' \item{labels}
#' \item{image a 2 dimensional array where each row is an image and the columns are toxins and environmentals from each week}
#' \item{classifications the classification of each image}
#' \item{locations the sampling station of each image}
#' \item{dates the date of the final week of each image}
#' \item{scaling_factors}
#' }
#' 
#' @export
pool_images_and_labels <- function(image_list_subset, 
                                   cfg, 
                                   missing_value = 0.5, 
                                   scaling_factors = NULL, 
                                   scaling = c("normalize", "input_scale")[2],
                                   downsample=FALSE,
                                   upsample=FALSE) {
  
  xx <- image_list_subset
  
  if (upsample == TRUE) {
    xx <- upsample(xx)
  }
  
  if (downsample == TRUE) {
    xx <- xx |> 
      balance_classes() |> 
      sample()
  } else{
    xx <- sample(xx)
  }
  
  dim_image <- dim(xx[[1]]$image)
  
  images <- lapply(xx, function(x){return(x$image)})
  
  image <- abind::abind(images, along = 3) |> 
    aperm(c(3, 1, 2)) |> 
    keras::array_reshape(c(length(xx), prod(dim_image)))
  
  image <- image |> 
    replace_na(missing_value = missing_value)
  
  labels <- sapply(xx, function(x){return(x$classification)}) |> 
    keras::to_categorical(num_classes = cfg$model$num_classes)
  
  classifications <- sapply(xx, function(x){return(x$classification)})
  attr(classifications, "names") <- NULL
  
  locations <- sapply(xx, function(x){return(x$location_id)})
  attr(locations, "names") <- NULL
  
  location_id_int <- sapply(xx, function(x){return(x$location_id_int)})
  attr(location_id_int, "names") <- NULL
  
  dates <- sapply(xx, function(x){return(x$date)})
  attr(dates, "names") <- NULL
  
  toxicity <- sapply(xx, function(x){x$toxicity})
  attr(toxicity, "names") <- NULL
  
  species <- sapply(xx, function(x){x$species})
  attr(species, "names") <- NULL
  
  species_id_int <- sapply(xx, function(x){x$species_id_int})
  attr(species_id_int, "names") <- NULL
  
  if (!is.null(cfg$image_list$species)) {
    image <- add_categorical_species(cfg, image, species)
  }
  
  r <- list(labels = labels, 
            image = image, 
            classifications = classifications,
            toxicity = toxicity,
            species = species,
            species_id_int = species_id_int,
            locations = locations,
            location_id_int = location_id_int,
            dates = dates,
            scaling_factors = scaling_factors)
  
  return(r)
} 


#' Replace any NA values with specified missing value
#' @param x 2D array image object 
#' @param missing_value integer to replace NA toxin levels with
#' @return x
replace_na <- function(x, missing_value = -1) {
  x[is.na(x)] <- missing_value
  return(x)
}

#' Adds categorical variables to each image
#' @param cfg list defining psptools configuration
#' @param image 2D array where each row is an image and the columns are toxins and environmentals from each week 
#' @param species vector of species the length of `image`
#' @return image with additional cols representing categorical species
#' 
add_categorical_species <- function(cfg, image, species) {
  s <- unique(c(cfg$train_test$train$species, cfg$train_test$test$species))
  lut <- seq(0,length(s)-1)
  names(lut) <- s
  
  z <- sapply(species, function(x){return(lut[[x]])})
  attr(z, "names") <- NULL
  
  species_cats <- keras::to_categorical(z, num_classes = length(s))
  
  new_image <- cbind(image, species_cats)
}
