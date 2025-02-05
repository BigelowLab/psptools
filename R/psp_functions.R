#' Set classification values 
#' 
#' @param v vector of numbers (x$total_toxicity)
#' @param lut ordered vector of toxicity levels (tox_levels)
#' @param na_value value to replace missing values in v
#' @return ix vector closure codes
#' 
#' @export
recode_classification <- function(v, 
                                  lut = c(0,10,30,80), 
                                  na_value = 0){
  na <- is.na(v)
  v[na] <- na_value
  
  ix <- findInterval(v, lut) -1
  
  return(ix)
} 


#' Takes toxin and environmental input columns and normalizes to range 0-1
#' 
#' @param x tibble of raw data
#' @param toxins toxins in raw data
#' @param environmentals environmental variables
#' @return tibble of normalized input columns 
#' 
#' @export
normalize_input <- function(x, toxins, environmentals) {
  
  image_cols <- x |> 
    dplyr::select(dplyr::all_of(c(toxins, environmentals)))
  
  other_cols <- x |> 
    dplyr::select(!dplyr::all_of(c(toxins, environmentals)))
  
  scaling_factors <- list(min  = apply(image_cols, 2, min, na.rm=TRUE), 
                          max  = apply(image_cols, 2, max, na.rm=TRUE), 
                          mean = apply(image_cols, 2, mean, na.rm=TRUE), 
                          std  = apply(image_cols, 2, sd, na.rm=TRUE))
  
  scaled_image_cols <- sapply(names(image_cols), function(name) {(image_cols[[name]] - scaling_factors$min[name])/(scaling_factors$max[name] - scaling_factors$min[name])}, simplify=FALSE) |> 
    dplyr::bind_cols()
  
  scaled_data <- dplyr::bind_cols(other_cols, scaled_image_cols)
  
  return(scaled_data)
}


#' For a given station and year, find how many batches(images) can be made
#' 
#' @param nx number of samples for that year and station
#' @param steps number of weeks in the image
#' @return nbatches number of batches that can be made for a given year and station
#' 
#' @export
n_batches <- function(nx, 
                      steps) {
  nbatches <- nx - steps + 1
  
  return(nbatches)
}


#' For a given station and year, find the indices of each batch (image) that can be made from the subset
#' 
#' @param nbatches number of batches that can be made for a given year and station
#' @param steps number of weeks in the image
#' @return batches 
#' 
#' @export
compute_batches <- function(nbatches, 
                            steps) {
  steps <- 1:steps 
  batches <- lapply(1:nbatches, function(n){steps + n - 1})
  
  return(batches)
}


#' Find class weights for imbalanced model input (training data)
#' 
#' @param train_classes labels for training data (target toxicity classification)
#' @return class weight named list
#' 
#' @export
get_class_weights <- function(train_classes) {
  
  classes <- dplyr::tibble(class=train_classes) |> 
    dplyr::count(class) |> 
    dplyr::mutate(weight_max=max(.data$n)/.data$n,
                  weight_inverse=1/.data$n)
  
  class_weights <- classes$weight_max
  names(class_weights) <- classes$class
  
  return(as.list(class_weights))
}


#' Function to balaance training data so all classes are distributed evenly (downsample)
#' 
#' @param s a list of images with imbalanced class frequencies
#' @return list of images with equal class frequencies
#' 
#' @export
balance_classes <- function(s){
  
  classes <- sapply(s, function(x){return(x$classification)})
  s <- split(s, classes)
  
  class_freqs <- dplyr::tibble(class=classes) |> 
    dplyr::count(class) |> 
    dplyr::mutate(freq = .data$n/length(classes))
  
  min_sample <- min(class_freqs$n)
  
  balanced_samples <- lapply(s, function(x){return(sample(x, min_sample))}) |> 
    unlist(recursive=FALSE)
  
  return(balanced_samples)
  
}

#' Function to generate a validation set of labeled samples with an even distribution of each class
#' 
#' @param image_list_subset subset of images for training
#' @param cfg configuration file for model
#' @return list of images to use as a validation set
#' 
#' @export
validation_splitter <- function(image_list_subset, cfg) {
  
  xx <- unlist(image_list_subset, recursive = FALSE)
  
  val_set_size <- as.integer(length(xx)*cfg$model$validation_split)
  val_sample_size <- as.integer(floor(val_set_size/length(cfg$image_list$tox_levels)))
  
  classes <- sapply(xx, function(x){return(x$classification)})
  xx <- split(xx, classes)
  
  class_freqs <- dplyr::tibble(class=classes) |> 
    dplyr::count(class) |> 
    dplyr::mutate(freq = .data$n/length(classes))
  
  min_sample <- as.integer(min(class_freqs$n)/2)
  
  zero_sample = val_set_size-min_sample*3
  
  val0 <- sample(xx$"0", size=zero_sample)
  val1 <- sample(xx$"1", size=min_sample)
  val2 <- sample(xx$"2", size=min_sample)
  val3 <- sample(xx$"3", size=min_sample)
  
  val_set <- c(val0, val1, val2, val3)
  
  xx <- unlist(xx, recursive=FALSE)
  train_set <- setdiff(xx, val_set)
  
  years <- sapply(train_set, function(x) {return(x$year)})
  train_set <- split(train_set, years)
  
  years <- sapply(val_set, function(x) {return(x$year)})
  val_set <- split(val_set, years)
  
  r <- list("train" = train_set,
            "val" = val_set)
  
  return(r)
}

#' Adds a dummy row for forecast targets (which have no data in our package, but need to meet the gap filtering when making images)
#' 
#' @param data the raw data before going into `make_image_list()`
#' @return a modified tibble where each site with data this year has an additional dummy row to make an image for a forecast
#' 
#' @export
forecast_dummy <- function(data) {
  
  add_dummy <- function(tbl, key) {
    
    #need to add standard_gap to config instead of hard wired 7
    forecast_date <- tbl$date[length(tbl$date)] + 7
    
    dummy_added <- tbl |> 
      dplyr::add_row(id = paste("FORECAST_WEEK", sep="_"),
                     location_id = as.character(key),
                     date = forecast_date,
                     #year = format(Sys.Date(), format="%Y"),
                     year = format(max(tbl$date), format="%Y"),
                     gap_days = 7)
    
    return(dummy_added)
  }
  
  new_data <- data |> 
    dplyr::group_by(.data$location_id) |> 
    dplyr::group_map(add_dummy, .keep=TRUE) |> 
    dplyr::bind_rows()
  
  return(new_data)
}

#' Function to log a list of inputs - should this log all of them?
#' 
#' @param data raw input data
#' @param cfg list of model configuration parameters
#' @return same data as input but with log transformed variables
#' 
#' @export
log_inputs <- function(data,
                       cfg) {
  
  vars = c(cfg$image_list$toxins, cfg$image_list$environmentals)
  
  log_transform <- function(var) {
    r <- log10(var+1)
    return(r)
  }
  
  logged_data <- data |> 
    dplyr::mutate_at(vars, log_transform)
  
  return(logged_data)
}


#' Convert to as POSIXct to an 8D week number (1-46) or 7D week number (1-52)
#' (From package 'dismotools')
#'
#' @param x POSIXct or Date vector
#' @param week_length numeric, the number of days per week
#' @return numeric 1-46 or 1-52 week number
#' 
#' @export
date_to_week <- function(x, 
                         week_length = c(8,7)[2]){
  if (missing(x)) x <- Sys.Date()
  J <- as.numeric(format(x, "%j"))
  (J-1) %/% week_length + 1
}



#' Balances class distribution in training set by adding duplicates to classes that are more rare
#' 
#' @param images a list of images with an imbalanced class distribution
#' @return a training set of images with each class appearing the same number of times
#' 
#' @export
upsample <- function(images) {
  
  classes <- sapply(images, function(x){return(x$classification)})
  split_images <- split(images, classes)
  
  class_freqs <- dplyr::tibble(class=classes) |> 
    dplyr::count(class) |> 
    dplyr::mutate(freq = .data$n/length(classes),
                  max_p = max(.data$n)/.data$n)
  
  max_sample <- max(class_freqs$n)
  
  balanced_samples <- lapply(split_images, function(x){return(rep_len(x, max_sample))}) |> 
    unlist(recursive=FALSE)
  
  return(balanced_samples)
}


