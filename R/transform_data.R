#' Takes raw data and transforms it to be ready for model input
#' wraps `make_image_list()` and `pool_images_and_labels()` into one function
#' 
#' @param cfg model configuration file
#' @param input_data a tibble of input samples where each row is a sample and each column is a feature
#' @param forecast_mode logical, if true, creates new dummy rows for each station to generate a forecast
#' @returns two lists for transformed forecast model input training and testing data as `pool_images_and_labels()` output.
#' 
#' @export
transform_data <- function(cfg, 
                           input_data, 
                           forecast_mode = FALSE) {
  data <- input_data |> 
    log_inputs(cfg) |> # Log inputs to even distribution
    find_multisample_weeks(mode = cfg$image_list$multisample_weeks) |> # Handle multi-sampled weeks 
    normalize_input(cfg$image_list$toxins, cfg$image_list$environmentals) |> # normalize all of the input variables before splitting 
    dplyr::arrange(.data$location_id, .data$date) |> # make sure rows are ordered by site, date
    dplyr::mutate(classification = recode_classification(.data$total_toxicity, cfg$image_list$tox_levels)) # update classification
    
  #if (forecast_mode == TRUE) {
  #  #call function to make new row after the newest measurement at each site, standard_gap days ahead, with NA values for variables and label
  #  data <- forecast_dummy(data)
  #}
  
  if (tolower(cfg$train_test$split_by) == "year_region_species") {
    is_cfg_valid(cfg)
    
    test_data <- dplyr::filter(data, .data$year %in% cfg$train_test$test$year & 
                                     .data$species %in% cfg$train_test$test$species &
                                     .data$region %in% cfg$train_test$test$region)
    
    test <- make_image_list(test_data, cfg) |>
      pool_images_and_labels(cfg)
    
    train_data <- dplyr::filter(data, .data$year %in% cfg$train_test$train$year & 
                                      .data$species %in% cfg$train_test$train$species & 
                                      .data$region %in% cfg$train_test$train$region &
                                      !.data$id %in% test_data$id)
    
    train <- make_image_list(train_data, cfg) |>
      pool_images_and_labels(cfg)
    
  } else if (tolower(cfg$train_test$split_by) == "fraction") {
    
    image_list <- make_image_list(data, cfg)
    
    imgs <- seq(1, length(image_list)) # creates indices for all images to randomly sample for train/test sets
    test_n <- round(length(imgs)*cfg$train_test$test_fraction)
    
    set.seed(seed=cfg$train_test$seed)
    
    TEST <- sample(imgs, test_n)
    
    TRAIN <- setdiff(imgs, TEST)
    
    train <- image_list[TRAIN]
    test <- image_list[TEST]
    
  } else if (tolower(cfg$train_test$split_by) == "forecast_mode") {
    is_cfg_valid(cfg)
    
    test_data <- dplyr::filter(data, .data$year %in% cfg$train_test$test$year & 
                                 .data$species %in% cfg$train_test$test$species &
                                 .data$region %in% cfg$train_test$test$region) |>
      forecast_dummy()
    
    test <- make_image_list(test_data, cfg) |>
      pool_images_and_labels(cfg)
    
    train_data <- dplyr::filter(data, .data$year %in% cfg$train_test$train$year & 
                                  .data$species %in% cfg$train_test$train$species & 
                                  .data$region %in% cfg$train_test$train$region)
    
    train <- make_image_list(train_data, cfg) |>
      pool_images_and_labels(cfg)
  }
  
  #else if (tolower(cfg$train_test$split_by) == "function") {
    # splitting by custom train/test splitting functions
    #split_fun <- get(cfg$train_test$split_by)
    #image_list <- split_fun(cfg, image_list)
  #}
  
  processed <- list(train = train,
                    test = test)
  
  # do we still want to keep balance validation set as an option
  #if (cfg$model$balance_val_set == TRUE) {
  #  train_val_split <- validation_splitter(image_list[TRAIN], cfg)
  #  
  #  #Group the training and testing data
  #  train <- pool_images_and_labels(train_val_split$train, 
  #                                  cfg = cfg, 
  #                                  downsample=cfg$model$downsample)
  #  
  #  val <- pool_images_and_labels(train_val_split$val,
  #                                cfg = cfg)
  #  
  #  test <- pool_images_and_labels(image_list[TEST], 
  #                                 cfg = cfg)
  #  
  #  processed <- list(train = train,
  #                    val = val,
  #                    test = test)
  #} 
  
  return(processed)
}

