#' Takes raw data and transforms it to be ready for model input
#' wraps `make_image_list()` and `pool_images_and_labels()` into one function
#' 
#' @param cfg model configuration file
#' @param input_data a tibble of input samples where each row is a sample and each column is a feature
#' @param forecast_mode logical, if true, creates new dummy rows for each station to generate a forecast
#' @returns two lists for forecast model input training and testing data. each list contains:
#' \itemize{
#' \item{labels}
#' \item{image}
#' \item{classifications}
#' \item{toxicity}
#' \item{locations}
#' \item{dates}
#' \item{scaling_factors}
#' }
#' 
#' @export
transform_data <- function(cfg, 
                           input_data, 
                           forecast_mode = FALSE) {
  
  # Log inputs to even distribution
  # Handle multi-sampled weeks
  data <- input_data |> 
    log_inputs(cfg) |>
    find_multisample_weeks(mode = cfg$image_list$multisample_weeks)
  
  if (forecast_mode == TRUE) {
    #call function to make new row after the newest measurement at each site, standard_gap days ahead, with NA values for variables and label
    data <- forecast_dummy(data)
  }
  
  image_list <- make_image_list(raw_data = data,
                                tox_levels =     cfg$image_list$tox_levels,
                                forecast_steps = cfg$image_list$forecast_steps,
                                n_steps =        cfg$image_list$n_steps,
                                minimum_gap =    cfg$image_list$minimum_gap,
                                maximum_gap =    cfg$image_list$maximum_gap,
                                toxins =         cfg$image_list$toxins,
                                environmentals = cfg$image_list$environmentals)
  
  ## splitting by custom train/test splitting functions
  #split_fun <- get(cfg$train_test$split_by)
  #image_list <- split_fun(cfg, image_list)
  
  if (tolower(cfg$train_test$split_by) == "fraction") {
    
    # creates indices for all images to randomly sample for train/test sets
    imgs <- seq(1, length(image_list))
    test_n <- round(length(imgs)*cfg$train_test$test_fraction)
    
    set.seed(seed=cfg$train_test$seed)
    
    TEST <- sample(imgs, test_n)
    
    TRAIN <- setdiff(imgs, TEST)
    
  } else if (tolower(cfg$train_test$split_by) == "year") {
    
    #Splits image_list by year for grouping into train/test data
    years <- sapply(image_list, function(x) {return(x$year)})
    image_list <- split(image_list, years)
    
    #configuration
    TRAIN <-   cfg$train_test$train
    TEST <-    cfg$train_test$test
    
  } else {
    stop("split_by value not known: ", cfg$train_test$split_by)
  }
  

  if (cfg$model$balance_val_set == TRUE) {
    train_val_split <- validation_splitter(image_list[TRAIN], cfg)
    
    #Group the training and testing data
    train <- pool_images_and_labels(train_val_split$train, 
                                    cfg = cfg, 
                                    downsample=cfg$model$downsample)
    
    val <- pool_images_and_labels(train_val_split$val,
                                  cfg = cfg)
    
    test <- pool_images_and_labels(image_list[TEST], 
                                   cfg = cfg)
    
    processed <- list(train = train,
                      val = val,
                      test = test)
  } else {
    train <- pool_images_and_labels(image_list[TRAIN], 
                                    cfg = cfg, 
                                    downsample =cfg$model$downsample,
                                    upsample = FALSE)
    
    test <- pool_images_and_labels(image_list[TEST], 
                                   cfg = cfg,
                                   upsample = FALSE)
    
    processed <- list(train = train,
                      test = test)
  }

  return(processed)
}

