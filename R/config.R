#' Function to read configuration yaml file for model
#' 
#' @param filename name of config yaml file
#' @param autopopulate optional argument of fields to autopopulate
#' @param old logical if true then config file will be read from `configurations/old`
#' @return config list of configuration elements for model
#' 
#' @export
read_config <- function(filename, autopopulate = "num_classes", old=FALSE){
  
  if (old) {
    c_file_path <- system.file("configurations", "old_configurations", filename, package="psptools")
  } else {
    c_file_path <- system.file("configurations", filename, package="psptools")
  }
  
  config <- yaml::read_yaml(c_file_path)
  
  if (inherits(autopopulate, "character")) {
    if ("num_classes" %in% autopopulate) {
      config$model$num_classes = length(config$image_list$tox_levels)
    }
  }
  
  return(config)
}


#' Tests if required configuration options are present
#' @param cfg a psptools configuration
#' @returns NULL if pass or error if fail
#' @export
is_cfg_valid <- function(cfg) {
  if (any(is.null(cfg$train_test$train$species),
          is.null(cfg$train_test$train$region),
          is.null(cfg$train_test$train$year),
          is.null(cfg$train_test$test$species),
          is.null(cfg$train_test$test$region),
          is.null(cfg$train_test$test$year))) {
    stop("Configuration object `cfg` must contain values for species, region and year for both train and test. 
         See https://bigelowlab.github.io/psptools-guide/configuration_files for more information")
  }
}