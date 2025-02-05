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