#' Saves a pre-trained PSP forecast model to the saved_models directory
#' @param model a pre-trained model
#' @param model_name character name to save model as
#' @returns NULL
#' @export
save_psp_model <- function(model,
                           model_name) {
  model_dir <- system.file(file.path("saved_models"), package="psptools")
  model_path <- file.path(model_dir, model_name)
  keras::save_model_hdf5(model, model_path)
}

#' Loads a pre-trained PSP forecast model from the saved_models directory
#' @param model_name character name of saved model
#' @returns NULL
#' @export
load_psp_model <- function(model_name) {
  model_dir <- system.file(file.path("saved_models"), package="psptools")
  model_path <- file.path(model_dir, model_name)
  keras::load_model_hdf5(model_path)
}