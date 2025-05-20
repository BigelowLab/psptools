#' Function to build, compile and fit the model to the training data.
#' The model is then evaluates with the test data.
#' 
#' @param cfg model configuration file
#' @param model_input output of transform_data()
#' @param forecast_mode logical if testing a model with unknown test targets (aka making a forecast with a trained model)
#' @return forecast object containing an evaluation of the model, information about test data and actual classifications
#' \itemize{
#' \item{forecast list containing prediction information along with model object and history. if forecast_mode==FALSE, also returns test set information and metrics}
#' \item{model fitted keras model object}
#' \item{history a keras_training_history object containing metrics from model fitting}}
#' 
#' @export
forecast_model <- function(cfg, model_input, forecast_mode=FALSE) {
  
  # Define the model
  model <- keras::keras_model_sequential() |> 
    keras::layer_dense(units = cfg$model$units1,
                       activation = "relu", 
                       input_shape = dim(model_input$test$image)[2], 
                       name = "layer1") |> 
    keras::layer_dropout(rate = cfg$model$dropout1) |> 
    keras::layer_dense(units = cfg$model$units2,
                       activation = "relu",
                       name = "layer2") |> 
    keras::layer_dropout(rate = cfg$model$dropout2) |> 
    keras::layer_dense(units = cfg$model$num_classes, 
                       activation = "softmax")
  
  #Compile the model
  model <- model |> 
    keras::compile(optimizer =  cfg$model$optimizer,
                   loss =       cfg$model$loss_function, 
                   metrics =    cfg$model$model_metrics)
  
  #Fit the training data to the model
  if (cfg$model$use_class_weights == TRUE) {
    
    class_weights <- get_class_weights(model_input$train$classifications)
    
    history <- model |> 
      keras::fit(x =                model_input$train$image,
                 y =                model_input$train$labels,
                 batch_size =       cfg$model$batch_size,
                 epochs =           cfg$model$epochs,
                 validation_split = cfg$model$validation_split,
                 #validation_data =  list(model_input$val$image, model_input$val$labels),
                 shuffle =          cfg$model$shuffle,
                 class_weight =     class_weights)
  } else {
    history <- model |> 
      keras::fit(x =                model_input$train$image,
                 y =                model_input$train$labels,
                 batch_size =       cfg$model$batch_size,
                 epochs =           cfg$model$epochs,
                 validation_split = cfg$model$validation_split,
                 #validation_data =  list(model_input$val$image, model_input$val$labels),
                 shuffle =          cfg$model$shuffle)
  }
  
  if (forecast_mode == TRUE) {
    
    predicted_probs <- model |> 
      predict(model_input$test$image)
    
    forecast <- list(year = cfg$train_test$test,
                     dates = model_input$test$dates,
                     locations = model_input$test$locations,
                     species = model_input$test$species,
                     predicted_probs = predicted_probs)
    
    model_output <- list(forecast = forecast, 
                         model = model, 
                         history = history)
  } else {
    metrics <- model |> 
      keras::evaluate(x = model_input$test$image, 
                      y = model_input$test$label)
    
    predicted_probs <- model |> 
      predict(model_input$test$image)
    
    forecast <- list(metrics = metrics,
                     year = cfg$train_test$test,
                     dates = model_input$test$dates,
                     locations = model_input$test$locations,
                     species = model_input$test$species,
                     test_classifications = model_input$test$classifications,
                     test_toxicity = model_input$test$toxicity,
                     predicted_probs = predicted_probs)
    
    model_output <- list(forecast = forecast, 
                         model = model, 
                         history = history)
  }
  return(model_output)
}
