#' Function to plot confusion matrix from forecast model test run with past data (known targets in test data)
#' 
#' @param cfg the configuration object used to build and train the model
#' @param forecast_list formatted predictions from forecast model
#' @return confusion_matrix plot of actual vs predicted labels in the test set
#' 
#' @export 
make_confusion_matrix <- function(cfg, forecast_list) {
  
  num_levels <- length(cfg$image_list$tox_levels)
  levels <- seq(from=0, to=(num_levels-1))
  
  #cm <- as.data.frame(table(predicted = factor(model$predicted_classifications, levels), actual = factor(model$test_classifications, levels)))
  
  cm <- as.data.frame(table(predicted = factor(forecast_list$predicted_class, levels),
                            actual = factor(forecast_list$actual_class, levels)))  |> 
    dplyr::mutate(frac = round(.data$Freq/sum(.data$Freq)*100)) |> 
    dplyr::mutate(frac = sapply(.data$frac, function(x) if (x == "0") {x = "<1"} else {x}))
  
  confusion_matrix <- ggplot2::ggplot(data = cm,
                                      mapping = ggplot2::aes(x = .data$predicted, y = .data$actual)) +
    ggplot2::geom_tile(ggplot2::aes(fill = log(.data$Freq+1))) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%1.0f", .data$Freq)), vjust = 1, size=8) +
    ggplot2::geom_text(ggplot2::aes(label = paste(.data$frac, "%", sep="")), vjust = 4, size=4)+
    ggplot2::scale_fill_gradient(low = "white", 
                                 high = "blue") +
    ggplot2::labs(x = "Predicted Classifications", 
                  y = "Actual Classifications") +
    ggplot2::theme_linedraw() +
    ggplot2::theme(axis.text=  ggplot2::element_text(size=14),
                   axis.title= ggplot2::element_text(size=14,face="bold"),
                   title =     ggplot2::element_text(size = 14, face = "bold"),
                   legend.position = "none") 
  
  return(confusion_matrix)
  
}
