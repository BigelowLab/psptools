



split_by_year <- function(cfg, image_list) {
  
  years <- sapply(image_list, function(x) {return(x$year)})
  image_list <- split(image_list, years)
  
  #configuration
  TRAIN <-   cfg$train_test$train
  TEST <-    cfg$train_test$test
}


split_by_random <- function(cfg, image_list) {
  imgs <- seq(1, length(image_list))
  test_n <- round(length(imgs)*cfg$train_test$test_fraction)
  
  set.seed(seed=cfg$train_test$seed)
  
  TEST <- sample(imgs, test_n)
  TRAIN <- setdiff(imgs, TEST)
}