# Script for preparing new configuration files

library(yaml)

cfg <- list(
  configuration="irish_data_test",
  image_list = list(tox_levels = c(0,10,60,80),
                    forecast_steps = 1,
                    n_steps = 3,
                    minimum_gap = 4,
                    maximum_gap = 10,
                    multisample_weeks = "last",
                    toxins = c("gtx1", "gtx4", "gtx2", "gtx3", "dcgtx2", "dcgtx3", "gtx5", "neo", "dcstx", "stx", "c1", "c2")),
  model = list(balance_val_set=FALSE,
               downsample=FALSE,
               use_class_weights=FALSE,
               dropout1 = 0.3,
               dropout2 = 0.3,
               batch_size = 32, 
               units1 = 32, 
               units2 = 32, 
               epochs = 128, 
               validation_split = 0.2,
               shuffle = TRUE,
               num_classes = 4,
               optimizer="adam",
               loss_function="categorical_crossentropy",
               model_metrics=c("categorical_accuracy")),
  train_test = list(split_by = "year",
                    train = list(species = "mytilus",
                                 region = "maine",
                                 year=c("2014", "2015")), 
                    test = list(species = "mytilus",
                                region = "maine",
                                year = c("2016")))
)

write_yaml(cfg, file="inst/configurations/new_config.yaml")
