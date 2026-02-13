# Script for preparing new configuration files


# single year test example
cfg <- list(
  configuration="test_2024",
  image_list = list(tox_levels = c(0,10,30,80),
                    forecast_steps = 1,
                    n_steps = 2,
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
  train_test = list(split_by = "year_region_species",
                    train = list(species = "mytilus",
                                 region = "maine",
                                 year=c("2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023", "2024")), 
                    test = list(species = "mytilus",
                                region = "maine",
                                year = c("2025")))
)


# forecast mode example
cfg <- list(
  configuration="v0.4.0",
  image_list = list(tox_levels = c(0,10,30,80),
                    forecast_steps = 1,
                    n_steps = 2,
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
               epochs = 64, 
               validation_split = 0.2,
               shuffle = TRUE,
               num_classes = 4,
               optimizer="adam",
               loss_function="categorical_crossentropy",
               model_metrics=c("categorical_accuracy")),
  train_test = list(split_by = "forecast_mode",
                    train = list(species = "mytilus",
                                 region = "maine",
                                 year=c("2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023", "2024", "2025")), 
                    test = list(species = "mytilus",
                                region = "maine",
                                year = c("2025")))
)

library(yaml)

existing <- list.files("inst/configurations/")

new_config_name <- "v0.4.0.yaml"

if (!new_config_name %in% existing) {
  file_path <- file.path("inst/configurations/", new_config_name)
  write_yaml(cfg, file=file_path)
}


