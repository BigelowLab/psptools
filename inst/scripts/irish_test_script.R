## Script for testing package installation and configurations for Irish PSP data

suppressPackageStartupMessages({
  library(psptools)
  
  library(dplyr)
  library(readxl)
  library(tidyr)
})

# Define configuration for the test
cfg <- list(
  configuration="irish_data_test",
  image_list = list(tox_levels = c(0,10,30,80),
                    forecast_steps = 1,
                    n_steps = 2,
                    minimum_gap = 4,
                    maximum_gap = 10,
                    multisample_weeks = "last",
                    toxins = c("gtx14", "gtx23", "dcgtx23", "gtx5", "neo", "dcstx", "stx", "c12")),
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
  train_test = list(split_by = "year",
                    train = c("2019", "2020", "2021", "2022", "2024"), 
                    test = c("2023"))
)



# Read in raw data

input_data <- read_excel("~/Documents/data/ireland/HABS2DataExtractFrom2019_PSPBiotoxinKY-CH.xlsx") |>
  select(`Sample Date`, `Shellfish Species`, `Sample Site Code`, `PST Overall Result`, `Isomer name`, `Isomer Result`) |>
  pivot_wider(names_from = `Isomer name`,
              values_from = `Isomer Result`) |>
  rename(date = `Sample Date`,
         species = `Shellfish Species`,
         location_id = `Sample Site Code`,
         total_toxicity = `PST Overall Result`,
         dcgtx23 = `decarbamoy Gonyauxtoxin-2,3`,
         dcneo = `decarbamoy NeoSaxitoxin`,
         dcstx = `decarbamoyl Saxitoxin`,
         gtx14 = `Gonyautoxin-1,4 (GTX-1,4)`,
         gtx23 = `Gonyautoxin-2,3 (GTX-2,3)`,
         gtx5 = `Gonyautoxin-5 (GTX-5)`,
         neo = `NeoSaxitoxin (Neo)`,
         c12 = `N-sulfocarbamoyl C-1,2`,
         stx = `Saxitoxin (STX)`) |>
  mutate(total_toxicity =total_toxicity/10,
         classification = 0,
         year = format(date, format="%Y"),
         date = as.Date(date),
         id = paste(location_id, date, sep="_")) |>
  filter(species == "Mytilus edulis" & !location_id == "KY-CH") |>
  compute_gap()


glimpse(input_data)


# Prepare model input
model_input <- transform_data(cfg, input_data, forecast_mode=FALSE)

# Run the model and make predictions
test <- run_model(cfg, model_input)

# Metrics
test$metrics

# Confusion matrix
make_confusion_matrix(cfg, test$predictions)
