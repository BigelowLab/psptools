## Keras setup script 

install.packages("keras")
library(reticulate)
virtualenv_create("r-reticulate", python=install_python())

library(keras)
install_keras(envname = "r-reticulate")

# Restart, then check that it worked:

packageVersion("keras")
packageVersion("tensorflow")