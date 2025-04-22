required_packages <- c("dplyr", "stringr", "ranger", "treeshap", "tidyverse", "tidymodels", 
                       "vip", "ggplot2", "pdp", "DALEXtra", "tidyr", "ggbeeswarm", "rpart", 
                       "rpart.plot", "scales", "rlang", "sf", "rnaturalearth", "rnaturalearthdata", 
                       "rsvg", "car", "caret", "corrplot", "fastshap", "fs", "ggcorrplot", 
                       "inTrees", "janitor", "lsr", "naniar", "plotly", "xgboost", "yaml", 
                       "yardstick")

# Function to check and install packages
check_and_install <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(paste("Package", pkg, "not found. Attempting to install..."))
    install.packages(pkg, lib = "~/R/libs", repos = "https://cran.rstudio.com/")
  }
}

# Set the library path
.libPaths(c("~/R/libs", .libPaths()))

# Check and install missing packages
invisible(sapply(required_packages, check_and_install))

# Load all required packages
invisible(lapply(required_packages, library, character.only = TRUE))
