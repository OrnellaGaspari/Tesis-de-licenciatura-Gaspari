## Name: libraries.R ##
## Author: Inês Silva ##
## Date: 18th March 2026

## Goal? Install & load all necessary libraries

library(easypackages)
easypackages::packages(
  
  # file paths & directories
  "here",
  "fs",
  "tools",
  
  # file storage & reading
  "readr",
  "readxl",
  "writexl",
  "gt",
  "officer",
  
  # spatial data processing  
  "terra",
  "raster",
  "sp",
  "sf",
  "rnaturalearth",
  "rnaturalearthdata",
  "rworldmap",
  "spThin",
  
  # species Distribution Modelling
  "biomod2", 
  "gam",
  "mda", 
  "earth", 
  "maxnet",
  "xgboost",
  "MAXENT", 
  "randomForest",
  "rgbif",
  
  # data manipulation & visulisation
  "dplyr",
  "tidyverse",
  "ggplot2",
  "stringr",
  "tibble",
  "tidyterra",
  "tidyr",
  "rphylopic",
  "viridis",
  "patchwork",
  "RColorBrewer",
  "mapview",
  "janitor",
  "lubridate",
  "usdm",
  prompt = FALSE)