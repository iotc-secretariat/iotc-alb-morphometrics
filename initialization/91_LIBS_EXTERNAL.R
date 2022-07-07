# Install/load libraries required for analysis
# Install/load pacman
if(!require(pacman)){
  install.packages("pacman")
  suppressPackageStartupMessages(library(pacman,quietly = TRUE))
}

p_load(
  "tidyverse", 
  "flextable", 
  "scales", 
  "openxlsx", 
  "rmarkdown", 
  "knitr", 
  "bookdown", 
  "officer", 
  "dplyr", 
  "ggsci", 
  "data.table", 
  "sf", 
  "colorspace", 
  "wellknown", 
  "rnaturalearth", 
  "MASS", 
  "lubridate", 
  "maps", 
  "mapdata", 
  "lubridate", 
  "mgcv", 
  "mgcViz", 
  "randomForest",
  "patchwork"
  )

# Set chart theme to theme_bw
theme_set(theme_bw())

