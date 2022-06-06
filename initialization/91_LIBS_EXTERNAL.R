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
  "ggpubr",
  "gridExtra",
  "rmarkdown",
  "knitr",
  "bookdown",
  "officer",
  "dplyr",
  "ggsci",
  "data.table",
  "sf",
  "colorspace",
  "patchwork",
  "wellknown",
  "rnaturalearth",
  "MASS",
  "ggforce",
  "lubridate"
  )

# Set chart theme to theme_bw
theme_set(theme_bw())

