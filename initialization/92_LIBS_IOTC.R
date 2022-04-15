# Install/load pacman
if(!require(pacman)){
  install.packages("pacman")
  suppressPackageStartupMessages(library(pacman,quietly = TRUE))
}

# Install/load libraries required for analysis
pacman::p_load("iotc.base.common.plots")

print("Initializing IOTC libs...")

# Initialise species colors
initialize_all_species_colors()
initialize_all_gears_colors()

print("IOTC libs fully initialized")

