#----------------------------
#--- Install Required Packages
#============================

#--- Packages to Install ----
packages <- c(
  # "shiny",
  # "shinyAce",
  # "rsconnect",
  "parallel",
  "MASS",
  # "lazyeval",
  "gridExtra",
  "tidyr",
  "dplyr",
  "purrr",
  "pbapply",
  "ggplot2",
  "RColorBrewer" #,
  # "reshape2",
  # "akima"
)

#--- Install Missing Packages ----
LoadPackages <- function() {
  # Find missing packages
  newpackages <- packages[!(packages %in% installed.packages()[,"Package"])]
  # Install missing packages if need be
  if (length(newpackages))
    install.packages(newpackages)
  # Load packages
  temp <- lapply(packages, require, character.only = TRUE)
}


#--- ggplot Theme ----
# theme_set(theme_linedraw())
theme_set(theme_bw())
options(scipen = 10)