#---------------------------->
#--- Install Required Packages
#============================>

#--- Packages to Install ----
packages <- c(
  "parallel",
  "gridExtra",
  "tidyr",
  "dplyr",
  "purrr",
  "pbapply",
  "ggplot2",
  "RColorBrewer",
  "akima"
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

LoadPackages()
