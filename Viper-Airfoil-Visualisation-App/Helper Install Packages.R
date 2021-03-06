#----------------------------
#--- Install Required Packages
#============================

#--- Install Packages ----
# install.packages(c(
#   "shiny",
#   "shinyAce",
#   "rsconnect",
#   "MASS",
#   "lazyeval",
#   "gridExtra",
#   "tidyr",
#   "dplyr",
#   "purrr",
#   "pbapply",
#   "ggplot2",
#   "RColorBrewer",
#   "reshape2",
#   "akima"
# ))

#--- Load Packages ----
library(shiny)
library(shinyAce)
library(rsconnect)
library(MASS)
library(lazyeval)
library(gridExtra)
library(tidyr)
library(dplyr)
library(purrr)
library(pbapply)
library(ggplot2)
library(RColorBrewer)
library(reshape2)
library(akima)

# theme_set(theme_linedraw())
theme_set(theme_bw())
options(scipen = 10)