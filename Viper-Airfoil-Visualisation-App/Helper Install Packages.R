#----------------------------
#--- Install Required Packages
#============================

## Install Packages ======================================================================
# install.packages(c(
#   "shiny",
#   "shinyAce",
#   "rsconnect",
#   "MASS",
#   "lazyeval",
#   "tidyr",
#   "dplyr",
#   "purrr",
#   "ggplot2",
#   "RColorBrewer",
#   "reshape2"
# ))

## Load Packages ======================================================================
library(shiny)
library(shinyAce)
library(rsconnect)
library(MASS)
library(lazyeval)
library(tidyr)
library(dplyr)
library(purrr)
library(ggplot2)
library(RColorBrewer)
library(reshape2)
library(directlabels)

# theme_set(theme_linedraw())
theme_set(theme_bw())
options(scipen = 10)