library(dplyr)
library(stringr)
library(ggplot2)
library(plotly)
library(shinycssloaders)
library(formattable)
library(markdown)
library(overlapping)

source("www/functions.R")
source("www/gameStatPlots.R")
source("www/EloFunctions.R")
source("www/OptimizeSingleFactors.R")
source("www/calibrationFunctions.R")
source("www/OptimizeTwoFactors.R")
source("www/4FactorOptimization.R")
source("www/PredictGamesFunctions.R")

options(shiny.sanitize.errors = FALSE)

#global variables
startingElo <- 1400
EloDenom <- 400
MOVinteger <- 1
