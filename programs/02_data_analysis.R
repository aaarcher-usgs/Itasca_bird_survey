#' # Data analysis for Itasca Breeding Bird survey
#' 
#' This program analyzes the Itasca Breeding Bird survey data. 
#' 
#' ## Preamble
#+ libraries, warnings = F

remove(list=ls())

#' ## Load data
#' 
load(file = "data/processed_data/bird_counts_metaanalysis.R")

#' ## Regression analysis with bird specific intercepts and slopes
#' 
full.model <- lm(Count~ Year*Species, data = counts.long)
summary(full.model)


