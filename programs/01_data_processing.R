#' # Data processing for Itasca Breeding Bird survey
#' 
#' This program takes the raw data for the Itasca Breeding Bird survey and prepares it
#' for analysis. 
#' 
#' ## Preamble
#+ libraries, warnings = F
library(readxl)

#' ## Load data
#' 
bird.Counts <- read_xlsx(path = "data/raw_data/Bird numbers_papers.xlsx", sheet = "Sheet1")
