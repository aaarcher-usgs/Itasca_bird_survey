#' # Data processing for Itasca Breeding Bird survey
#' 
#' This program takes the raw data for the Itasca Breeding Bird survey and prepares it
#' for analysis. 
#' 
#' ## Preamble
#+ libraries, warnings = F
library(readxl)

remove(list=ls())

#' ## Load data
#' 
bird.Counts <- read_xlsx(path = "data/raw_data/Bird numbers_papers.xlsx", sheet = "Sheet1")

#' ## Process data
#' 
#' Remove extra data at end
clean.bird.counts <- bird.Counts[!is.na(bird.Counts$Species),]

#' More R-friendly column names:
#' 
colnames(clean.bird.counts) <- c("Species",
                                 "min.2018",
                                 "max.2018",
                                 "count.res.2018",
                                 "half.res.2018",
                                 "transient.2018",
                                 "count.1980.mills",
                                 "count.1990.Whittaker",
                                 "count.1990.sladek",
                                 "count.1991.pondeszwa",
                                 "count.1992.reuvers",
                                 "count.1997")
colnames(clean.bird.counts)

#' Convert numerical columns to number
str(clean.bird.counts)
clean.bird.counts$min.2018 <- as.numeric(clean.bird.counts$max.2018)
clean.bird.counts$count.1990.Whittaker <- 
  ifelse(clean.bird.counts$count.1990.Whittaker=="1T", yes = 0.1,
         no = ifelse(clean.bird.counts$count.1990.Whittaker=="2T", yes = 0.2,
         no = as.numeric(clean.bird.counts$count.1990.Whittaker)))
table(clean.bird.counts$count.1990.Whittaker)
table(clean.bird.counts$count.1991.pondeszwa)
clean.bird.counts$count.1991.pondeszwa <- ifelse(
  clean.bird.counts$count.1991.pondeszwa == "T", 
  yes = 0.1, 
  no = as.numeric(clean.bird.counts$count.1991.pondeszwa))
table(clean.bird.counts$count.1991.pondeszwa)
table(clean.bird.counts$count.1992.reuvers)
clean.bird.counts$count.1992.reuvers <- ifelse(
  clean.bird.counts$count.1992.reuvers == "T",
  yes = 0.1,
  no = as.numeric(clean.bird.counts$count.1992.reuvers))
table(clean.bird.counts$count.1992.reuvers)
table(clean.bird.counts$count.1997)
clean.bird.counts$count.1997 <- ifelse(
  clean.bird.counts$count.1997=="T",
  yes = 0.1,
  no = ifelse(
    clean.bird.counts$count.1997=="T... Questionable", yes = 0.0, 
    no = as.numeric(clean.bird.counts$count.1997)))
table(clean.bird.counts$count.1997)

#' Combine data into long format for analysis
#'
#' There are 54 bird species, 
#' with studies conducted in 1980, 1990, 1991, 1992, 1997, 2018
#'
counts.long <- as.data.frame(matrix(NA, nrow = 324, ncol = 3))
colnames(counts.long) <- c("Species", "Year", "Count")
counts.long$Species <- clean.bird.counts$Species
counts.long$Year <- rep(c(1980, 1990, 1991, 1992, 1997, 2018), each = 54)
counts.long$Count[counts.long$Year==1980] <- clean.bird.counts$count.1980.mills
counts.long$Count[counts.long$Year==1990] <- clean.bird.counts$count.1990.Whittaker
counts.long$Count[counts.long$Year==1991] <- clean.bird.counts$count.1991.pondeszwa
counts.long$Count[counts.long$Year==1992] <- clean.bird.counts$count.1992.reuvers
counts.long$Count[counts.long$Year==1997] <- clean.bird.counts$count.1997
counts.long$Count[counts.long$Year==2018] <- (clean.bird.counts$min.2018 + 
                                                clean.bird.counts$max.2018)/2

#' ## Save data for analysis
#' 
#' Bird counts by year and species
save(counts.long, file = "data/processed_data/bird_counts_metaanalysis.R")
