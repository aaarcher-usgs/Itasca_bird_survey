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

#' ## Test by species
chickadee <- lm(Count~ Year, data = counts.long[counts.long$Species=="Black-capped Chickadee",])
summary(chickadee)

black.th.gr.warbler <- lm(Count~ Year, data = counts.long[counts.long$Species=="Black Throated Green Warbler",])
summary(black.th.gr.warbler)

blue.jay <- lm(Count~Year, data = counts.long[counts.long$Species=="Blue Jay",])
summary(blue.jay)

brown.creeper <- lm(Count~Year, data = counts.long[counts.long$Species=="Brown Creeper",])
summary(brown.creeper)

canada.warbler <- lm(Count~Year, data = counts.long[counts.long$Species=="Canada Warbler",])
summary(canada.warbler)

chestnut.warbler <- lm(Count~ Year, data = counts.long[counts.long$Species=="Chestnut-sided Warbler",])
summary(chestnut.warbler)

yellowthroat <- lm(Count ~ Year, data = counts.long[counts.long$Species =="Common Yellowthroat",])
summary(yellowthroat)

wood.pewee <- lm(Count ~ Year, data = counts.long[counts.long$Species == "Eastern Wood Pewee",])
summary(wood.pewee)

flycatcher <- lm(Count ~ Year, data = counts.long[counts.long$Species == "Least Flycatcher",])
summary(flycatcher)

hairy.woodpecker <- lm(Count ~ Year, data = counts.long[counts.long$Species == "Hairy Woodpecker",])
summary(hairy.woodpecker)


hermit.thrush <- lm(Count ~ Year, data = counts.long[counts.long$Species == "Hermit Thrush",])
summary(hermit.thrush)

mourning.warbler <- lm(Count ~ Year, data = counts.long[counts.long$Species == "Mourning Warbler",])
summary(mourning.warbler)

nashville.warbler <- lm(Count ~ Year, data = counts.long[counts.long$Species == "Nashville Warbler",])
summary(nashville.warbler)

parula <- lm(Count ~ Year, data = counts.long[counts.long$Species == "Northern Parula",])
summary(parula)

ovenbird <- lm(Count ~ Year, data = counts.long[counts.long$Species == "Ovenbird",])
summary(ovenbird)

pine.warbler <- lm(Count ~ Year, data = counts.long[counts.long$Species == "Pine Warbler",])
summary(pine.warbler)

red.breast.nuthatch <- lm(Count ~ Year, data = counts.long[counts.long$Species == "Red-Breasted Nuthatch",])
summary(red.breast.nuthatch)

red.vireo <- lm(Count ~ Year, data = counts.long[counts.long$Species == "Red-eyed Vireo",])
summary(red.vireo)

ruffed.grouse <- lm(Count ~ Year, data = counts.long[counts.long$Species == "Ruffed Grouse",])
summary(ruffed.grouse)

scarlet.tanager <- lm(Count ~ Year, data = counts.long[counts.long$Species == "Scarlet Tanager",])
summary(scarlet.tanager)

winter.wren <- lm(Count ~ Year, data = counts.long[counts.long$Species == "Winter Wren",])
summary(winter.wren)

yellow.b.sapsucker <- lm(Count ~ Year, data = counts.long[counts.long$Species == "Yellow-bellied Sapsucker",])
summary(yellow.b.sapsucker)
