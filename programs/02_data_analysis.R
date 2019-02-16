#' # Data analysis for Itasca Breeding Bird survey
#' 
#' This program analyzes the Itasca Breeding Bird survey data. 
#' 
#' ## Preamble
#+ libraries, warnings = F
library(ggplot2)
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

#' ## Plotting results
#' 
#' 
#' Black-throated green warbler
ggplot(black.th.gr.warbler$model, 
       aes_string(x = names(black.th.gr.warbler$model)[2],
                  y = names(black.th.gr.warbler$model)[1]))+
  geom_point()+
  stat_smooth(method = "lm", col = "black")+
  labs(title = paste0("Black-throated Green Warbler, ",
                      "Adj R2 = ", round(signif(summary(black.th.gr.warbler)$adj.r.squared),digits = 2),
                      "; p = ", round(signif(summary(black.th.gr.warbler)$coef[2,4],5), digits = 4)))+
  theme_classic()
#ggsave(filename = "blackThroatedGreenWarbler.png", device = "png", path = "presentations/2019_TWS/trend_plots")



#' Blue jay
ggplot(blue.jay$model, 
       aes_string(x = names(blue.jay$model)[2],
                  y = names(blue.jay$model)[1]))+
  geom_point()+
  stat_smooth(method = "lm", col = "black")+
  labs(title = paste0("Blue Jay, ",
                      "Adj R2 = ", round(signif(summary(blue.jay)$adj.r.squared),digits = 2),
                      "; p = ", round(signif(summary(blue.jay)$coef[2,4],5), digits = 4)))+
  theme_classic()
#ggsave(filename = "blueJay.png", device = "png", path = "presentations/2019_TWS/trend_plots")

#' Chestnut-sided Warbler
ggplot(chestnut.warbler$model, 
       aes_string(x = names(chestnut.warbler$model)[2],
                  y = names(chestnut.warbler$model)[1]))+
  geom_point()+
  stat_smooth(method = "lm", col = "black")+
  labs(title = paste0("Chestnut-sided Warbler, ",
                      "Adj R2 = ", round(signif(summary(chestnut.warbler)$adj.r.squared),digits = 2),
                      "; p = ", round(signif(summary(chestnut.warbler)$coef[2,4],5), digits = 4)))+
  theme_classic()
#ggsave(filename = "chestnutSidedWarbler.png", device = "png", path = "presentations/2019_TWS/trend_plots")

#' Eastern wood pewee
ggplot(wood.pewee$model, 
       aes_string(x = names(wood.pewee$model)[2],
                  y = names(wood.pewee$model)[1]))+
  geom_point()+
  stat_smooth(method = "lm", col = "black")+
  labs(title = paste0("Eastern Wood Pewee, ",
                      "Adj R2 = ", round(signif(summary(wood.pewee)$adj.r.squared),digits = 2),
                      "; p = ", round(signif(summary(wood.pewee)$coef[2,4],5), digits = 4)))+
  theme_classic()
#ggsave(filename = "easternWoodPewee.png", device = "png", path = "presentations/2019_TWS/trend_plots")

#' Ovenbird
ggplot(ovenbird$model, 
       aes_string(x = names(ovenbird$model)[2],
                  y = names(ovenbird$model)[1]))+
  geom_point()+
  stat_smooth(method = "lm", col = "black")+
  labs(title = paste0("Ovenbird, ",
                      "Adj R2 = ", round(signif(summary(ovenbird)$adj.r.squared),digits = 2),
                      "; p = ", round(signif(summary(ovenbird)$coef[2,4],5), digits = 4)))+
  theme_classic()
#ggsave(filename = "ovenbird.png", device = "png", path = "presentations/2019_TWS/trend_plots")

#' Red-breasted Nuthatch
ggplot(red.breast.nuthatch$model, 
       aes_string(x = names(red.breast.nuthatch$model)[2],
                  y = names(red.breast.nuthatch$model)[1]))+
  geom_point()+
  stat_smooth(method = "lm", col = "black")+
  labs(title = paste0("Red-breasted Nuthatch, ",
                      "Adj R2 = ", round(signif(summary(red.breast.nuthatch)$adj.r.squared),digits = 2),
                      "; p = ", round(signif(summary(red.breast.nuthatch)$coef[2,4],5), digits = 4)))+
  theme_classic()
#ggsave(filename = "redBreastedNuthatch.png", device = "png", path = "presentations/2019_TWS/trend_plots")

#' Red-eyed Vireo
ggplot(red.vireo$model, 
       aes_string(x = names(red.vireo$model)[2],
                  y = names(red.vireo$model)[1]))+
  geom_point()+
  stat_smooth(method = "lm", col = "black")+
  labs(title = paste0("Red-eyed Vireo, ",
                      "Adj R2 = ", round(signif(summary(red.vireo)$adj.r.squared),digits = 2),
                      "; p = ", round(signif(summary(red.vireo)$coef[2,4],5), digits = 4)))+
  theme_classic()
#ggsave(filename = "redEyedVireo.png", device = "png", path = "presentations/2019_TWS/trend_plots")

#' Ruffed Grouse
ggplot(ruffed.grouse$model, 
       aes_string(x = names(ruffed.grouse$model)[2],
                  y = names(ruffed.grouse$model)[1]))+
  geom_point()+
  stat_smooth(method = "lm", col = "black")+
  labs(title = paste0("Red-eyed Vireo, ",
                      "Adj R2 = ", round(signif(summary(ruffed.grouse)$adj.r.squared),digits = 2),
                      "; p = ", round(signif(summary(ruffed.grouse)$coef[2,4],5), digits = 4)))+
  theme_classic()
#ggsave(filename = "ruffedGrouse.png", device = "png", path = "presentations/2019_TWS/trend_plots")

#' Scarlet Tanager
ggplot(scarlet.tanager$model, 
       aes_string(x = names(scarlet.tanager$model)[2],
                  y = names(scarlet.tanager$model)[1]))+
  geom_point()+
  stat_smooth(method = "lm", col = "black")+
  labs(title = paste0("Scarlet Tanager, ",
                      "Adj R2 = ", round(signif(summary(scarlet.tanager)$adj.r.squared),digits = 2),
                      "; p = ", round(signif(summary(scarlet.tanager)$coef[2,4],5), digits = 4)))+
  theme_classic()
#ggsave(filename = "scarletTanager.png", device = "png", path = "presentations/2019_TWS/trend_plots")
