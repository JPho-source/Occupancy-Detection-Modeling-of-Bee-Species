# clear all objects from the current R environment
rm(list=ls())

# packages
library(dplyr)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(stringr)

# list of bee species used for occupancy modeling
tribes <- c('Bombus griseocollis', 'Bombus impatiens', 'Halictus ligatus', 'Xylocopa virginica', 'Bombus bimaculatus', 
            'Agapostemon virescens', 'Melissodes bimaculatus', 'Anthidium manicatum', 'Bombus auricomus', 
            'Bombus pensylvanicus', 'Anthidium oblongatum', 'Ptilothrix bombiformis', 'Hylaeus leptocephalus', 
            'Halictus confusus', 'Halictus rubicundus', 'Hylaeus modestus', 'Triepeolus lunatus',  'Xenoglossa pruinosa', 
            'Calliopsis andreniformis', 'Megachile xylocopoides', 'Melitoma taurea', 'Anthophora abrupta')

# import data sets
model1_filtered <- read.csv("model1_summary.csv", row.names = 1)
functional_traits <-	read.csv("functional_traits_4Archive.csv")

# orders functional trait species data frame by tribes 
functional_traits <- functional_traits %>% mutate(Species = factor(Species, levels = tribes)) %>% arrange(Species)

names(functional_traits)
dput(names(functional_traits))

# subset of predictors
PC1 <- data.frame()
habitatComplexity  <- data.frame()
gardenBedArea <- data.frame()
vegetativeDensity <- data.frame()
SppRichness <- data.frame()

for (i in 1:length(tribes)) {
  tempdf <- model1_filtered[1:6, ]
  PC1 <- rbind(PC1, tempdf[2, ])
  gardenBedArea <- rbind(gardenBedArea, tempdf[3, ])
  habitatComplexity <- rbind(habitatComplexity, tempdf[4, ])
  vegetativeDensity <- rbind(vegetativeDensity, tempdf[5, ])
  SppRichness <- rbind(SppRichness, tempdf[6, ])
  model1_filtered <- model1_filtered[-(1:6),]
}

# combines covariates influences on species with their respected functional traits
PC1 <- merge(functional_traits, PC1, by.x = "Species", by.y = "tribe")
habitatComplexity <- merge(functional_traits, habitatComplexity, by.x = "Species", by.y = "tribe")
gardenBedArea <- merge(functional_traits, gardenBedArea, by.x = "Species", by.y = "tribe")
vegetativeDensity <- merge(functional_traits, vegetativeDensity, by.x = "Species", by.y = "tribe")
SppRichness <- merge(functional_traits, SppRichness, by.x = "Species", by.y = "tribe")

# sets violin and box plot line width 
vlw <- 1
blw <- 1.25

#############
## Nesting ##
#############

# sets theme of plots
mytheme <- theme(
  plot.title = element_text(hjust = 0.5, vjust = 2, size = 33),
  axis.title.x = element_text(color = "black", size = 33, margin = margin(t = 10)),
  axis.title.y = element_text(color = "black", size = 33, margin = margin(r = 10)),
  axis.text.y = element_text(color = "black", size = 25),
  axis.text.x = element_text(color = "black", size = 28),
  panel.background = element_rect(fill = "white", color = NA),
  plot.background  = element_rect(fill = "white", color = NA),
  panel.grid   = element_blank(),
  panel.border = element_blank(),
  axis.line    = element_line(color = "black"),
  axis.ticks.x = element_line(color = "black"),
  axis.ticks.y = element_line(color = "black"),
  plot.margin = margin(10, 4.5, 10, 4.5)
)

nest_Urb <- ggplot(PC1, aes(x = Nesting, y = mean)) +
  geom_violin(aes(fill = Nesting), trim = FALSE, color = "black", linewidth = vlw, show.legend = FALSE) +
  geom_boxplot(aes(fill = Nesting), width = 0.08, color = "black", outlier.shape = NA, linewidth = blw, show.legend = FALSE) +
  geom_jitter(color = "black", width = 0.1, size = 3, alpha = 0.7, show.legend = FALSE) +
  scale_fill_manual(values = c("Above Ground" = "#B2DF8A", "Below Ground" = "#B2DF8A")) +
  labs(title = "Urbanization", x = NULL, y = "Mean Coefficient") +
  mytheme +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_text(aes(x = 0.47, y = Inf, label = "a"), size = 13, hjust = 0, vjust = 1.2, color = "black")


nest_HC <- ggplot(habitatComplexity, aes(x = Nesting, y = mean)) +
  geom_violin(aes(fill = Nesting), trim = FALSE, color = "black", linewidth = vlw, show.legend = FALSE) +
  geom_boxplot(aes(fill = Nesting), width = 0.08, color = "black", outlier.shape = NA, linewidth = blw, show.legend = FALSE) +
  geom_jitter(color = "black", width = 0.1, size = 3, alpha = 0.7, show.legend = FALSE) +
  scale_fill_manual(values = c("Above Ground" = "#B2DF8A", "Below Ground" = "#B2DF8A")) +
  labs(title = "Habitat Complexity", x = NULL, y = "Mean Coefficient") +
  mytheme +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_text(aes(x = 0.47, y = Inf, label = "b"), size = 13, hjust = 0, vjust = 1.2, color = "black")


nest_VD <- ggplot(vegetativeDensity , aes(x = Nesting, y = mean)) +
  geom_violin(aes(fill = Nesting), trim = FALSE, color = "black", linewidth = vlw, show.legend = FALSE) +
  geom_boxplot(aes(fill = Nesting), width = 0.08, color = "black", outlier.shape = NA, linewidth = blw, show.legend = FALSE) +
  geom_jitter(color = "black", width = 0.1, size = 3, alpha = 0.7, show.legend = FALSE) +
  scale_fill_manual(values = c("Above Ground" = "#B2DF8A", "Below Ground" = "#B2DF8A")) +
  labs(title = "Vegatation Density", x = NULL, y = "Mean Coefficient") +
  mytheme +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_text(aes(x = 0.47, y = Inf, label = "c"), size = 13, hjust = 0, vjust = 1.2, color = "black")


nest_GBA <- ggplot(gardenBedArea, aes(x = Nesting, y = mean)) +
  geom_violin(aes(fill = Nesting), trim = FALSE, color = "black", linewidth = vlw, show.legend = FALSE) +
  geom_boxplot(aes(fill = Nesting), width = 0.08, color = "black", outlier.shape = NA, linewidth = blw, show.legend = FALSE) +
  geom_jitter(color = "black", width = 0.1, size = 3, alpha = 0.7, show.legend = FALSE) +
  scale_fill_manual(values = c("Above Ground" = "#B2DF8A", "Below Ground" = "#B2DF8A")) +
  labs(title = "Garden Bed Area", x = NULL, y = "Mean Coefficient") +
  mytheme +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_text(aes(x = 0.47, y = Inf, label = "d"), size = 13, hjust = 0, vjust = 1.2, color = "black")


nest_FS <- ggplot(SppRichness, aes(x = Nesting, y = mean)) +
  geom_violin(aes(fill = Nesting), trim = FALSE, color = "black", linewidth = vlw, show.legend = FALSE) +
  geom_boxplot(aes(fill = Nesting), width = 0.08, color = "black", outlier.shape = NA, linewidth = blw, show.legend = FALSE) +
  geom_jitter(color = "black", width = 0.1, size = 3, alpha = 0.7, show.legend = FALSE) +
  scale_fill_manual(values = c("Above Ground" = "#B2DF8A", "Below Ground" = "#B2DF8A")) +
  labs(title = "Flower Species Richness", x = NULL, y = "Mean Coefficient") +
  mytheme +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_text(aes(x = 0.47, y = Inf, label = "e"), size = 13, hjust = 0, vjust = 1.2, color = "black")


# T-Test 
HC_t <- t.test(mean ~ Nesting, data = habitatComplexity)
PC1_t <- t.test(mean ~ Nesting, data = PC1)
GBA_t <- t.test(mean ~ Nesting, data = gardenBedArea)
VD_t <- t.test(mean ~ Nesting, data = vegetativeDensity)
FS_t <- t.test(mean ~ Nesting, data = SppRichness)

HC_t
PC1_t
GBA_t
VD_t
FS_t

nest_Urb <- nest_Urb + theme(axis.title.x = element_blank())
nest_HC <- nest_HC + theme(axis.title.x = element_blank(), axis.title.y = element_blank())
nest_VD <- nest_VD + theme(axis.title.x = element_blank(), axis.title.y = element_blank())
nest_GBA <- nest_GBA + theme(axis.title.x = element_blank())
nest_FS <- nest_FS + theme(axis.title.x = element_blank(), axis.title.y = element_blank())

# saves nesting violin plots 
nest_subplots <- grid.arrange(nest_Urb, nest_HC, nest_VD, nest_GBA, nest_FS, nrow = 2, ncol = 3)
ggsave("nest_subplots.png", width = 25, height = 18, plot = nest_subplots, dpi = 600)


###############
## Sociality ##
###############

# sets theme of plots
mytheme <- theme(
  plot.title = element_text(hjust = 0.5, vjust = 2, size = 33),
  axis.title.x = element_text(color = "black", size = 33, margin = margin(t = 10)),
  axis.title.y = element_text(color = "black", size = 33, margin = margin(r = 10)),
  axis.text.y = element_text(color = "black", size = 25),
  axis.text.x = element_text(color = "black", size = 23),
  panel.background = element_rect(fill = "white", color = NA),
  plot.background  = element_rect(fill = "white", color = NA),
  panel.grid   = element_blank(),
  panel.border = element_blank(),
  axis.line    = element_line(color = "black"),
  axis.ticks.x = element_line(color = "black"),
  axis.ticks.y = element_line(color = "black"),
  plot.margin = margin(10, 4.5, 10, 4.5)
)

# order by sociality
levels = c("Social", "Semi-Social", "Solitary", "Kleptoparasitic")
PC1$Sociality <- factor(PC1$Sociality, levels)
gardenBedArea$Sociality <- factor(gardenBedArea$Sociality, levels)
vegetativeDensity$Sociality <- factor(vegetativeDensity$Sociality, levels)
SppRichness$Sociality <- factor(SppRichness$Sociality, levels)
habitatComplexity$Sociality <- factor(habitatComplexity$Sociality, levels)


soc_Urb <- ggplot(PC1, aes(x = factor(Sociality, levels = c("Social", "Semi-Social", "Solitary", "Kleptoparasitic")), y = mean)) +
  geom_violin(aes(fill = Sociality), trim = FALSE, color = "black", linewidth = vlw, show.legend = FALSE) +
  geom_boxplot(aes(fill = Sociality), width = 0.08, color = "black", outlier.shape = NA, linewidth = blw, show.legend = FALSE) +
  geom_jitter(color = "black", width = 0.1, size = 3, alpha = 0.7, show.legend = FALSE) +
  scale_fill_manual(values = c("Social" = "#FDBF6F", "Semi-Social" = "#FDBF6F", "Solitary" = "#FDBF6F","Kleptoparasitic" = "#FDBF6F")) +
  labs(title = "Urbanization", x = NULL, y = "Mean Coefficient") +
  mytheme +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_text(aes(x = 0.47, y = Inf, label = "a"), size = 13, hjust = 0, vjust = 1.2, color = "black")


soc_HC <- ggplot(habitatComplexity,aes(x = factor(Sociality, levels = c("Social", "Semi-Social", "Solitary", "Kleptoparasitic")), y = mean)) +
  geom_violin(aes(fill = Sociality), trim = FALSE, color = "black", linewidth = vlw, show.legend = FALSE) +
  geom_boxplot(aes(fill = Sociality), width = 0.08, color = "black", outlier.shape = NA, linewidth = blw, show.legend = FALSE) +
  geom_jitter(color = "black", width = 0.1, size = 3, alpha = 0.7, show.legend = FALSE) +
  scale_fill_manual(values = c("Social" = "#FDBF6F", "Semi-Social" = "#FDBF6F", "Solitary" = "#FDBF6F","Kleptoparasitic" = "#FDBF6F")) +
  labs(title = "Habitat Complexity", x = NULL, y = NULL) +
  mytheme +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_text(aes(x = 0.47, y = Inf, label = "b"), size = 13, hjust = 0, vjust = 1.2, color = "black")


soc_VD <- ggplot(vegetativeDensity, aes(x = factor(Sociality, levels = c("Social", "Semi-Social", "Solitary", "Kleptoparasitic")), y = mean)) +
  geom_violin(aes(fill = Sociality), trim = FALSE, color = "black", linewidth = vlw, show.legend = FALSE) +
  geom_boxplot(aes(fill = Sociality), width = 0.08, color = "black", outlier.shape = NA, linewidth = blw, show.legend = FALSE) +
  geom_jitter(color = "black", width = 0.1, size = 3, alpha = 0.7, show.legend = FALSE) +
  scale_fill_manual(values = c("Social" = "#FDBF6F", "Semi-Social" = "#FDBF6F", "Solitary" = "#FDBF6F","Kleptoparasitic" = "#FDBF6F")) +
  labs(title = "Vegetative Density", x = NULL, y = "Mean Coefficient") +
  mytheme +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_text(aes(x = 0.47, y = Inf, label = "c"), size = 13, hjust = 0, vjust = 1.2, color = "black")


soc_GBA <- ggplot(gardenBedArea, aes(x = factor(Sociality, levels), y = mean)) +
  geom_violin(aes(fill = Sociality), trim = FALSE, color = "black", linewidth = vlw, show.legend = FALSE) +
  geom_boxplot(aes(fill = Sociality), width = 0.08, color = "black", outlier.shape = NA, linewidth = blw, show.legend = FALSE) +
  geom_jitter(color = "black", width = 0.1, size = 3, alpha = 0.7, show.legend = FALSE) +
  scale_fill_manual(values = c("Social" = "#FDBF6F", "Semi-Social" = "#FDBF6F", "Solitary" = "#FDBF6F", "Kleptoparasitic" = "#FDBF6F")) +
  labs(title = "Garden Bed Area", x = NULL, y = "Mean Coefficient") +
  mytheme +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_text(aes(x = 0.47, y = Inf, label = "d"), size = 13, hjust = 0, vjust = 1.2, color = "black")


soc_FS <- ggplot(SppRichness, aes(x = factor(Sociality, levels), y = mean)) +
  geom_violin(aes(fill = Sociality), trim = FALSE, color = "black", linewidth = vlw, show.legend = FALSE) +
  geom_boxplot(aes(fill = Sociality), width = 0.08, color = "black", outlier.shape = NA, linewidth = blw, show.legend = FALSE) +
  geom_jitter(color = "black", width = 0.1, size = 3, alpha = 0.7, show.legend = FALSE) +
  scale_fill_manual(values = c("Social" = "#FDBF6F", "Semi-Social" = "#FDBF6F", "Solitary" = "#FDBF6F", "Kleptoparasitic" = "#FDBF6F")) +
  labs(title = "Flower Species Richness", x = NULL, y = "Mean Coefficient") +
  mytheme +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_text(aes(x = 0.47, y = Inf, label = "e"), size = 13, hjust = 0, vjust = 1.2, color = "black")


# T-Test 
HC_t <- pairwise.t.test(habitatComplexity$mean, habitatComplexity$Sociality, p.adjust.method = "BH")
PC1_t <- pairwise.t.test(PC1$mean, PC1$Sociality, p.adjust.method = "BH")
GBA_t <- pairwise.t.test(gardenBedArea$mean, gardenBedArea$Sociality, p.adjust.method = "BH")
VD_t <- pairwise.t.test(vegetativeDensity$mean,  vegetativeDensity$Sociality, p.adjust.method = "BH")
FS_t <- pairwise.t.test(SppRichness$mean, SppRichness$Sociality, p.adjust.method = "BH")

HC_t
PC1_t
GBA_t
VD_t
FS_t

soc_Urb <- soc_Urb + theme(axis.title.x = element_blank())
soc_HC <- soc_HC + theme(axis.title.x = element_blank(), axis.title.y = element_blank())
soc_VD <- soc_VD + theme(axis.title.x = element_blank(), axis.title.y = element_blank())
soc_GBA <- soc_GBA + theme(axis.title.x = element_blank())
soc_FS <- soc_FS + theme(axis.title.x = element_blank(), axis.title.y = element_blank())

# saves sociality violin plots 
soc_subplots <- grid.arrange(soc_Urb, soc_HC, soc_VD, soc_GBA, soc_FS, nrow = 2, ncol = 3)
ggsave("soc_subplots.png", width = 25, height = 18, plot = soc_subplots, dpi = 600)


###################
## Native/Origin ##
###################
# sets theme of plots
mytheme <- theme(
  plot.title = element_text(hjust = 0.5, vjust = 2, size = 33),
  axis.title.x = element_text(color = "black", size = 33, margin = margin(t = 10)),
  axis.title.y = element_text(color = "black", size = 33, margin = margin(r = 10)),
  axis.text.y = element_text(color = "black", size = 25),
  axis.text.x = element_text(color = "black", size = 28),
  panel.background = element_rect(fill = "white", color = NA),
  plot.background  = element_rect(fill = "white", color = NA),
  panel.grid   = element_blank(),
  panel.border = element_blank(),
  axis.line    = element_line(color = "black"),
  axis.ticks.x = element_line(color = "black"),
  axis.ticks.y = element_line(color = "black"),
  plot.margin = margin(10, 4.5, 10, 4.5)
)

nat_Urb <- ggplot(PC1, aes(x = Native, y = mean)) +
  geom_violin(aes(fill = Native), trim = FALSE, color = "black", linewidth = vlw, show.legend = FALSE) +
  geom_boxplot(aes(fill = Native), width = 0.08, color = "black", outlier.shape = NA, linewidth = blw, show.legend = FALSE) +
  geom_jitter(color = "black", width = 0.1, size = 3, alpha = 0.7, show.legend = FALSE) +
  scale_fill_manual(values = c("Native" = "#A6CEE3", "Exotic" = "#A6CEE3")) +
  labs(title = "Urbanization", x = NULL, y = "Mean Coefficient") +
  mytheme +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_text(aes(x = 0.47, y = Inf, label = "a"), size = 13, hjust = 0, vjust = 1.2, color = "black")


nat_HC <- ggplot(habitatComplexity, aes(x = Native, y = mean)) +
  geom_violin(aes(fill = Native), trim = FALSE, color = "black", linewidth = vlw, show.legend = FALSE) +
  geom_boxplot(aes(fill = Native), width = 0.08, color = "black", outlier.shape = NA, linewidth = 1.25, show.legend = FALSE) +
  geom_jitter(color = "black", width = 0.1, size = 3, alpha = 0.7, show.legend = FALSE) +
  scale_fill_manual(values = c("Native" = "#A6CEE3", "Exotic" = "#A6CEE3")) +
  labs(title = "Habitat Complexity", x = NULL, y = "Mean Coefficient") +
  mytheme +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_text(aes(x = 0.47, y = Inf, label = "b"), size = 13, hjust = 0, vjust = 1.2, color = "black")


nat_VD <- ggplot(vegetativeDensity , aes(x = Native, y = mean)) +
  geom_violin(aes(fill = Native), trim = FALSE, color = "black", linewidth = vlw, show.legend = FALSE) +
  geom_boxplot(aes(fill = Native), width = 0.08, color = "black", outlier.shape = NA, linewidth = blw, show.legend = FALSE) +
  geom_jitter(color = "black", width = 0.1, size = 3, alpha = 0.7, show.legend = FALSE) +
  scale_fill_manual(values = c("Native" = "#A6CEE3", "Exotic" = "#A6CEE3")) +
  labs(title = "Vegetative Density", x = NULL, y = "Mean Coefficient") +
  mytheme +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_text(aes(x = 0.47, y = Inf, label = "c"), size = 13, hjust = 0, vjust = 1.2, color = "black")


nat_GBA<- ggplot(gardenBedArea , aes(x = Native, y = mean)) +
  geom_violin(aes(fill = Native), trim = FALSE, color = "black", linewidth = vlw, show.legend = FALSE) +
  geom_boxplot(aes(fill = Native), width = 0.08, color = "black", outlier.shape = NA, linewidth = blw, show.legend = FALSE) +
  geom_jitter(color = "black", width = 0.1, size = 3, alpha = 0.7, show.legend = FALSE) +
  scale_fill_manual(values = c("Native" = "#A6CEE3", "Exotic" = "#A6CEE3")) +
  labs(title = "Garden Bed Area", x = NULL, y = "Mean Coefficient") +
  mytheme +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_text(aes(x = 0.47, y = Inf, label = "d"), size = 13, hjust = 0, vjust = 1.2, color = "black")


nat_FS <- ggplot(SppRichness, aes(x = Native, y = mean)) +
  geom_violin(aes(fill = Native), trim = FALSE, color = "black", linewidth = vlw, show.legend = FALSE) +
  geom_boxplot(aes(fill = Native), width = 0.08, color = "black", outlier.shape = NA, linewidth = blw, show.legend = FALSE) +
  geom_jitter(color = "black", width = 0.1, size = 3, alpha = 0.7, show.legend = FALSE) +
  scale_fill_manual(values = c("Native" = "#A6CEE3", "Exotic" = "#A6CEE3")) +
  labs(title = "Flower Species Richness", x = NULL, y = "Mean Coefficient") +
  mytheme +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_text(aes(x = 0.47, y = Inf, label = "e"), size = 13, hjust = 0, vjust = 1.2, color = "black")


# T-Test
HC_t <- t.test(mean ~ Native, data = habitatComplexity)
PC1_t <- t.test(mean ~ Native, data = PC1)
GBA_t <- t.test(mean ~ Native, data = gardenBedArea)
VD_t <- t.test(mean ~ Native, data = vegetativeDensity)
FS_t <- t.test(mean ~ Native, data = SppRichness)

HC_t
PC1_t
GBA_t
VD_t
FS_t

nat_Urb <- nat_Urb + theme(axis.title.x = element_blank())
nat_HC <- nat_HC + theme(axis.title.x = element_blank(), axis.title.y = element_blank())
nat_VD <- nat_VD + theme(axis.title.x = element_blank(), axis.title.y = element_blank())
nat_GBA <- nat_GBA + theme(axis.title.x = element_blank())
nat_FS <- nat_FS + theme(axis.title.x = element_blank(), axis.title.y = element_blank())

# saves native violin plots 
nat_subplots <- grid.arrange(nat_Urb, nat_HC, nat_VD, nat_GBA, nat_FS, nrow = 2, ncol = 3)
ggsave("nat_subplots.png", width = 25, height = 18, plot = nat_subplots, dpi = 600)
