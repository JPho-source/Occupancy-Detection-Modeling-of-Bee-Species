# clear all objects from the current environment
rm(list=ls())

# packages
library(ggplot2)
library(metafor)
library(dplyr)
library(tidyverse)
library(gridExtra)

# list of bee species used for occupancy modeling
tribes <- c('Bombus griseocollis', 'Bombus impatiens', 'Halictus ligatus', 'Xylocopa virginica', 'Bombus bimaculatus', 
            'Agapostemon virescens', 'Melissodes bimaculatus', 'Anthidium manicatum', 'Bombus auricomus', 'Bombus pensylvanicus', 
            'Anthidium oblongatum', 'Ptilothrix bombiformis', 'Hylaeus leptocephalus', 'Halictus confusus', 'Halictus rubicundus', 
            'Hylaeus modestus', 'Triepeolus lunatus', 'Xenoglossa pruinosa', 'Calliopsis andreniformis', 'Megachile xylocopoides', 
            'Melitoma taurea', 'Anthophora abrupta')

# import data sets
model1_filtered <- read.csv("model1_summary.csv", row.names = 1)
functional_traits <-	read.csv("functional_traits_4Archive.csv")

# orders functional trait species data frame by tribes 
functional_traits <- functional_traits %>%
  mutate(Species = factor(Species, levels = tribes)) %>%
  arrange(Species)

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

# orders species by sociality 
sociality_order <- c("Social", "Semi-Social", "Solitary", "Kleptoparasitic")
PC1_filtered <- PC1 %>% mutate(Sociality = factor(Sociality, levels = sociality_order)) %>% arrange(Sociality, Species)
habitatComplexity_filtered <- habitatComplexity %>% mutate(Sociality = factor(Sociality, levels = sociality_order)) %>% arrange(Sociality, Species)
gardenBedArea_filtered <- gardenBedArea %>% mutate(Sociality = factor(Sociality, levels = sociality_order)) %>% arrange(Sociality, Species)
vegetativeDensity_filtered <- vegetativeDensity %>% mutate(Sociality = factor(Sociality, levels = sociality_order)) %>% arrange(Sociality, Species)
SppRichness_filtered <- SppRichness %>% mutate(Sociality = factor(Sociality, levels = sociality_order)) %>% arrange(Sociality, Species)

# orders by social group, and within each group orders alphabetically by species name
PC1_filtered <- PC1_filtered %>% mutate(Sociality = factor(Sociality, sociality_order))
PC1_filtered <- PC1_filtered %>% separate(Species, into = c("Genus", "Species"), sep = " ", remove = FALSE)
PC1_filtered <- PC1_filtered %>% arrange(Sociality, Genus, Species)

habitatComplexity_filtered <- habitatComplexity_filtered %>% mutate(Sociality = factor(Sociality, levels = sociality_order))
habitatComplexity_filtered <- habitatComplexity_filtered %>% separate(Species, into = c("Genus", "Species"), sep = " ", remove = FALSE)
habitatComplexity_filtered <- habitatComplexity_filtered %>% arrange(Sociality, Genus, Species)

gardenBedArea_filtered <- gardenBedArea_filtered %>% mutate(Sociality = factor(Sociality, levels = sociality_order))
gardenBedArea_filtered <- gardenBedArea_filtered %>% separate(Species, into = c("Genus", "Species"), sep = " ", remove = FALSE)
gardenBedArea_filtered <- gardenBedArea_filtered %>% arrange(Sociality, Genus, Species)

vegetativeDensity_filtered <- vegetativeDensity_filtered %>% mutate(Sociality = factor(Sociality, levels = sociality_order))
vegetativeDensity_filtered <- vegetativeDensity_filtered %>% separate(Species, into = c("Genus", "Species"), sep = " ", remove = FALSE)
vegetativeDensity_filtered <- vegetativeDensity_filtered %>% arrange(Sociality, Genus, Species)

SppRichness_filtered <- SppRichness_filtered %>% mutate(Sociality = factor(Sociality, levels = sociality_order))
SppRichness_filtered <- SppRichness_filtered %>% separate(Species, into = c("Genus", "Species"), sep = " ", remove = FALSE)
SppRichness_filtered <- SppRichness_filtered %>% arrange(Sociality, Genus, Species)

# fixes the data set column names
datasets <- c("PC1_filtered", "habitatComplexity_filtered", "gardenBedArea_filtered", "vegetativeDensity_filtered", "SppRichness_filtered")

for (d in datasets) {
  if (exists(d)) {
    df <- get(d)
    
    names(df) <- gsub("^X2\\.50\\.$", "X2.5.", names(df))
    names(df) <- gsub("^X97\\.50\\.$", "X97.5.", names(df))
    
    assign(d, df, envir = .GlobalEnv)
  }
}

# re-adds full species name column
PC1_filtered$Species_full <- paste(PC1_filtered$Genus, PC1_filtered$Species)
habitatComplexity_filtered$Species_full <- paste(habitatComplexity_filtered$Genus, habitatComplexity_filtered$Species)
gardenBedArea_filtered$Species_full <- paste(gardenBedArea_filtered$Genus, gardenBedArea_filtered$Species)
vegetativeDensity_filtered$Species_full <- paste(vegetativeDensity_filtered$Genus, vegetativeDensity_filtered$Species)
SppRichness_filtered$Species_full <- paste(SppRichness_filtered$Genus, SppRichness_filtered$Species)

# create a new column to categorize credible intervals (CIs) by color for each covariate
PC1_filtered$color_category <- ifelse(
  !is.na(PC1_filtered$mean) & !is.na(PC1_filtered$X25.) & !is.na(PC1_filtered$X75.) & !is.na(PC1_filtered$X2.5.) & !is.na(PC1_filtered$X97.5.), 
  ifelse((pmin(PC1_filtered$X2.5., PC1_filtered$X97.5.) > 0) | (pmax(PC1_filtered$X2.5., PC1_filtered$X97.5.) < 0), "green4",
         ifelse(((pmin(PC1_filtered$X2.5., PC1_filtered$X97.5.) <= 0) & (pmax(PC1_filtered$X2.5., PC1_filtered$X97.5.) >= 0)) &
                  !((pmin(PC1_filtered$X10., PC1_filtered$X90.) <= 0) & (pmax(PC1_filtered$X10., PC1_filtered$X90.) >= 0)), "yellow3",
                ifelse(((pmin(PC1_filtered$X10., PC1_filtered$X90.) <= 0) & (pmax(PC1_filtered$X10., PC1_filtered$X90.) >= 0)) &
                         !((pmin(PC1_filtered$X25., PC1_filtered$X75.) <= 0) & (pmax(PC1_filtered$X25., PC1_filtered$X75.) >= 0)), "gray60",
                       "gray80"))),
  "gray80"
)

habitatComplexity_filtered$color_category <- ifelse(
  !is.na(habitatComplexity_filtered$mean) & !is.na(habitatComplexity_filtered$X25.) & !is.na(habitatComplexity_filtered$X75.) & !is.na(habitatComplexity_filtered$X2.5.) & !is.na(habitatComplexity_filtered$X97.5.), 
  ifelse((pmin(habitatComplexity_filtered$X2.5., habitatComplexity_filtered$X97.5.) > 0) | (pmax(habitatComplexity_filtered$X2.5., habitatComplexity_filtered$X97.5.) < 0), "green4",
         ifelse(((pmin(habitatComplexity_filtered$X2.5., habitatComplexity_filtered$X97.5.) <= 0) & (pmax(habitatComplexity_filtered$X2.5., habitatComplexity_filtered$X97.5.) >= 0)) &
                  !((pmin(habitatComplexity_filtered$X10., habitatComplexity_filtered$X90.) <= 0) & (pmax(habitatComplexity_filtered$X10., habitatComplexity_filtered$X90.) >= 0)), "yellow3",
                ifelse(((pmin(habitatComplexity_filtered$X10., habitatComplexity_filtered$X90.) <= 0) & (pmax(habitatComplexity_filtered$X10., habitatComplexity_filtered$X90.) >= 0)) &
                         !((pmin(habitatComplexity_filtered$X25., habitatComplexity_filtered$X75.) <= 0) & (pmax(habitatComplexity_filtered$X25., habitatComplexity_filtered$X75.) >= 0)), "gray60",
                       "gray80"))),
  "gray80"
)

gardenBedArea_filtered$color_category <- ifelse(
  !is.na(gardenBedArea_filtered$mean) & !is.na(gardenBedArea_filtered$X25.) & !is.na(gardenBedArea_filtered$X75.) & !is.na(gardenBedArea_filtered$X2.5.) & !is.na(gardenBedArea_filtered$X97.5.), 
  ifelse((pmin(gardenBedArea_filtered$X2.5., gardenBedArea_filtered$X97.5.) > 0) | (pmax(gardenBedArea_filtered$X2.5., gardenBedArea_filtered$X97.5.) < 0), "green4",
         ifelse(((pmin(gardenBedArea_filtered$X2.5., gardenBedArea_filtered$X97.5.) <= 0) & (pmax(gardenBedArea_filtered$X2.5., gardenBedArea_filtered$X97.5.) >= 0)) &
                  !((pmin(gardenBedArea_filtered$X10., gardenBedArea_filtered$X90.) <= 0) & (pmax(gardenBedArea_filtered$X10., gardenBedArea_filtered$X90.) >= 0)), "yellow3",
                ifelse(((pmin(gardenBedArea_filtered$X10., gardenBedArea_filtered$X90.) <= 0) & (pmax(gardenBedArea_filtered$X10., gardenBedArea_filtered$X90.) >= 0)) &
                         !((pmin(gardenBedArea_filtered$X25., gardenBedArea_filtered$X75.) <= 0) & (pmax(gardenBedArea_filtered$X25., gardenBedArea_filtered$X75.) >= 0)), "gray60",
                       "gray80"))),
  "gray80"
)

vegetativeDensity_filtered$color_category <- ifelse(
  !is.na(vegetativeDensity_filtered$mean) & !is.na(vegetativeDensity_filtered$X25.) & !is.na(vegetativeDensity_filtered$X75.) & !is.na(vegetativeDensity_filtered$X2.5.) & !is.na(vegetativeDensity_filtered$X97.5.), 
  ifelse((pmin(vegetativeDensity_filtered$X2.5., vegetativeDensity_filtered$X97.5.) > 0) | (pmax(vegetativeDensity_filtered$X2.5., vegetativeDensity_filtered$X97.5.) < 0), "green4",
         ifelse(((pmin(vegetativeDensity_filtered$X2.5., vegetativeDensity_filtered$X97.5.) <= 0) & (pmax(vegetativeDensity_filtered$X2.5., vegetativeDensity_filtered$X97.5.) >= 0)) &
                  !((pmin(vegetativeDensity_filtered$X10., vegetativeDensity_filtered$X90.) <= 0) & (pmax(vegetativeDensity_filtered$X10., vegetativeDensity_filtered$X90.) >= 0)), "yellow3",
                ifelse(((pmin(vegetativeDensity_filtered$X10., vegetativeDensity_filtered$X90.) <= 0) & (pmax(vegetativeDensity_filtered$X10., vegetativeDensity_filtered$X90.) >= 0)) &
                         !((pmin(vegetativeDensity_filtered$X25., vegetativeDensity_filtered$X75.) <= 0) & (pmax(vegetativeDensity_filtered$X25., vegetativeDensity_filtered$X75.) >= 0)), "gray60",
                       "gray80"))),
  "gray80"
)

SppRichness_filtered$color_category <- ifelse(
  !is.na(SppRichness_filtered$mean) & !is.na(SppRichness_filtered$X25.) & !is.na(SppRichness_filtered$X75.) & !is.na(SppRichness_filtered$X2.5.) & !is.na(SppRichness_filtered$X97.5.), 
  ifelse((pmin(SppRichness_filtered$X2.5., SppRichness_filtered$X97.5.) > 0) | (pmax(SppRichness_filtered$X2.5., SppRichness_filtered$X97.5.) < 0), "green4",
         ifelse(((pmin(SppRichness_filtered$X2.5., SppRichness_filtered$X97.5.) <= 0) & (pmax(SppRichness_filtered$X2.5., SppRichness_filtered$X97.5.) >= 0)) &
                  !((pmin(SppRichness_filtered$X10., SppRichness_filtered$X90.) <= 0) & (pmax(SppRichness_filtered$X10., SppRichness_filtered$X90.) >= 0)), "yellow3",
                ifelse(((pmin(SppRichness_filtered$X10., SppRichness_filtered$X90.) <= 0) & (pmax(SppRichness_filtered$X10., SppRichness_filtered$X90.) >= 0)) &
                         !((pmin(SppRichness_filtered$X25., SppRichness_filtered$X75.) <= 0) & (pmax(SppRichness_filtered$X25., SppRichness_filtered$X75.) >= 0)), "gray60",
                       "gray80"))),
  "gray80"
)


# reverses order for the plots
PC1_filtered$Species_full <- factor(PC1_filtered$Species_full,levels = rev(PC1_filtered$Species_full))
habitatComplexity_filtered$Species_full <- factor(habitatComplexity_filtered$Species_full,levels = rev(habitatComplexity_filtered$Species_full))
gardenBedArea_filtered$Species_full <- factor(gardenBedArea_filtered$Species_full,levels = rev(gardenBedArea_filtered$Species_full))
vegetativeDensity_filtered$Species_full <- factor(vegetativeDensity_filtered$Species_full, levels = rev(vegetativeDensity_filtered$Species_full))
SppRichness_filtered$Species_full <- factor(SppRichness_filtered$Species_full, levels = rev(SppRichness_filtered$Species_full))


# sets theme for plots
mytheme <- theme(
  plot.title = element_text(hjust = 0.5, size = 29),
  axis.text.y = element_text(color = "black", face = "italic", size = 27),
  axis.text.x = element_text(color = "black", size = 25),
  axis.title.x = element_text(size = 27),
  axis.title.y = element_text(size = 27),
  panel.border = element_blank(), 
  axis.line = element_line(),
  axis.ticks.x = element_line(color = "black"),  
  axis.ticks.y = element_line(color = "black"),
  panel.grid = element_blank(),  
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(), plot.margin = margin(10, 60, 10, 10)
)


plot_PC1 <- ggplot(PC1_filtered, aes(x = mean, y = Species_full)) +
  geom_point(aes(color = color_category), size = 3) +  
  geom_segment(aes(x = X25., xend = X75., color = color_category), size = 2) +  
  geom_segment(aes(x = X10., xend = X90., color = color_category), size = 1) +  
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  geom_segment(aes(x = X2.5., xend = X97.5., color = color_category), size = 1, linetype = "dashed") + 
  labs(title = "Urbanization", x = "Posterior Coefficient Value", y = "Species") +  
  scale_color_manual(values = c("gray80" = "gray80", "gray60" = "gray60", "green4" = "green4", "yellow3" = "yellow3"), breaks = c("gray80", "gray60", "green4", "yellow3")) +
  annotate("text", x = -4, y = Inf, label = "a", size = 11.5, hjust = 0, vjust = 1, color = "black") + 
  theme_minimal() + 
  mytheme +
  theme(legend.position = "none") 


plot_GBA <- ggplot(gardenBedArea_filtered, aes(x = mean, y = Species_full)) +
  geom_point(aes(color = color_category), size = 3) +  
  geom_segment(aes(x = X25., xend = X75., color = color_category), size = 2) +  
  geom_segment(aes(x = X10., xend = X90., color = color_category), size = 1) +  
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  geom_segment(aes(x = X2.5., xend = X97.5., color = color_category), size = 1, linetype = "dashed") + 
  labs(title = "Garden Bed Area", x = "Posterior Coefficient Value", y = "Species") +  
  scale_color_manual(values = c("gray80" = "gray80", "gray60" = "gray60", "green4" = "green4", "yellow3" = "yellow3"), breaks = c("gray80", "gray60", "green4", "yellow3")) +
  annotate("text", x = -4.5, y = Inf, label = "", size = 11.5, hjust = 0, vjust = 1, color = "black") + 
  theme_minimal() + 
  mytheme +
  theme(legend.position = "none") 


plot_VD <- ggplot(vegetativeDensity_filtered, aes(x = mean, y = Species_full)) +
  geom_point(aes(color = color_category), size = 3) +  
  geom_segment(aes(x = X25., xend = X75., color = color_category), size = 2) +  
  geom_segment(aes(x = X10., xend = X90., color = color_category), size = 1) +  
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  geom_segment(aes(x = X2.5., xend = X97.5., color = color_category), size = 1, linetype = "dashed") + 
  labs(title = "Vegatation Density", x = "Posterior Coefficient Value", y = "Species") +  
  scale_color_manual(values = c("gray80" = "gray80", "gray60" = "gray60", "green4" = "green4", "yellow3" = "yellow3"), breaks = c("gray80", "gray60", "green4", "yellow3")) +
  annotate("text", x = -5, y = Inf, label = "c", size = 11.5, hjust = 0, vjust = 1, color = "black") + 
  theme_minimal() + 
  mytheme +
  theme(legend.position = "none") 


plot_FS <- ggplot(SppRichness_filtered, aes(x = mean, y = Species_full)) +
  geom_point(aes(color = color_category), size = 3) +  
  geom_segment(aes(x = X25., xend = X75., color = color_category), size = 2) +  
  geom_segment(aes(x = X10., xend = X90., color = color_category), size = 1) +  
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  geom_segment(aes(x = X2.5., xend = X97.5., color = color_category), size = 1, linetype = "dashed") + 
  labs(title = "Flower Species Richness", x = "Posterior Coefficient Value", y = "Species") +  
  scale_color_manual(values = c("gray80" = "gray80", "gray60" = "gray60", "green4" = "green4", "yellow3" = "yellow3"), breaks = c("gray80", "gray60", "green4", "yellow3")) +
  annotate("text", x = -6, y = Inf, label = "d", size = 11.5, hjust = 0, vjust = 1, color = "black") +
  theme_minimal() + 
  mytheme +
  theme(legend.position = "none") 


plot_HC <- ggplot(habitatComplexity_filtered, aes(x = mean, y = Species_full)) +
  geom_point(aes(color = color_category), size = 3) +  
  geom_segment(aes(x = X25., xend = X75., color = color_category), size = 2) +  
  geom_segment(aes(x = X10., xend = X90., color = color_category), size = 1) +  
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  geom_segment(aes(x = X2.5., xend = X97.5., color = color_category), size = 1, linetype = "dashed") + 
  labs(title = "Habitat Complexity", x = "Posterior Coefficient Value", y = "Species") +  
  scale_color_manual(values = c("gray80" = "gray80", "gray60" = "gray60", "green4" = "green4", "yellow3" = "yellow3"), breaks = c("gray80", "gray60", "green4", "yellow3")) +
  annotate("text", x = -6, y = Inf, label = "b", size = 11.5, hjust = 0, vjust = 1, color = "black") + 
  theme_minimal() + 
  mytheme +
  theme(legend.position = "none") 

# removes axis titles (comment this section out if needed)
plot_PC1 <- plot_PC1 + theme(axis.title.x = element_blank(), axis.title.y = element_blank())
plot_HC <- plot_HC + theme(axis.title.x = element_blank(), axis.title.y = element_blank())
plot_VD <- plot_VD + theme(axis.title.x = element_blank(), axis.title.y = element_blank())  
plot_FS <- plot_FS +  theme(axis.title.x = element_blank(), axis.title.y = element_blank())
plot_GBA <- plot_GBA + theme(axis.title.x = element_blank(), axis.title.y = element_blank())


# main plots
cat_subplots_main <- grid.arrange(plot_PC1, plot_HC, plot_VD, plot_FS, ncol = 2, nrow = 2)
ggsave("cat_subplots_main.png", width = 24, height = 22, plot = cat_subplots_main, dpi = 600)

# supplemental plots
ggsave("cat_subplots_supp.png", width = 14, height = 10, plot = plot_GBA, dpi = 600)
