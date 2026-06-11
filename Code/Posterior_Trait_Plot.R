# clear all objects from the current R environment
rm(list=ls())

# categorical trait analysis packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)

# import posterior results for categorical functional traits
final_results <- read.csv("final_catagorical_results.csv")

# orders and subsets each trait into covariates
categorical_results <- final_results %>% filter(trait %in% c("Native", "Sociality", "Nesting"))

PC1 <- subset(categorical_results, covariate == "PC1")
habitatComplexity <- subset(categorical_results, covariate == "Habitat Complexity")
gardenBedArea <- subset(categorical_results, covariate == "Garden Bed Area")
vegetativeDensity <- subset(categorical_results, covariate == "Vegetative Density")
SppRichness <- subset(categorical_results, covariate == "Flower Species Richness")

PC1$comparison <- factor(PC1$comparison, levels = rev(PC1$comparison))
habitatComplexity$comparison <- factor(habitatComplexity$comparison, levels = rev(habitatComplexity$comparison))
gardenBedArea$comparison <- factor(gardenBedArea$comparison, levels = rev(gardenBedArea$comparison))
vegetativeDensity$comparison <- factor(vegetativeDensity$comparison, levels = rev(vegetativeDensity$comparison))
SppRichness$comparison <- factor(SppRichness$comparison, levels = rev(SppRichness$comparison))


# sets theme for plots 
mytheme <- theme(
  plot.title = element_text(hjust = 0.5, size = 24),
  axis.text.y = element_text(color = "black", size = 18),
  axis.text.x = element_text(color = "black", size = 18),
  axis.title.x = element_text(size = 23),
  axis.title.y = element_text(size = 23),
  panel.border = element_blank(),
  axis.line = element_line(),
  axis.ticks.x = element_line(color = "black"),
  axis.ticks.y = element_line(color = "black"),
  panel.grid = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  plot.margin = margin(10, 60, 10, 10)
)

plot_PC1 <- ggplot(PC1, aes(x = mean_diff, y = comparison)) +
  geom_point(color = "black", size = 6) +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Urbanization", x = "Difference in Response", y = "Trait Comparison") +
  theme_minimal() + 
  mytheme +
  theme(legend.position = "none") +
  annotate("text", x = min(PC1$lower), y = Inf, label = "a", size = 9, hjust = 0, vjust = 1)

plot_HC <- ggplot(habitatComplexity, aes(x = mean_diff, y = comparison)) +
  geom_point(color = "black", size = 6) +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Habitat Complexity", x = "Difference in Response", y = "Trait Comparison") +
  theme_minimal() +
  mytheme +
  theme(legend.position = "none") +
  annotate("text", x = min(habitatComplexity$lower), y = Inf, label = "b", size = 9, hjust = 0, vjust = 1)

plot_GBA <- ggplot(gardenBedArea, aes(x = mean_diff, y = comparison)) +
  geom_point(color = "black", size = 6) +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Garden Bed Area", x = "Difference in Response", y = "Trait Comparison") +
  theme_minimal() + 
  mytheme +
  theme(legend.position = "none") +
  annotate("text", x = min(gardenBedArea$lower), y = Inf, label = "a", size = 9, hjust = 0, vjust = 1)

plot_VD <- ggplot(vegetativeDensity, aes(x = mean_diff, y = comparison)) +
  geom_point(color = "black", size = 6) +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Vegetative Density", x = "Difference in Response", y = "Trait Comparison") +
  theme_minimal() + mytheme +
  theme(legend.position = "none") +
  annotate("text", x = min(vegetativeDensity$lower), y = Inf, label = "c", size = 9, hjust = 0, vjust = 1)

plot_FS <- ggplot(SppRichness, aes(x = mean_diff, y = comparison)) +
  geom_point(color = "black", size = 6) +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Flower Species Richness", x = "Difference in Response", y = "Trait Comparison") +
  theme_minimal() + 
  mytheme +
  theme(legend.position = "none") +
  annotate("text", x = min(SppRichness$lower), y = Inf, label = "b", size = 9, hjust = 0, vjust = 1)


plot_PC1 <- plot_PC1 + theme(axis.title.x = element_blank(), axis.title.y = element_blank())
plot_HC <- plot_HC + theme(axis.title.x = element_blank(), axis.title.y = element_blank())
plot_VD <- plot_VD + theme(axis.title.x = element_blank(), axis.title.y = element_blank())
plot_FS <- plot_FS + theme(axis.title.x = element_blank(), axis.title.y = element_blank())

# main plots
trait_subplots_main <- grid.arrange(plot_PC1, plot_HC, plot_VD, ncol = 2, nrow = 2)
ggsave("trait_subplots_main.png", width = 18, height = 14, plot = trait_subplots_main, dpi = 300)

# supplemental plots
trait_subplots_supp <- grid.arrange(plot_GBA, plot_FS, ncol = 2, nrow = 1)
ggsave("trait_subplots_supp.png", width = 18, height = 8, plot = trait_subplots_supp, dpi = 300)



# linear model trait analysis packages
library(ggplot2)
library(dplyr)

# upload posterior results for body length
final_body_results <- read.csv("final_body_results.csv")

# sets theme for plots 
mytheme <- theme(
  plot.title = element_text(hjust = 0.5, size = 20),
  axis.text.y = element_text(color = "black", size = 16),
  axis.text.x = element_text(color = "black", size = 16),
  axis.title.x = element_text(size = 20),
  axis.title.y = element_text(size = 20),
  panel.border = element_blank(),
  axis.line = element_line(),
  axis.ticks.x = element_line(color = "black"),
  axis.ticks.y = element_line(color = "black"),
  panel.grid = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  plot.margin = margin(10, 60, 10, 10)
)

# corrects covariate names 
plot_data <- final_body_results %>%
  mutate(covariate = recode(covariate, "PC1" = "Urbanization"),
         covariate = factor(covariate,levels = rev(c(
           "Urbanization",
           "Habitat Complexity",
           "Garden Bed Area",
           "Vegetative Density",
           "Flower Species Richness"
         ))
         )
  )

plot_body_length <- ggplot(plot_data, aes(x = mean, y = covariate)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2, size = 1) +
  geom_point(size = 3) +
  labs(x = "Body Length Effect Size", y = "",  title = "Female Body Length Effects") +
  theme_bw(base_size = 14) +
  theme_minimal() + 
  mytheme

# save body length plot
ggsave("plot_body_length.png", width = 11, height = 7, plot = plot_body_length, dpi = 300)
