# clear all objects from the current R environment
rm(list=ls())

# packages
library(dplyr)
library(BayesFactor)

# import data sets
post_model_vals <- read.csv("model1_posterior.csv")
functional_traits <- read.csv("functional_traits_4Archive.csv")

post_model_vals <- post_model_vals %>%
  rename(Species = tribe)

functional_traits$Native <- factor(functional_traits$Native)
functional_traits$Sociality <- factor(functional_traits$Sociality)
functional_traits$Nesting <- factor(functional_traits$Nesting)

# merge functional traits with posterior values
post_merged <- merge(
  post_model_vals,
  functional_traits,
  by = "Species"
)

covar_type <- c(
  "PC1",
  "Habitat Complexity",
  "Garden Bed Area",
  "Vegetative Density",
  "Flower Species Richness"
)

trait_type <- c(
  "Native",
  "Sociality",
  "Nesting"
)


# =============================================================
# Posterior uncertainty propagation for categorical traits
#
# For each iteration:
#   1. Sample one posterior coefficient estimate per species.
#   2. Calculate the mean coefficient for each trait group.
#   3. Compute the difference between groups.
#
# Repeating this process propagates species-level posterior
# uncertainty into trait-group comparisons.
#
# The resulting distribution of group differences is used to:
#   - estimate mean effect differences
#   - calculate 95% credible intervals
#   - estimate the probability of a positive effect
#   - compute Bayes Factors against a null difference of zero
# =============================================================

n_iter <- 3000 # number of posterior re sampling iterations

final_results <- data.frame()

for (covar in covar_type) {
  
  covar_data <- post_merged %>%
    filter(covariate == covar)
  
  for (trait in trait_type) {
    
    groups <- sort(unique(covar_data[[trait]]))
    
    # compare all pairwise trait-group combinations
    pairs <- combn(groups, 2, simplify = FALSE)
    
    for (pair in pairs) {
      
      g1 <- pair[1]
      g2 <- pair[2]
      
      diff_vals <- c()
      
      # sample one posterior draw per species to preserve
      for (i in 1:n_iter) {
        
        sampled <- covar_data %>%
          group_by(Species) %>%
          slice_sample(n = 1) %>%
          ungroup()
        
        x <- sampled$value[sampled[[trait]] == g1]
        y <- sampled$value[sampled[[trait]] == g2]
        
        x <- x[!is.na(x)]
        y <- y[!is.na(y)]
        
        if (length(x) >= 1 & length(y) >= 1) {
          
          # calculate difference in mean response between trait groups
          diff <- mean(x) - mean(y)
          
          diff_vals <- c(diff_vals, diff)
        }
      }
      
      # evaluate evidence for a non-zero difference using Bayes Factors
      temp_bf <- ttestBF(x = diff_vals, mu = 0)
      
      log_BF <- temp_bf@bayesFactor$bf
      BF <- exp(log_BF)
      
      temp <- data.frame(
        covariate = covar,
        trait = trait,
        comparison = paste(g1, "-", g2),
        
        BF = BF,
        log_BF = log_BF,
        
        mean_diff = mean(diff_vals, na.rm = TRUE),
        
        lower = quantile(diff_vals, 0.025, na.rm = TRUE),
        
        upper = quantile(diff_vals, 0.975, na.rm = TRUE),
        
        prop_positive = mean(diff_vals > 0, na.rm = TRUE)
      )
      
      final_results <- rbind(final_results, temp)
    }
  }
}

rownames(final_results) <- NULL

# save data results
write.csv(final_results,"final_catagorical_results.csv")


# ===============================================================
# Posterior uncertainty propagation for female body length
#
# For each covariate:
#   1. Sample one posterior coefficient estimate per species.
#   2. Fit a Bayesian linear model relating species response
#      coefficients to standardized female body length
#   3. Repeat across posterior draws to propagate uncertainty
#      from the occupancy model into the trait analysis.
#
# The resulting distribution of regression coefficients is used
# to estimate:
#   - mean body-size effect
#   - 95% credible intervals
#   - probability of positive/negative relationships
#   - Bayes Factor support for the body-size effect
# ==============================================================

# posterior sampling proportionate for continuous functional trait (body length)
n_iter <- 250

body_results <- list()

for (covar in covar_type) {
  
  covar_data <- post_merged %>%
    filter(covariate == covar) %>%
    filter(!is.na(Female.Length), !is.na(value))
  
  beta_draws <- numeric(n_iter)
  bf_draws <- numeric(n_iter)
  
  # sample one posterior draw per species
  for (i in 1:n_iter) {
    
    sampled <- covar_data %>%
      group_by(Species) %>%
      slice_sample(n = 1) %>%
      ungroup() %>%
      mutate(Female.Length.z = as.numeric(scale(Female.Length)))
    
    # Bayesian regression of species response versus body size
    fit <- lmBF(value ~ Female.Length.z, data = sampled)
    
    # extract Bayes Factor for body-size effect
    bf_draws[i] <- extractBF(fit)$bf
    
    # draw posterior samples of regression coefficients
    post <- posterior(fit, iterations = 250)
    
    # store posterior mean slope estimate for this iteration
    beta_draws[i] <- mean(post[, "Female.Length.z"])
  }
  
  # summarize posterior distribution across uncertainty-propagation
  body_results[[covar]] <- data.frame(
    covariate = covar,
    mean = mean(beta_draws),
    lower = quantile(beta_draws, 0.025),
    upper = quantile(beta_draws, 0.975),
    prop_positive = mean(beta_draws > 0),
    prop_negative = mean(beta_draws < 0),
    BF_mean = mean(bf_draws),
    log_BF_mean = mean(log(bf_draws))
  )
}

final_body_results <- bind_rows(body_results)

# save data results
write.csv(final_body_results,"final_body_results.csv")
