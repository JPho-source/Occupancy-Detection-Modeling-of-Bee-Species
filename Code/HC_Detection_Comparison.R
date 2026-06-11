# clear all objects from the current R environment
rm(list=ls())

# packages
library(omnibus)
library(ubms)
library(unmarked)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(grid)
library(stringr)

# list of bee species used for occupancy modeling
tribes <- c('Bombus griseocollis', 'Bombus impatiens', 'Halictus ligatus', 'Xylocopa virginica', 'Bombus bimaculatus', 
            'Agapostemon virescens', 'Melissodes bimaculatus', 'Anthidium manicatum', 'Bombus auricomus', 'Bombus pensylvanicus', 
            'Anthidium oblongatum', 'Ptilothrix bombiformis', 'Hylaeus leptocephalus', 'Halictus confusus', 'Halictus rubicundus', 
            'Hylaeus modestus', 'Triepeolus lunatus', 'Xenoglossa pruinosa', 'Calliopsis andreniformis', 'Megachile xylocopoides', 
            'Melitoma taurea', 'Anthophora abrupta')

# read detection data file
rawDetectionData <- read.csv("SbeeDataClean_strict_v2_4Archive.csv") 
rawDetectionData<-subset(rawDetectionData, LocationID!= 30)
rawDetectionData<- subset(rawDetectionData, !is.na(longitude) &!is.na(latitude))

# make list of sites
uniqueSites <- sort(unique(rawDetectionData$LocationID))

# make list of sampling dates
sampleDates <- rawDetectionData$observed_on 
sampleDates <- substr(sampleDates, 1, 10)
uniqueDates <- unique(sampleDates)
uniqueDates <- sort(uniqueDates)

# remove missing dates
if (anyNA(uniqueDates)) 
  uniqueDates[is.na(uniqueDates)] <- NULL

if (any(uniqueDates == '')) 
  uniqueDates <- uniqueDates[uniqueDates != '']

# get detections/non-detections for each site/date
for(thisBee in 1:length(tribes)) { 
  
  y <- data.frame() 
  
  for (thisID in uniqueSites) { 
    
    # records from this site
    recs <- which(rawDetectionData$LocationID == thisID) 
    thisSiteRecs <- rawDetectionData[recs, , drop=FALSE]
    thisSiteDates <- sampleDates[recs]
    
    # empty list of detections for this site across all dates
    detections <- rep(NA, length(uniqueDates))  
    
    # for each date, are there any detections of target taxon?
    for (thisDate in uniqueDates) {
      if (any(thisSiteDates == thisDate)) { 
        
        taxa <- thisSiteRecs$scientific_name[thisSiteDates==thisDate] 
        if (any(taxa %in% tribes[thisBee])) { 
          detections[uniqueDates == thisDate] <- 1
        } 
        else {
          detections[uniqueDates == thisDate] <- 0
        }
      }
    }
    detections <- rbind(detections)
    y <- rbind(y, detections) 
  }
  
  locationID <- data.frame(LocationID=uniqueSites)
  y <- cbind(locationID, y)
  
  names(y)[2:ncol(y)] <- paste0('date', 1:(ncol(y) - 1))
  
  
  # duration of observations
  obsCovars_duration <- data.frame(LocationID = y$LocationID)
  
  rawDetectionData$Duration <- as.numeric(rawDetectionData$Duration)
  
  for (thisDate in uniqueDates) {
    
    x <- data.frame(x = rep(NA, length(uniqueSites)))
    for (thisID in obsCovars_duration$LocationID) {
      
      recs <- which(rawDetectionData$LocationID == thisID & sampleDates == thisDate)
      if (length(recs) > 0) {
        durs <- rawDetectionData$Duration[recs]
        durs <- mean(durs, na.rm=TRUE)
        x$x[obsCovars_duration$LocationID == thisID] <- durs
      } else {
        x$x[obsCovars_duration$LocationID == thisID] <- NA
      }
    }
    obsCovars_duration <- cbind(obsCovars_duration, x)
  }
  
  names(obsCovars_duration)[2:ncol(obsCovars_duration)] <- paste0('date', 1:(ncol(obsCovars_duration) - 1))
  
  # observer
  obsCovars_observer <- data.frame(LocationID = y$LocationID)
  
  for (thisDate in uniqueDates) {
    
    x <- data.frame(x = rep(NA, length(uniqueSites)))
    for (thisID in obsCovars_observer$LocationID) {
      
      recs <- which(rawDetectionData$LocationID == thisID & sampleDates == thisDate)
      if (length(recs) > 0) {
        observer <- rawDetectionData$user_login[recs]
        observer <- unique(observer)[1]
        x$x[obsCovars_observer$LocationID == thisID] <- observer
      } else {
        x$x[obsCovars_observer$LocationID == thisID] <- NA
      }
    }
    obsCovars_observer <- cbind(obsCovars_observer, x)
  }
  
  names(obsCovars_observer)[2:ncol(obsCovars_observer)] <- paste0('date', 1:(ncol(obsCovars_observer) - 1))
  
  # julian day
  obsCovars_julianDay <- data.frame(LocationID = y$LocationID)
  
  for (thisDate in uniqueDates) {
    
    x <- data.frame(x = rep(NA, length(uniqueSites)))
    for (thisID in obsCovars_julianDay$LocationID) {
      
      recs <- which(rawDetectionData$LocationID == thisID & sampleDates == thisDate)
      if (length(recs) > 0) {
        date <- as.Date(thisDate)
        jday <- format(date, '%j')
        jday <- as.numeric(jday)
        x$x[obsCovars_julianDay$LocationID == thisID] <- jday
      } else {
        x$x[obsCovars_julianDay$LocationID == thisID] <- NA
      }
    }
    obsCovars_julianDay <- cbind(obsCovars_julianDay, x)
  }
  
  names(obsCovars_julianDay)[2:ncol(obsCovars_julianDay)] <- paste0('date', 1:(ncol(obsCovars_julianDay) - 1))
  
  
  ######################################
  ## compile regional site covariates ##
  ######################################
  regionalEnvData <- read.csv('PCA_4Archive.csv')  
  regionalEnvData<- subset(regionalEnvData, LocID!=30)
  
  # remove duplicated rows
  regionalEnvData <- regionalEnvData[!duplicated(regionalEnvData$LocID), ]
  
  covars <- c("LocID", 
              "PC1", 
              "PC2", 
              "percent_Pop2020_500m",                   
              "percent_URBAN_IMPERVIOUS_500m",          
              "percent_OPEN_WATER_500m",                
              "percent_ROW_CROPS_500m",                 
              "percent_GRASSLAND_500m",                 
              "percent_BARREN_SPARSELY_VEGETATED_500m", 
              "percent_EVERGREEN_WOODY_VEGETATION_500m",
              "percent_DECIDUOUS_WOODY_VEGETATION_500m", 
              "X", 
              "Y")
  
  siteCovars <- data.frame(LocationID = y$LocationID)
  
  for (thisCovar in covars) {
    
    values <- numeric()
    
    for (thisID in siteCovars$LocationID) {
      
      recs <- which(regionalEnvData$LocID == thisID)
      
      if (length(recs) > 0) {
        thisCovarValue <- regionalEnvData[recs, thisCovar]
        values <- c(values, thisCovarValue)
      } 
      else {
        values <- c(values, NA)
      }
      
    } # next site
    
    values <- data.frame(x = values)
    names(values) <- thisCovar
    
    if (exists('siteCovars')) {
      siteCovars <- cbind(siteCovars, values)
    } else {
      siteCovars <- values
    }
  } # next variable
  
  
  ###################################
  ## compile local site covariates ##
  ###################################
  localEnvData <- read.csv('Bed Entry Sheet1_4Archive.csv')
  localEnvData<- subset(localEnvData, LocationID!=30)
  
  # habitat complexity
  localEnvData$Habitat.Complexity <- as.numeric(as.character(localEnvData$Habitat.Complexity))
  
  x <- data.frame(Habitat.Complexity = rep(NA, length(uniqueSites)))
  for (thisID in siteCovars$LocationID) {
    recs <- which(localEnvData$LocationID == thisID)
    
    if (length(recs) > 0) {
      x$Habitat.Complexity[siteCovars$LocationID == thisID] <-
        localEnvData$Habitat.Complexity[recs[1]]
    }
  }
  
  siteCovars <- cbind(siteCovars, x)
  
  # garden bed area
  x <- data.frame(totalBedArea_m2 = rep(NA, length(uniqueSites)))
  for (thisID in siteCovars$Location) {
    recs <- which(localEnvData$LocationID == thisID)
    
    if (length(recs) > 0) {
      vals <- localEnvData$Area..m.2.[recs]
      vals <- abs(vals)
      vals <- sum(vals)
      x$totalBedArea_m2[siteCovars$LocationID == thisID] <- vals
    }
  }
  
  x$totalBedArea_m2 <- log10(x$totalBedArea_m2)
  siteCovars <- cbind(siteCovars, x)
  
  # percent bed area that are flowering plants (vegetative density)
  x <- data.frame(vegetativeDensityPerc = rep(NA, length(uniqueSites)))
  localEnvData$Veg.Density <- as.numeric(localEnvData$Veg.Density)
  for (thisID in siteCovars$Location) {
    recs <- which(localEnvData$LocationID == thisID)
    
    if (length(recs) > 0) {
      areas_m2 <- localEnvData$Area..m.2.[recs]
      areas_m2 <- abs(areas_m2)
      sumAreas <- sum(areas_m2)
      vals <- localEnvData$Veg.Density[recs]
      vals <- vals / 100
      vals <- vals * areas_m2
      vals <- sum(vals) / sumAreas
      x$vegetativeDensityPerc[siteCovars$LocationID == thisID] <- vals
    }
  }
  
  siteCovars <- cbind(siteCovars, x)
  
  # rename for long, lat, and impervious surface
  names(siteCovars)[names(siteCovars) == 'X'] <- 'longitude'
  names(siteCovars)[names(siteCovars) == 'Y'] <- 'latitude'
  names(siteCovars)[names(siteCovars) == 'percent_URBAN_IMPERVIOUS_500m'] <- 'Impervious.Surface'
  
  # flower species richness
  data2023.sum <- read.csv("Total.Flower.Count-Richness_4Archive.csv") 
  
  colnames(data2023.sum)[1]= "LocationID"
  allSites<-as.data.frame(siteCovars$LocationID)
  colnames(allSites)[1]="LocationID"
  data2023.sum<-merge(data2023.sum, allSites, by = "LocationID", all=TRUE, no.dups = TRUE)
  siteCovars<- merge(siteCovars, data2023.sum,by="LocationID")
            
  
  ###########
  ## model ##
  ###########
  siteCovars$LocationID <- as.factor(siteCovars$LocationID)
  
  # remove location ID columns from data frames where they don't belong
  y$LocationID <- NULL
  obsCovars_observer$LocationID <- NULL
  obsCovars_duration$LocationID <- NULL
  obsCovars_julianDay$LocationID <- NULL
  
  obsCovars <- list(observer = obsCovars_observer,
                    duration = obsCovars_duration,
                    julianDay = obsCovars_julianDay)
  
  umf <- unmarkedFrameOccu(y = y, siteCovs = siteCovars, obsCovs = obsCovars)
  
  
  # model
  umf@obsCovs$jd <- scale(umf@obsCovs$julianDay)
  umf@obsCovs$jd2 <- umf@obsCovs$jd^2
  
  
  # no HC for detection
  model1 <- stan_occu(
    ~ observer + scale(duration) + jd + jd2 ~
      scale(PC1) + scale(totalBedArea_m2) + Habitat.Complexity +
      scale(vegetativeDensityPerc) + scale(FlowerRichness),
    data=umf,
    chains=4,
    iter=3000
  )
  
  # with HC for detection
  model2 <- stan_occu(
    ~ observer + scale(duration) + jd + jd2 + Habitat.Complexity ~
      scale(PC1) + scale(totalBedArea_m2) + Habitat.Complexity +
      scale(vegetativeDensityPerc) + scale(FlowerRichness),
    data=umf,
    chains=4,
    iter=3000
  )
  
  
  # creates data frames if it is going through first bee
  if (thisBee == 1) {
    model1_sum_filtered <- data.frame()
    post_model_vals <- data.frame()  
    post_model1_HC <- data.frame()
    post_model2_HC <- data.frame()
  }
  
  
  # collects occupancy posterior values for each species
  param_name <- paste0("beta_state[", 4, "]")
  
  values <- c(
    model1@stanfit@sim$samples[[1]][[param_name]][1501:3000],
    model1@stanfit@sim$samples[[2]][[param_name]][1501:3000],
    model1@stanfit@sim$samples[[3]][[param_name]][1501:3000],
    model1@stanfit@sim$samples[[4]][[param_name]][1501:3000]
  )
  
  postTemp_single <- data.frame(
    tribe     = tribes[thisBee],
    covariate = 'Habitat Complexity',
    value     = values
  )
  
  post_model1_HC <- rbind(post_model1_HC, postTemp_single)
  
  
  param_name <- paste0("beta_state[", 4, "]")
  
  values <- c(
    model2@stanfit@sim$samples[[1]][[param_name]][1501:3000],
    model2@stanfit@sim$samples[[2]][[param_name]][1501:3000],
    model2@stanfit@sim$samples[[3]][[param_name]][1501:3000],
    model2@stanfit@sim$samples[[4]][[param_name]][1501:3000]
  )
  
  postTemp_single <- data.frame(
    tribe     = tribes[thisBee],
    covariate = 'Habitat Complexity',
    value     = values
  ) 
  
  post_model2_HC <- rbind(post_model2_HC, postTemp_single)
  
} # end of major loop



######################
## model comparison ##
######################

tribe_order <- c("Bombus bimaculatus", "Halictus confusus", "Bombus impatiens","Anthidium oblongatum", "Hylaeus leptocephalus", "Halictus rubicundus", "Agapostemon virescens",
                 "Xylocopa virginica","Bombus pensylvanicus", "Melissodes bimaculatus", "Triepeolus lunatus", "Bombus auricomus", "Ptilothrix bombiformis", "Bombus griseocollis", 
                 "Halictus ligatus", "Hylaeus modestus", "Anthidium manicatum", "Xenoglossa pruinosa", "Anthophora abrupta",  "Megachile xylocopoides", "Melitoma taurea", 
                 "Calliopsis andreniformis")


# caculates summaries from each model
summary_m1 <- post_model1_HC %>%
  group_by(tribe) %>%
  summarise(
    mean = mean(value),
    low = quantile(value, 0.025),
    high = quantile(value, 0.975),
    .groups = "drop"
  ) %>%
  mutate(model = "Model 1")

summary_m2 <- post_model2_HC %>%
  group_by(tribe) %>%
  summarise(
    mean = mean(value),
    low = quantile(value, 0.025),
    high = quantile(value, 0.975),
    .groups = "drop"
  ) %>%
  mutate(model = "Model 2")

plot_data <- bind_rows(summary_m1, summary_m2)
plot_data$tribe <- factor(plot_data$tribe, levels = tribe_order)


# sets theme for plots
mytheme <- theme(
  plot.title = element_text(hjust = 0.5, size = 29),
  axis.text.y = element_text(color = "black", face = "italic", size = 22),
  axis.text.x = element_text(color = "black", size = 22),
  axis.title.x = element_text(size = 25),
  axis.title.y = element_text(size = 25),
  panel.border = element_blank(), 
  axis.line = element_line(),
  axis.ticks.x = element_line(color = "black"),  
  axis.ticks.y = element_line(color = "black"),
  panel.grid = element_blank(),
  legend.text = element_text(size = 25),
  legend.title = element_blank()
)


# plot
HC_DET_plot <- ggplot(plot_data, aes(x = tribe, y = mean, color = model)) +
  geom_point(position = position_dodge(width = 1)) +
  geom_errorbar(aes(ymin = low, ymax = high), position = position_dodge(width = 1), width = 1) +
  coord_flip() +
  labs(x = "Species", y = "Effect of Habitat Complexity (Model 1 and Model 2)", color = "Model") +
  mytheme

HC_DET_plot
ggsave("HC_DET_plot.png", width = 14, height = 15, plot = HC_DET_plot, dpi = 600)
