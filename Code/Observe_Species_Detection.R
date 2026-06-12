# clear all objects from the current R environment
rm(list=ls())

# packages
library(omnibus)
library(ubms)
library(unmarked)
library(dplyr)
library(ggplot2)
library(tibble)
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
  
  covars <- c("LocID", "PC1", "PC2", 
              "percent_Pop2020_500m",                   
              "percent_URBAN_IMPERVIOUS_500m",          
              "percent_OPEN_WATER_500m",                
              "percent_ROW_CROPS_500m",                 
              "percent_GRASSLAND_500m",                 
              "percent_BARREN_SPARSELY_VEGETATED_500m", 
              "percent_EVERGREEN_WOODY_VEGETATION_500m",
              "percent_DECIDUOUS_WOODY_VEGETATION_500m", 
              "X", "Y")
  
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
  for (thisID in siteCovars$Location) {
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
  
  data2023.sum<- data2023.sum[,-1]
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
  
  model1 <- stan_occu(
    ~ observer + scale(duration) + scale(julianDay) ~
      LocationID,
    data=umf,
    chains=4,
    iter=3000
  )
  
  
  # creates data frames if it is going through first bee
  if (thisBee == 1) {
    model1_sum_det_filtered <- data.frame() 
  }
  
  # creates summary of detection from model
  model1_det_sum <- summary(model1, 'det')
  covariateTable <- cbind(tribe = tribes[thisBee], model1_det_sum)
  model1_sum_det_filtered <- rbind(model1_sum_det_filtered, covariateTable)
  
} # end of major loop


# sets plot theme
mytheme <- theme(
  plot.title = element_text(hjust = 0.5, size = 22),
  axis.title.x = element_text(color = "black", size = 20),
  axis.title.y = element_text(color = "black", size = 20),
  axis.text.y = element_text(color = "black", size = 15),
  panel.border = element_blank(),
  panel.background = element_blank(),
  axis.line = element_line(),
  axis.ticks.x = element_line(color = "black"),
  axis.ticks.y = element_line(color = "black"),
  axis.text.x = element_text(color = "black", size = 15),
  panel.grid = element_blank())


#####################
# Species Detection #
#####################
model1_sum_det_filtered <- model1_sum_det_filtered %>%
  rownames_to_column(var = "covariate")

species_detect <- model1_sum_det_filtered %>%
  filter(grepl("^\\(Intercept\\)", covariate)) %>%
  mutate(
    det_prob = plogis(mean),
    det_prob_low = plogis(`2.5%`),
    det_prob_high = plogis(`97.5%`),
    tribe = factor(tribe, levels = tribe[order(det_prob)])
  )

species_det <- ggplot(species_detect, aes(x = tribe, y = det_prob)) +
  geom_point(size = 2) +
  geom_errorbar(
    aes(ymin = det_prob_low, ymax = det_prob_high),
    width = 0
  ) +
  coord_flip() +
  labs(
    title = "Species Detectability",
    x = "Species",
    y = "Detection Probability"
  ) +
  mytheme

species_det

ggsave("species_det.png", width = 12, height = 8, plot = species_det, dpi = 300)


######################
# Observer Detection #
######################

# import datasets
pca  <- read.csv("PCA_4Archive.csv")
bed  <- read.csv("Bed Entry Sheet1_4Archive.csv")
sbee <- read.csv("SbeeDataClean_strict_v2_4Archive.csv")

# all bed sites
bed_sites <- unique(bed$LocationID)

# bed sites that are also in PCA
bed_in_pca <- intersect(bed_sites, unique(pca$LocID))

# from that subset, which are also in SBEE
bed_in_pca_in_sbee <- intersect(bed_in_pca, unique(sbee$LocationID))

# users from matched sites
matched_users <- sbee %>%
  filter(LocationID %in% bed_in_pca_in_sbee) %>%
  select(LocationID, user_login)

valid_users <- matched_users %>%
  pull(user_login) %>%
  unique() %>%
  tolower() %>%
  trimws()

# filter observer rows from model output
detect_filtered <- model1_sum_det_filtered %>%
  filter(str_detect(covariate, "^observer")) %>%
  mutate(
    observer = str_remove(covariate, "^observer"),
    observer = tolower(trimws(observer))
  ) %>%
  filter(observer %in% valid_users)

# get intercept
intercept <- model1_sum_det_filtered %>%
  filter(covariate == "(Intercept)") %>%
  summarise(intercept = mean(mean, na.rm = TRUE)) %>%
  pull(intercept)

# build observer-level detection probabilities
observer_detect <- detect_filtered %>%
  mutate(
    logit_det     = intercept + mean,
    det_prob      = plogis(logit_det),
    det_prob_low  = plogis(logit_det - se_mean),
    det_prob_high = plogis(logit_det + se_mean)
  )

# summarize one value per observer
observer_summary <- observer_detect %>%
  group_by(observer) %>%
  summarise(
    mean_det_prob = mean(det_prob, na.rm = TRUE),
    mean_se_logit = mean(se_mean, na.rm = TRUE),
    mean_det_low  = mean(det_prob_low, na.rm = TRUE),
    mean_det_high = mean(det_prob_high, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    rank = rank(-mean_det_prob)
  )

# plot
observer_det_plot <- ggplot(observer_summary, aes(x = rank, y = mean_det_prob)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean_det_low, ymax = mean_det_high), width = 0) +
  labs(title = "Observer Detectability Varies", x = "Observer rank", y = "Mean detection probability") +
  mytheme

observer_det_plot

ggsave("observer_det.png", width = 10, height = 6, observer_det_plot, dpi = 300 )
