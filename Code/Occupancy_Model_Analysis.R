# clear all objects from the current R environment
rm(list=ls())

# packages
library(omnibus)
library(ubms)
library(unmarked)
library(dplyr)
library(ggplot2)

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
  
  # # adding flower species richness/diversity to siteCovars
  # localEnvData$Veg.Density <- as.numeric(localEnvData$Veg.Density)
  

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
    ~ observer + scale(duration) + jd + jd2 ~
      scale(PC1) + scale(totalBedArea_m2) + Habitat.Complexity +
      scale(vegetativeDensityPerc) + scale(FlowerRichness),
    data=umf,
    chains=4,
    iter=3000
  )
  
  
  # creates data frames if it is going through first bee
  if (thisBee == 1) {
    model1_sum_det_filtered <- data.frame()
    model1_sum_occu_filtered <- data.frame()
    post_model_vals <- data.frame()  
  }
  
  # creates summary of detection from model
  model1_det_sum <- summary(model1, 'det')
  covariateTable <- cbind(tribe = tribes[thisBee], model1_det_sum)
  model1_sum_det_filtered <- rbind(model1_sum_det_filtered, covariateTable)
  
  # creates summary of occupancy from model
  model1_occu_sum <- summary(model1, 'state')
  covariateTable <- cbind(tribe = tribes[thisBee], model1_occu_sum)
  model1_sum_occu_filtered <- rbind(model1_sum_occu_filtered, covariateTable)
  
  
  # collects occupancy posterior values for each species
  covar_name <- c('Intercept','PC1','Garden Bed Area','Habitat Complexity',
                  'Vegetative Density','Flower Species Richness')
  
  postTemp <- data.frame()
  
  for (col in 1:6) {
    
    param_name <- paste0("beta_state[", col, "]")
    
    values <- c(
      model1@stanfit@sim$samples[[1]][[param_name]][1501:3000],
      model1@stanfit@sim$samples[[2]][[param_name]][1501:3000],
      model1@stanfit@sim$samples[[3]][[param_name]][1501:3000],
      model1@stanfit@sim$samples[[4]][[param_name]][1501:3000]
    )
    
    postTemp_single <- data.frame(
      tribe     = tribes[thisBee],
      covariate = covar_name[col],
      value     = values
    )
    
    postTemp <- rbind(postTemp, postTemp_single)
    
  }
  
  post_model_vals <- rbind(post_model_vals, postTemp)
  
  
  # saves model data
  save(model1, file = paste0(tribes[thisBee], ".Rdata"))

  # collects and saves trace plot
  species_traceplot <- traceplot(model1, pars=c("beta_state"))
  species_traceplot <- species_traceplot + ggtitle(paste0(tribes[thisBee], " (Traceplot Plot)"))
  ggsave(paste0("Trace Plot ", tribes[thisBee], ".png"),
         width = 12, height = 6, plot = species_traceplot, dpi = 300)

  # collects and saves residual plot
  species_residual <- plot_residuals(model1, 'state')
  species_residual <- species_residual + ggtitle(paste0(tribes[thisBee], " (Residual Plot)"))
  ggsave(paste0("Residual Plot ", tribes[thisBee], ".png"),
         width = 12, height = 6, plot = species_residual, dpi = 300)

  # collects and saves detection plot
  species_det_plot <- plot_effects(model1, 'det')
  ggsave(paste0("Detection Plot ", tribes[thisBee], ".png"),
         width = 8, height = 8, plot = species_det_plot, dpi = 300)

  # collects and saves occupancy plot
  species_state_plot <- plot_effects(model1, 'state')
  ggsave(paste0("Occupancy Plot ", tribes[thisBee], ".png"),
         width = 8, height = 8, plot = species_state_plot, dpi = 300)
  
} # end of major loop


# calculates Z-Score and Probabilities
m <- c("model1_sum_occu_filtered")
perc <- c(0.025, 0.10, 0.25, 0.5, 0.75, 0.9, 0.975)

for(model_number in 1:length(m)) {
  
  covariate_model <- get(m[model_number]) 
  
  for(pIndex in 1:length(perc)) {
    
    z <- qnorm(perc[pIndex])
    x_values <- covariate_model$mean + z * covariate_model$sd
    
    col_name <- paste0(perc[pIndex] * 100, "%")
    covariate_model[[col_name]] <- x_values
  }
  
  percentile_cols <- c("2.5%", "10%", "25%", "50%", "75%", "90%", "97.5%")
  
  covariate_model <- covariate_model[, c(
    setdiff(names(covariate_model), percentile_cols),
    percentile_cols
  )]
  
  assign(m[model_number], covariate_model)
}

# save data frames after the loop and calculations
write.csv(model1_sum_det_filtered, "model1_det_outputs.csv")
write.csv(model1_sum_occu_filtered, "model1_summary.csv")
write.csv(post_model_vals, "model1_posterior.csv")
