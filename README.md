# Effects of habitat complexity on urban bee occupancy eclipse those of urbanization and flower diversity

This repository contains the data, analysis code, and supplementary materials for the paper: "Effects of habitat complexity on urban bee occupancy eclipse those of urbanization and flower diversity". The repository is organized to allow full reproduction of the analyses and figures presented in the paper.

---

## Authors
Jason Pho, Cheyenne Davis, Colby Kapp, Evelyn Guerrero, [Nina Fogel](https://orcid.org/0000-0002-8065-2150), [Adam B. Smith](https://orcid.org/0000-0002-6420-1659), [Nicole Miller-Struttmann*](https://orcid.org/0000-0002-4799-4802) (*Corresponding Author)

---

## Repository Structure

### **Datasets**
##### Bed Entry Sheet1_4Archive.csv
- Raw field data collected on the environmental conditions of each site. This dataset contains
site information , survey observations, and local environmental variables (habitat complexity
and area of garden beds) recorded during field sampling at each sample site.
- Column names: Date is the date of data collection, LocationID is the unique identifier code
given to each location. Habitat Complexity is a categorical rank describing the structure of
the garden (see the associated article for more details). Bed # refers to the unique number
given to each garden bed. Location B/F/S/ denotes the location of the garden bed (B:
backyard, F: frontyard, S: side yard). Length 1 is the length of the bed in meters, Width 1 is
the width of the bed in meters, and Area (m^2) is the area of the bed, calculated based on
the shape of the bed (most rectangular). Veg Density is the percent of the bed that is
covered in vegetation, and % Bare ground is the percent of the bed with exposed soil or
light mulching. Year is the year during which the data were collected.

##### SbeeDataClean_strict_v2_4Archive.csv
- Cleaned and quality-controlled version of the raw bee survey dataset used for the occupancy modeling.
- Column names: LocationID is the unique identifier code given to each location. Columns
latitude and longitude provide the geographic location of the obseravations, and the
coordinates_obscured describes whether or not the location was obscured on iNaturalist.
scientific_name and common_name provide the scientific and common name for the bee
species observed. Protocol was recorded in the notes on iNaturalists; participants entered
“sbee” in the Notes section on iNaturalist to indicate use of the Shutterbee Citizen science
protocol. Duration refers to the duration of the survey in minutes. BeeGenus and
BeeSpecies include the scientific genus and scientific species names, respectively. Columns
year, month, and day record the date on which the bee was observed. Observed_on is the formatted year, month, and day (YYYY-MM-DD) of bee observation. User logins were deidentified and replaced with unique anonymous identifiers.

##### functional_traits_4Archive.csv
- Bee species functional trait data used for trait-based analyses as gathered from the
literature. Variables include morphological, nesting, foraging, and size and habitat
characteristics relevant to each species.
- Column names: Species includes the genus and species name, Genus includes only the
genus name. Abbreviated_Name contains the shortened scientific name for figures.
Nesting contains information about the most commonly sited nesting location: Above
Ground or Below Ground. Lecty includes categorical description of foraging specialization
(i.e., Generalist or Specialist). Native column contains the geographic status of the species,
either Exotic or Native. Female.Length refers to the mean length of female bees in the
genus, as reported in the literature. Size contains a categorical description of size.

##### PCA_4Archive.csv
- Principal component analysis dataset and outputs used to quantify urbanization (PC1) from
remotely-sensed variables within a 500m radius of each across study sites. Variables
included human population density, impervious surface cover, open water, crop cover,
grassland, barren/sparsely vegetated land, evergreen tree cover, and deciduous tree cover.
- Column names: LocID is the unique identifier code given to each location. PC1 and PC2
contain the first and second principle components, respectively, of a principle component
analysis of environmental variables (specifically, human population density, percent
impervious surface area, and precent tree cover) within a 500m radius around each site.
Column percent_Pop2020_500m contains human population density from the 2020 census
surrounding each site weighted by the buffer size. percent_URBAN_IMPERVIOUS_500m
contains percent of the land cover that was impervious surface.
percent_OPEN_WATER_500m, percent_ROW_CROPS_500m, 
percent_GRASSLAND_500m, percent_BARREN_SPARSELY_VEGETATED_500M,
percent_EVERGREEN_WOODY_VEGETATION_500m, and
percent_DECIDUOUS_WOODY_VEGETATION_500m refer to the percent of land cover
within 500m around each site that was identified as open water, row crop, grassland
(including lawn grass and natural grasslands), barren or sparsely vegetated land, evergreen
trees and shrubs, and deciduous trees and shrubs, respectively. X and Y refer respectively to
the latitude and longitude of each sampling site.

##### Total.Flower.Count-Richness_4Archive.csv
- Floral abundance and richness data collected at survey sites. This dataset contains flower
counts and plant species richness measurements used in occupancy modeling.
- Column names: sbeeID is the unique identifier code given to each location.
TotalFlowerNumber is the estimated total number of flowers, fruits, and buds observed at
each site during a mid-season survey. FlowerRichness contains the number of flowering
plant species at each sample site.


### **Code** 
##### Occupancy_Model_Analysis.R
- Primary analysis script used to fit occupancy-detection models and evaluate the effects of habitat complexity, urbanization, vegetative diversity, garden bed area, flower species richness on bee occupancy.

##### Observer_Species_Detection.R
- Creates observer and bee species variability in detection results.  

##### HC_Detection_Comparison.R
- Compares alternative detection model structures and evaluates the influence of habitat complexity on detection probability.

##### Posterior_Propagation.R
- Performs posterior propagation analyses to estimate uncertainty and generate model predictions from fitted occupancy models.

##### Posterior_Trait_Plot.R
- Creates figures illustrating posterior distributions and trait-specific model results.

##### Caterpillar_Plot.R
- Creates caterpillar plots displaying parameter estimates and associated credible intervals from occupancy models.

##### Violin_Plot.R
- Creates violin plots visualizing the distribution of model outputs based on species slope means.


### **Workflow**
1. Run Occupancy_Model_Analysis.R to fit occupancy models.
2. Run Posterior_Propagation.R to generate model predictions and uncertainty estimates.
3. Run plotting scripts (Caterpillar_Plot.R, Posterior_Trait_Plot.R, and Violin_Plot.R).
#####  *(Supplementary)* 
4. Run HC_Detection_Comparison.R to compare habitat complexity as a detection.
5. Run Observer_Species_Detection.R to compare detection variability of observers and species.

---

## Citation

Refer to this citation to cite this code, data, and paper:
- Pho, J., Davis, D., Kapp, C., Guerrero, E., Fogel, N., Smith, A. B., Miller-Struttmann, N. (2026). Effects of habitat complexity on urban bee occupancy eclipse those of urbanization and flower diversity. _In press at Biological Conservation_.
  
---

## Additional Link

Check out our lab wbesite to learn about the SHUTTERBEE Project and other intresting works:
- SHUTTERBEE - [Link](https://shutterbee.net/)
