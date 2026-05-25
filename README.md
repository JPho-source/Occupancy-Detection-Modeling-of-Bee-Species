# Occupancy-Detection-Modeling

Application of Occupancy Detection Modeling (ODM) using participatory science data from the paper:

> "Effects of habitat complexity on urban bee occupancy eclipse those of urbanization and flower diversity"

---

## Authors

- Jason Pho 
- Cheyenne Davis
- Colby Kapp
- Evelyn Guerrero
- Nina Fogel 
- Adam B. Smith (https://orcid.org/0000-0002-6420-1659)
- Nicole Miller-Struttmann (https://orcid.org/0000-0002-4799-4802) (*Corresponding Author)

Affiliations:
- Department of Biology, Webster University
- Missouri Botanical Garden

---

## Overview

This repository contains R scripts and processed datasets used for:
- occupancy-detection modeling
- posterior analyses
- uncertainty propagation
- trait-based comparisons
- figure generation

---

## Repository Structure

```text
R/              # R scripts
data/           # processed datasets
outputs/        # model outputs and figures
```

---

## Workflow

1. Clean and merge datasets
2. Build detection histories
3. Fit occupancy-detection models
4. Extract posterior distributions
5. Generate figures and tables

---

## Main Environmental Covariates

- Urbanization (`PC1`)
- Habitat Complexity
- Vegetative Density
- Flower Species Richness
- Garden Bed Area

---
