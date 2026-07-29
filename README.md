# SHC Survey Postprocessing

Scripts for postprocessing a dataset of survey responses from Catalan citizens on second-hand clothing (SHC) consumption. 


## Overview

This repository contains the R scripts used to clean, transform, and prepare the raw survey data for the statistical analyses.


## Repository Structure

```text
.
├── data/
│   ├── raw/                      # Raw survey export 
│   └── proc/                     # Subsample
├── scripts/
│   ├── subsampling/              # Scripts to create the subsample representative of Catalonian population
├── outputs/                     
│   ├── figures/                  # Plots
│   └── tables/                   # Tables 
└── README.md
```

## Requirements

* **R** version >= 4.3.0

### Key packages

* `tidyverse` — data wrangling

## Data Availability

The raw survey data is described and available at https://dataverse.csuc.cat/dataset.xhtml?persistentId=doi:10.34810/DATA3531

## Citation

Morell-Delgado, Gemma; Urraca, Ruben; Talens Peiró, Laura; Toboso-Chavero, Susana, 2026, "Second-hand clothing consumption among the Catalan population", https://doi.org/10.34810/DATA3531, CORA.Repositori de Dades de Recerca, V1
