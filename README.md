# SHC Survey Postprocessing

Scripts for analyzing survey responses from Catalan citizens on second-hand clothing (SHC) consumption. 


## Overview

This repository contains the R scripts used to clean, transform, and prepare the raw survey data for the statistical analyses.


## Repository Structure

```text
.               # Subsample
├── scripts/
│   ├── subsampling/              # Scripts to create the subsample representative of Catalonian population
│   ├── descriptive/                # Scripts for descriptive statistics and bi-variate associations.
│   ├── lca/                             # Scripts to identify consumer profiles with latent class analysis
├── out/                                   # Tables 
├── figs/                                  # Plots
└── README.md
```

## Requirements

* **R** version >= 4.3.0

### Key packages

* `tidyverse` — data wrangling
* `lpSolve ` — quota sampling
* `poLCA` — Latent Class Analysis 
* `rstatix` — statistical tests

## Data Availability

The raw survey data is described and available at https://dataverse.csuc.cat/dataset.xhtml?persistentId=doi:10.34810/DATA3531

## Citation

Morell-Delgado, Gemma; Urraca, Ruben; Talens Peiró, Laura; Toboso-Chavero, Susana, 2026, "Motivations, barriers, and consumer profiles in second-hand clothing consumption", under review
