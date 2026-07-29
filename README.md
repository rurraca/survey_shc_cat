# SHC Survey Postprocessing

Scripts for postprocessing a dataset of survey responses from Catalan citizens on second-hand clothing (SHC) consumption. This repository accompanies the manuscript **[Manuscript Title]** (submitted to **[Journal Name]**, 2026).

## Overview

This repository contains the R scripts used to clean, transform, and prepare the raw survey data for the statistical analyses reported in the publication.

It does **not** include the raw survey instrument or participant-level identifiable data; only de-identified, processed outputs are provided, in line with our data-sharing agreement.

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
