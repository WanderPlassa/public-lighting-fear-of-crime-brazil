# Public Lighting, Fear of Crime, and Nighttime Mobility: Evidence from Brazil

## Authors

- Wander Plassa
- Luan Bernardelli
- Jamile Santiago

## Overview

This repository contains the replication materials for the article:

> Public Lighting, Fear of Crime, and Nighttime Mobility: Evidence from Brazil.

The repository includes all R scripts required to reproduce the data preparation, descriptive statistics, Structural Equation Models (SEM), tables, and figures presented in the manuscript.

## Repository structure

```
code/               R scripts
data/
   raw/             Original data (not redistributed)
   processed/       Processed datasets
output/
   tables/          Generated tables
   figures/         Generated figures
```

## Software requirements

- R (version 4.4 or later)
- RStudio (recommended)

Main packages:

- tidyverse
- lavaan
- semPlot
- psych
- haven
- janitor
- openxlsx
- ggplot2

## Data availability

This study uses data from the **2012 Brazilian National Victimization Survey (PNV)**.

The original dataset is **not redistributed** in this repository. Researchers should obtain the data from the original data provider and place it in the `data/raw/` folder.

## Reproducibility

Run the scripts in the `code/` folder in numerical order.

All tables and figures presented in the manuscript will be automatically reproduced.

## License

This repository is distributed under the MIT License.
