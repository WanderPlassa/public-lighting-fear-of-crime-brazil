# Public Lighting, Fear of Crime, and Nighttime Mobility: Evidence from Brazil

> **Note:** This repository accompanies a manuscript currently under peer review. The replication materials may be updated following the review process.
>
> ## Permanent archive

The archived version of this replication package is permanently available at:

https://doi.org/10.5281/zenodo.21268317

## Overview

This repository contains the complete replication materials for the article:

> **Public Lighting, Fear of Crime, and Nighttime Mobility: Evidence from Brazil**

The repository includes all R scripts required to reproduce the data preparation, descriptive statistics, Structural Equation Models (SEM), tables, and figures presented in the manuscript.

---

## Authors

- Wander Plassa
- Jamila Santiago
- Luan Bernardelli

---

## Repository structure

```
.
├── code/
│   ├── 00_run_all.R
│   ├── 01_packages.R
│   ├── 02_import_data.R
│   ├── 03_data_preparation.R
│   ├── 04_descriptive_statistics.R
│   └── 05_sem_models.R
│
├── data/
│   ├── raw/
│   └── processed/
│
├── output/
│   ├── figures/
│   └── tables/
│
├── LICENSE
├── CITATION.cff
└── README.md
```

---

## Software requirements

The replication package was developed in **R**.

Main packages include:

- tidyverse
- lavaan
- sf
- ggplot2
- marginaleffects
- psych
- openxlsx
- haven

Additional packages are automatically loaded by:

```
code/01_packages.R
```

---

## Data availability

The analyses are based on the **2012 Brazilian National Victimization Survey (PNV 2012)**.

The original microdata are **not redistributed** in this repository.

The dataset was originally obtained from the **CRISP (Center for the Study of Crime and Public Security, Federal University of Minas Gerais)** data repository.

Information about the repository is available at:

https://www.crisp.ufmg.br/repositorio-de-dados/

Researchers interested in replicating the analyses should obtain access to the original data from the institution responsible for the survey.

The replication scripts expect the dataset to be stored as:

```
data/raw/pm3549_tipo1_v1.rds
```

---

## Reproducing the analyses

Run the replication package by executing:

```r
source("code/00_run_all.R")
```

Alternatively, execute the scripts sequentially:

1. 01_packages.R
2. 02_import_data.R
3. 03_data_preparation.R
4. 04_descriptive_statistics.R
5. 05_sem_models.R

---

## Outputs

The replication package automatically generates:

- processed datasets;
- descriptive statistics;
- SEM estimation results;
- tables;
- figures.

Outputs are saved in:

```
output/
```

---

## License

This repository is distributed under the MIT License.

---

## Citation

If you use these replication materials, please cite this repository using the information provided in `CITATION.cff`.
