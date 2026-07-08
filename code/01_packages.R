################################################################################
# REPLICATION PACKAGE
#
# Public Lighting, Fear of Crime, and Nighttime Mobility:
# Evidence from Brazil
#
# Authors:
# Wander Plassa
# Jamila Santiago
# Luan Bernardelli
################################################################################

packages <- c(
  "dplyr",
  "readxl",
  "spdep",
  "sf",
  "ggplot2",
  "sp",
  "ggspatial",
  "psych",
  "writexl",
  "corrplot",
  "officer",
  "reshape2",
  "stargazer",
  "broom",
  "ggcorrplot",
  "sjPlot",
  "kableExtra",
  "gtsummary",
  "nnet",
  "DescTools",
  "stringr",
  "tidyr",
  "openxlsx",
  "haven",
  "lavaan",
  "marginaleffects",
  "purrr",
  "scales"
)

new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]

if(length(new_packages) > 0){
  install.packages(new_packages)
}

invisible(lapply(packages, library, character.only = TRUE))
