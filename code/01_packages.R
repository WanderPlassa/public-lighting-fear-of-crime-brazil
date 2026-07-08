################################################################################
# 01 - Load required packages
#
# Replication package:
# Public Lighting, Fear of Crime, and Nighttime Mobility:
# Evidence from Brazil
################################################################################

packages <- c(
  "dplyr",
  "readxl",
  "ggplot2",
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

options(repos = c(CRAN = "https://cloud.r-project.org"))

new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]

if (length(new_packages) > 0) {
  message("Installing missing packages...")
  install.packages(new_packages)
}

invisible(lapply(packages, function(pkg) {
  suppressPackageStartupMessages(
    library(pkg, character.only = TRUE)
  )
}))

message("R version: ", R.version.string)
message("✓ All packages successfully loaded.")

message("✓ All packages successfully loaded.")
