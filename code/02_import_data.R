################################################################################
# 02 - Import data
#
# Replication package:
# Public Lighting, Fear of Crime, and Nighttime Mobility:
# Evidence from Brazil
#
# Expected file:
# data/raw/pm3549_tipo1_v1.rds
################################################################################

# Path to the original dataset
data_path <- "data/raw/pm3549_tipo1_v1.rds"

# Check whether the dataset is available
if (!file.exists(data_path)) {
  stop(
    paste0(
      "Dataset not found.\n\n",
      "Expected location:\n",
      data_path,
      "\n\n",
      "Please see data/raw/README.md for instructions on obtaining the original PNV 2012 dataset."
    )
  )
}

# Import data
Base_geral <- readRDS(data_path)

message("✓ Dataset successfully imported.")
message("Observations: ", format(nrow(Base_geral), big.mark = ","))
message("Variables: ", ncol(Base_geral))
