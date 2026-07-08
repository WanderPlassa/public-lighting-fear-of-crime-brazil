################################################################################
# REPLICATION PACKAGE
#
# Public Lighting, Fear of Crime, and Nighttime Mobility:
# Evidence from Brazil
#
# This script reproduces all analyses presented in the paper.
#
# Authors:
# Wander Plassa
# Jamila Santiago
# Luan Bernardelli
################################################################################

message("===========================================")
message("Running replication package...")
message("===========================================")

source("code/01_packages.R")

message("✓ Packages loaded")

source("code/02_import_data.R")

message("✓ Data imported")

source("code/03_data_preparation.R")

message("✓ Data prepared")

source("code/04_descriptive_statistics.R")

message("✓ Descriptive statistics completed")

source("code/05_sem_models.R")

message("✓ SEM models estimated")

message("===========================================")
message("Replication completed successfully!")
message("===========================================")
