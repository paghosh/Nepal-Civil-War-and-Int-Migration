# 00_setup.R
# Purpose: Central configuration file

# ---- EDIT THIS PATH ----
DATA_RAW <- "/Users/rameshdulal/Library/CloudStorage/Dropbox/Nepal Civil Conflict/Results"

# Where processed data can be saved inside the repo
DATA_PROCESSED <- "data/Modified_Data"

if (!dir.exists(DATA_PROCESSED)) {
  dir.create(DATA_PROCESSED, recursive = TRUE)
}

message("Setup loaded")
