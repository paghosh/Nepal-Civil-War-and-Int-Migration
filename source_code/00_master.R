# =============================================================================
# 00_master.R - Master Script for Nepal Civil Conflict Analysis
# =============================================================================
# Project: Nepal Civil Conflict and International Migration
# Author: Ramesh Dulal
# Last Modified: April 2026
#
# DESCRIPTION:
# This master script runs all analysis scripts in sequence.
# You can run the entire analysis by sourcing this single file,
# or run individual scripts for specific tasks.
#
# =============================================================================

# Clear workspace
rm(list = ls())
cat("\014")

# Close any leftover connections from a previous failed run
closeAllConnections()

# ==============================================================================
# SECTION 1: PATH DEFINITIONS
# ==============================================================================

user <- Sys.getenv("USER")    # detects your Mac/Linux username automatically


if (user == "rameshdulal") {
  dropbox <- "/Users/rameshdulal/Library/CloudStorage/Dropbox"
  github  <- "/Users/rameshdulal/Documents/Web Portfolio/Nepal-Civil-War-and-Int-Migration"
  
} else if (user == "paghosh") {            # ← replace with your username. In your console type Sys.getenv("USER") to find your username
  dropbox <- "/Users/paghosh/Dropbox"      # ← replace with your actual Dropbox path
  github  <- "/Users/paghosh/..."          # ← replace with your actual GitHub repo path
  
} else {
  stop(paste(
    "ERROR: Unrecognized user '", user, "'.",
    "Please add your paths to 00_master.R"
  ))
}


# -----------------------------------------------------------------------------
# STEP 1: Setup (Paths)
# -----------------------------------------------------------------------------

# ── All other paths built from roots (same for everyone) ─────────────────── #
modified_data <- file.path(dropbox, "Nepal Civil Conflict/Data/Modified_Data")
r_scripts     <- file.path(github,  "source_code")
figures       <- file.path(github,  "figures")

# ── Table output folders by category ─────────────────────────────────────── #
tables_summary    <- file.path(github, "tables/Tables_Summary")
tables_main       <- file.path(github, "tables/Tables_Main")
tables_robustness <- file.path(github, "tables/Tables_Robustness")
tables_heterogen  <- file.path(github, "tables/Tables_Heterogeneity")
tables_mechanism  <- file.path(github, "tables/Tables_Mechanisms")


# ==============================================================================
# SECTION 2: LOG FILE
# ==============================================================================

today    <- format(Sys.Date(), "%m-%d-%Y")
log_file <- file.path(r_scripts, paste0("log_master_R_", today, ".txt"))
con      <- file(log_file, open = "wt")
sink(con, append = FALSE, split = TRUE)   # prints to both console and log
cat("00_master.R started:", format(Sys.time()), "\n\n")

# ==============================================================================
# SECTION 3: RUN EACH R SCRIPT IN SEQUENCE
# ==============================================================================

cat("========================================================\n")
cat(" STEP 1: Setup (Packages)\n")
cat("========================================================\n")
source(file.path(r_scripts, "01_setup.R"))


cat("========================================================\n")
cat(" STEP 2: Data Cleaning & Variable Creation\n")
cat("========================================================\n")
source(file.path(r_scripts, "02_data_cleaning.R"))

# 
# cat("========================================================\n")
# cat(" STEP 3: Summary Statistics & Descriptive Tables\n")
# cat("========================================================\n")
# source(file.path(r_scripts, "03_summary_statistics.R"))
# 
# 
# cat("========================================================\n")
# cat(" STEP 4: Main Regression Analysis\n")
# cat("========================================================\n")
# source(file.path(r_scripts, "04_regression_main.R"))
# 
# 
# cat("========================================================\n")
# cat(" STEP 5: Robustness Checks\n")
# cat("========================================================\n")
# source(file.path(r_scripts, "05_robustness.R"))
# 
# 
# cat("========================================================\n")
# cat(" STEP 6: Mechanism Analysis (Occupation & Education Channels)\n")
# cat("========================================================\n")
# source(file.path(r_scripts, "06_mechanism_analysis.R"))
# 
# 
# # ==============================================================================
# # END OF MASTER R FILE
# # ==============================================================================

cat("\n✓ All R steps completed successfully.\n")
cat("Session ended:", format(Sys.time()), "\n")
sink()
close(con)