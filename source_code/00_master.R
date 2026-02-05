# =============================================================================
# 00_master.R - Master Script for Nepal Civil Conflict Analysis
# =============================================================================
# Project: Nepal Civil Conflict and International Migration
# Author: Ramesh Dulal
# Last Modified: February 2026
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


# -----------------------------------------------------------------------------
# STEP 1: Setup (Paths, Packages)
# -----------------------------------------------------------------------------

source("01_setup.R")


# -----------------------------------------------------------------------------
# STEP 2: Data Cleaning & Variable Creation
# -----------------------------------------------------------------------------

source("02_data_cleaning.R")


# -----------------------------------------------------------------------------
# STEP 3: Summary Statistics & Descriptive Tables
# -----------------------------------------------------------------------------

source("03_summary_statistics.R")

# -----------------------------------------------------------------------------
# STEP 4: Main Regression Analysis
# -----------------------------------------------------------------------------

source("04_regression_main.R")

# -----------------------------------------------------------------------------
# STEP 5: Robustness Checks
# -----------------------------------------------------------------------------

source("05_robustness.R")


# -----------------------------------------------------------------------------
# STEP 6: Mechanism Analysis (Occupation & Education Channels)
# -----------------------------------------------------------------------------

source("06_mechanism_analysis.R")

# -----------------------------------------------------------------------------
# END OF THIS CODE
# -----------------------------------------------------------------------------
