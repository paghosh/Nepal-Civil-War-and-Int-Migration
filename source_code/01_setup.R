# =============================================================================
# 01_setup.R - Setup: Paths, Packages, and Settings
# =============================================================================
# Project: Nepal Civil Conflict and International Migration
# Author: Ramesh Dulal
# =============================================================================

# -----------------------------------------------------------------------------
# PATHS - Modify these for different users/machines
# -----------------------------------------------------------------------------

# USER 1: Ramesh
dropbox_path <- "/Users/rameshdulal/Library/CloudStorage/Dropbox/Nepal Civil Conflict"
output_path <- "/Volumes/Ramesh-Research"
code_path <- "/Users/rameshdulal/Documents/Web Portfolio/Nepal-Civil-War-and-Int-Migration/source_code"

# USER 2: Collaborator (uncomment and modify)
# dropbox_path <- "Specify the dropbox path here and uncomment this line"
# output_path <- "Specify the GitHub path for saving the output tables"
# code_path <- "Specify the Github path with source code"

# Set working directory
setwd(dropbox_path)




# -----------------------------------------------------------------------------
# PACKAGE INSTALLATION (run once, then comment out)
# -----------------------------------------------------------------------------

# install.packages(c(
#   "haven",        # Import Stata files
#   "dplyr",        # Data manipulation
#   "tidyr",        # Data reshaping
#   "ggplot2",      # Visualization
#   "labelled",     # Variable labels
#   "stringr",      # String manipulation
#   "knitr",        # Tables for LaTeX
#   "kableExtra",   # Enhanced tables
#   "writexl",      # Export to Excel
#   "fixest",       # Fixed effects regression
#   "stargazer",    # Regression tables
#   "modelsummary", # Model summary tables
#   "tinytex",      # LaTeX compilation
#   "tinytable",    # Tables
#   "webshot2"      # Save HTML as PNG
# ))

# For LaTeX packages (run once)
# tinytex::install_tinytex()
# tinytex::tlmgr_install(c("booktabs", "float", "colortbl", "xcolor"))

# -----------------------------------------------------------------------------
# LOAD PACKAGES
# -----------------------------------------------------------------------------

# Data import/export
library(haven)
library(writexl)

# Data manipulation
library(dplyr)
library(tidyr)
library(stringr)
library(labelled)

# Visualization
library(ggplot2)

# Tables and output
library(knitr)
library(kableExtra)
library(tinytex)
library(tinytable)
library(webshot2)

# Regression analysis
library(fixest)
library(stargazer)
library(modelsummary)

# -----------------------------------------------------------------------------
# GLOBAL SETTINGS
# -----------------------------------------------------------------------------

# Survey year for age calculations
SURVEY_YEAR <- 2017

# Conflict period
CONFLICT_START <- 1996
CONFLICT_END <- 2006

