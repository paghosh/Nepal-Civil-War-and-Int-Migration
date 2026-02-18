# ============================================
# SET YOUR PATHS HERE
# ============================================
user <- Sys.getenv("USER")

if (user == "rameshdulal") {
  data_path    <- "/Users/rameshdulal/Library/CloudStorage/Dropbox/Nepal Civil Conflict/Data/Raw_Data"
  nlfs_path <- "/Users/rameshdulal/Library/CloudStorage/Dropbox/Nepal Civil Conflict/Data/Raw_Data/NLFS 3/NLFS_dataset"
  results_path    <- "/Users/rameshdulal/Library/CloudStorage/Dropbox/Nepal Civil Conflict/Data/Modified_Data"
}

# Please change the directory path in the following lines.
# To find your username run 
#Sys.getenv("USER")
# in your R console

#if (user == "yourname") {
 # data_path    <- ""
  #results_path <- ""
#  code_path    <- ""
#}
# ============================================

# 1. SETUP ====================================




# Load Packages
library(haven) # This package helps importing data from stata dataset
library(dplyr) # This package helps to clean the data
library(ggplot2) # This helps to create graphs and visualization options
library(labelled)
library(knitr) # For exporting to Latex
library(kableExtra) # For exporting to Latex
library(writexl) # For exporting to excel format
# Capitalize the initial letter of the name of the district
library(stringr)

# 2. IMPORT MAIN DATASET ========================

# Import dataset from NLFS III
nlfs_data <- read_dta(file.path(results_path, "personal_nlfs_data.dta"))


# Check the data
names(nlfs_data) # Variable Names
dim(nlfs_data) # Dimension of the dataset (number of rows × number of columns)


# See the first rows
head(nlfs_data)

# Looking for value labels for a specific variable "dist": district
attributes(nlfs_data$dist)$labels

# 3. USING CONFLICT DATASET =========================

# Import dataset for Conflict Intensity
conflict_data <- read_dta(file.path(results_path, "conflict_intensity.dta"))

# Check the data
names(conflict_data) # Variable Names
dim(conflict_data) # Dimension of the dataset (number of rows × number of columns)

# See the first Rows
head(conflict_data)

# Looking for the value labels for a specific variable "incident_district_num"
attributes(conflict_data$incident_district_num)$labels #This shows the labels for the values or is a kind of codebook command
summary(conflict_data$incident_district_num) # It gives the summary stat with NA
table(conflict_data$incident_district_num, useNA = "ifany")

# Remove the missing district
conflict_data <- conflict_data %>%
  filter(!is.na(incident_district_num))



conflict_data <- conflict_data %>%
  mutate(district_name = str_to_title(as_factor(incident_district_num)))

# Check the names of the districts
unique(conflict_data$district_name)

# 4. MATCH DISTRICT NAMES BETWEEN DATASETS =============================

# Create district_name for NLFS data
nlfs_data <- nlfs_data %>%
  mutate(district_name = str_to_title(as_factor(dist)))

# Compare the names
conflict_districts <- sort(unique(conflict_data$district_name))
nlfs_districts <- sort(unique(nlfs_data$district_name))

# Find which needs fixing
cat("\n=== DISTRICTS THAT NEED FIXING ===\n")
cat("\nIn NLFS but NOT in Conflict (NLFS spelling):\n")
print(setdiff(nlfs_districts, conflict_districts))

cat("\nIn Conflict but NOT in NLFS (needs to be changed):\n")
print(setdiff(conflict_districts, nlfs_districts))

# Apply the fix
# Fix conflict district names to match NLFS spelling
conflict_data <- conflict_data %>%
  mutate(district_name_std = case_when(
    district_name == "Panchathar" ~ "Panchthar",
    district_name == "Kavre" ~ "Kavrepalanchok",
    district_name == "Sindhupalchowk" ~ "Sindhupalchok",
    district_name == "Parvat" ~ "Parbat",
    district_name == "Kapilvastu" ~ "Kapilbastu",
    district_name == "Dhanusha" ~ "Dhanusa",
    district_name == "Gorakha" ~ "Gorkha",
    district_name == "Makawanpur" ~ "Makwanpur",
    district_name == "Udaypur" ~ "Udayapur",
    TRUE ~ district_name  # Keep all others as they are
  ))

# Remove Manang from conflict data
conflict_data <- conflict_data %>%
  filter(district_name_std != "Manang") 


# For NLFS, just copy the district_name (it's already correct)
nlfs_data <- nlfs_data %>%
  mutate(district_name_std = district_name)

# Verify the fix
nlfs_districts_std <- sort(unique(nlfs_data$district_name_std))
conflict_districts_std <- sort(unique(conflict_data$district_name_std))

cat("\n=== AFTER FIXING ===\n")
cat("\nIn NLFS but NOT in Conflict:\n")
print(setdiff(nlfs_districts_std, conflict_districts_std))

cat("\nIn Conflict but NOT in NLFS:\n")
print(setdiff(conflict_districts_std, nlfs_districts_std))


# To see two specific columns 
conflict_data %>%
  select(incident_district_num, district_name, district_name_std) %>%
  View()

nlfs_data %>%
  select(dist, district_name, district_name_std) %>%
  distinct() %>%
  View()


# Checking the values labels in the conflict and NLFS data
attributes(conflict_data$district_name)$labels
attributes(nlfs_data$district_name)$labels
attributes(conflict_data$district_name_std)$labels

# Check the value type of the variables
str(conflict_data$district_name)
str(conflict_data$district_name_std)
str(nlfs_data$district_name)

# 5. ADD VALUE LABELS TO THE district_name_std =============================

# Create the labels for district_name_std
dist_codes <- nlfs_data %>%
  select(dist, district_name_std) %>%
  distinct() %>%
  arrange(dist)

# Creating the vector of the district code "dist" and the name of the district "district_name_std"
label_dist <- setNames(dist_codes$dist, dist_codes$district_name_std)

# View label vector
head(label_dist, 10)
length(label_dist)

# Add labels to NLFS data
nlfs_data <- nlfs_data %>%
  left_join(dist_codes %>% rename(district_code = dist),
            by = "district_name_std") %>%
  mutate(district_name_std = labelled(district_code, labels = label_dist)) %>%
  select(-district_code)

# Add labels to Conflict data
conflict_data <- conflict_data %>%
  left_join(dist_codes %>% rename(district_code = dist),
            by = "district_name_std") %>%
  mutate(district_name_std = labelled(district_code, labels = label_dist)) %>%
  select(-district_code)

nlfs_data_clean <- nlfs_data %>%
  select(-district_name)

conflict_data_clean <- conflict_data %>%
  select(-district_name)
# 6. Now merge two datasets =============================

nlfs_conflict_data <- left_join(nlfs_data_clean, conflict_data_clean, by = "district_name_std")

# Check the merged dataset
cat("NLFS observations:", nrow(nlfs_data), "\n")
cat("Conflict Observations:", nrow(conflict_data), "\n")
cat("Merged Observations:", nrow(nlfs_conflict_data), "\n")

# Check how many observations matched
nlfs_conflict_data %>%
  summarize(
    total = n(),
    has_conflict_data = sum(!is.na(incident_district_num))
  )


# 7. SAVE THE MERGED DATASET =============================

# Save in stata format
write_dta(nlfs_conflict_data, file.path(results_path, "nlfs_conflict_data.dta"))

