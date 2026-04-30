# ==============================================================================
# DAY 2: VARIABLES, DATA STRUCTURES & MANIPULATION
# ==============================================================================
library(readxl)
library(tidyverse)

# --- Session I: Variables & Conversion ---
age <- "25"         # This is text (Character)
age_num <- as.numeric(age) # Convert to number
is_anemic <- TRUE   # Logical

# --- Session II: Data Structures ---
# Vector: Ages of 5 patients in a ward
patient_ages <- c(22, 45, 30, 19, 55)

# Factor: Categorical data (Levels matter in Health)
province_factor <- factor(c("Bagmati", "Karnali", "Bagmati"), 
                          levels = c("Koshi", "Madhesh", "Bagmati", "Gandaki", "Lumbini", "Karnali", "Sudurpashchim"))

# --- Session III: Importing & Exporting Excel ---
# IMPORT the dummy data we created
#health_data <- read_excel("Nepal_Health_Data.xlsx")
health_data <- read_excel("C:/Users/Shirish Maharjan/OneDrive - HERD/Herd/R Training 2026/Data/Nepal_Health_Data.xlsx")


# Explore the data
head(health_data)    # First 6 rows
str(health_data)     # Structure of data
summary(health_data) # Quick stats

# --- Session IV: Data Manipulation (dplyr) ---
# Goal: Find all female patients in Karnali with low Hemoglobin (< 11)

# Example 1: Filtering
karnali_risk <- health_data %>%
  filter(Province == "Karnali" & Gender == "Female" & Hemoglobin < 11)

# Example 2: Selecting columns
subset_data <- health_data %>%
  select(Patient_ID, Province, BMI)

# Example 3: Creating new variables (Mutate)
health_data <- health_data %>%
  mutate(Anemia_Status = ifelse(Hemoglobin < 12, "Anemic", "Normal"),
         BP_Category = ifelse(Systolic_BP > 140, "Hypertension", "Normal"))

# EXPORT back to Excel
# write_xlsx(health_data, "Health_Data_With_Diagnosis.xlsx")