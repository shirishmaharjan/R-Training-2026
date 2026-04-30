# ==============================================================================
# DAY 1: GETTING STARTED WITH R & RSTUDIO
# Workshop: Data Analysis for Public Health Using R
# Datasets: infant.xls | lbw.xlsx
# ==============================================================================
# ABOUT THE DATASETS:
#   infant : 200 infant records | variables: id, age, sex, wt1, ht1,
#             water (clean water access), maeduc (mother's education),
#             smeg (smegma score), wt2 (follow-up weight)
#   lbw    : 189 birth records  | variables: ID, low (low birth weight 0/1),
#             smoke, race, age, lwt (last weight), ptl (premature labor),
#             ht (hypertension), ui (uterine irritability), ftv
#             (first trimester visits), bwt (birth weight in grams)
# ==============================================================================

# ---- SESSION I: WHY R? -------------------------------------------------------
# R is FREE, open-source, reproducible, and widely used in public health.
# This session is a facilitated discussion. No code needed here.
# Key reasons to use R:
#   1. It is free (unlike SPSS or STATA)
#   2. Handles data cleaning, statistics, AND graphs all in one place
#   3. Used by WHO, CDC, and research institutions worldwide
#   4. Scripts make your analysis reproducible and transparent

# ---- SESSION II: RSTUDIO TOUR & BASIC ARITHMETIC ----------------------------
# RStudio has 4 panels:
#   Top-Left  : Script Editor  (where you write code to save)
#   Bottom-Left: Console       (where results appear)
#   Top-Right : Environment    (shows your data objects)
#   Bottom-Right: Files/Plots/Help

# R as a calculator - try running these lines one by one
100 + 250          # Addition
1000 - 375         # Subtraction
60 * 7             # Multiplication
189 / 3            # Division
2^8                # Power / Exponent
sqrt(144)          # Square root
(10 + 20) * 3      # Brackets first

# Public health examples
total_births    <- 189          # Total records in lbw dataset
low_bw_cases    <- 59           # Approximate low birth weight cases
lbw_percent     <- (low_bw_cases / total_births) * 100
lbw_percent                     # Print the result

# Creating objects (variables)
study_name      <- "Low Birth Weight Study"
sample_size     <- 189
avg_bwt_grams   <- 2945

# Print them
study_name
sample_size
avg_bwt_grams

# ------------------------------------------------------------------------------
# TASK 1 (Session II):
#   The infant study has 200 infants. 102 are male (sex = 0).
#   a) Create an object called "total_infants" with value 200
#   b) Create an object called "male_infants" with value 102
#   c) Calculate and print the percentage of male infants
# ------------------------------------------------------------------------------

# ---- SESSION III: R HELP SYSTEM ----------------------------------------------
# When you are stuck, use the Help system!

?mean              # Help for mean function
?sum               # Help for sum
?table             # Help for frequency tables
help("read.csv")   # Another way to get help

# The Help panel (bottom-right) will show the documentation.
# Look for: Description, Usage, Arguments, Examples

# Use Tab key for auto-complete:
#   Type: me  and press Tab --> R suggests mean(), median(), etc.

# ------------------------------------------------------------------------------
# TASK 2 (Session III):
#   a) Open the help page for "median"
#   b) Open the help page for "cor.test"
#   c) What does the argument "na.rm = TRUE" do? Look it up in ?mean
# ------------------------------------------------------------------------------

# ---- SESSION IV: PACKAGES & LOADING DATA ------------------------------------
# Packages are like "apps" that add extra features to R.
# You install ONCE, but load every session.

# Step 1: Install packages (remove # to run, only needed once!)
# install.packages("readxl")    # For reading Excel files
# install.packages("tidyverse") # Collection of data tools
# install.packages("writexl")   # For saving Excel files

# Step 2: Load the packages (run every time you start R)
library(readxl)
library(tidyverse)

# Step 3: Set your working directory
# This tells R where your data files are saved.
# Go to: Session > Set Working Directory > Choose Directory
# OR use setwd() with your own path, example:
# setwd("C:/Users/YourName/Desktop/RWorkshop")

getwd()  # Check current directory

# Step 4: Load the datasets
# IMPORTANT: Make sure infant.xls and lbw.xlsx are in your working directory!

infant <- read_excel("infant.xls")   # Load infant dataset
infant <- read_excel("C:/Users/Shirish Maharjan/OneDrive - HERD/Herd/R Training 2026/Data/infant.xls")   # Load infant dataset

lbw    <- read_excel("lbw.xlsx")     # Load lbw dataset
lbw    <- read_excel("C:/Users/Shirish Maharjan/OneDrive - HERD/Herd/R Training 2026/Data/lbw.xlsx")     # Load lbw dataset

# Step 5: First look at the data
head(infant)        # First 6 rows of infant data
head(lbw)           # First 6 rows of lbw data

nrow(infant)        # How many rows (observations)?
ncol(infant)        # How many columns (variables)?
names(infant)       # What are the variable names?

nrow(lbw)
ncol(lbw)
names(lbw)

# ------------------------------------------------------------------------------
# TASK 3 (Session IV):
#   a) Load both datasets and check their dimensions using dim()
#   b) Use str() to see the data types of each variable in "lbw"
#   c) How many variables does the infant dataset have?
#   d) What does the variable "bwt" likely represent? Look at the values.
# ------------------------------------------------------------------------------

# ==============================================================================
# END OF DAY 1
# Well done! You have learned:
#   - Basic R arithmetic and object creation
#   - How to use the Help system
#   - How to install and load packages
#   - How to load Excel data into R
# ==============================================================================
