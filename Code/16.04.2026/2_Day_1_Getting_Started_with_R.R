# ==============================================================================
# DAY 1: INTRODUCTION TO R & RSTUDIO
# ==============================================================================

# --- Session I: Introduction to Open-Source ---
# Discussion: Why R? (Reproducibility, Cost, Community)

# --- Session II: RStudio & Basic Arithmetic ---
# The Console is your calculator
150 + 250   # Hospital expenses
500 / 22    # Dosage calculation
2^3         # Exponents

# Assignment: Creating your first object
nepal_population_millions <- 30.5
doctor_count <- 1500
doctors_per_million <- doctor_count / nepal_population_millions
doctors_per_million

# --- Session III: R Help System ---
# How to find help when you are stuck
?mean
?sum
help("data.frame")

# EXERCISE 1: Find the help page for "median". 
# What is the default behavior for 'na.rm'?

# --- Session IV: Packages & Installation ---
# Packages are 'apps' for R. 
# We need 'tidyverse' for data and 'readxl' for Excel.

# install.packages("tidyverse") # Run this only once
# install.packages("readxl")

library(tidyverse)
library(readxl)

# Check your working directory
getwd() 
# Set it: Session > Set Working Directory > Choose Directory