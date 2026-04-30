# ==============================================================================
# DAY 2: DATA STRUCTURES & DATA WRANGLING FOR PUBLIC HEALTH
# Workshop: Data Analysis for Public Health Using R
# Datasets: infant.xls | lbw.xlsx
# ==============================================================================

library(readxl)
library(tidyverse)

# Load datasets (adjust path if needed)
infant <- read_excel("infant.xls")
lbw    <- read_excel("lbw.xlsx")

# ---- SESSION I: DATA TYPES & VARIABLES ---------------------------------------
# R works with different types of data. Understanding types is essential!

# Check types of our real variables
class(lbw$age)      # age is numeric
class(lbw$smoke)    # smoke is numeric but represents Yes/No
class(lbw$bwt)      # birth weight in grams (numeric)

# The four main data types
num_var  <- 2945.5        # Numeric  : birth weight
text_var <- "Female"      # Character: a label or name
bool_var <- TRUE          # Logical  : TRUE or FALSE
int_var  <- 189L          # Integer  : whole number

# Converting between types
lbw$smoke_text <- ifelse(lbw$smoke == 1, "Smoker", "Non-Smoker")  # number -> text
lbw$low_logical <- as.logical(lbw$low)                             # 0/1 -> TRUE/FALSE

# Check what we created
head(lbw[, c("smoke", "smoke_text", "low", "low_logical")])

# ------------------------------------------------------------------------------
# TASK 1 (Session I):
#   a) What data type is infant$sex? Use class() to check.
#   b) Create a new column in 'infant' called "sex_label" where:
#        0 = "Male",  1 = "Female"  (hint: use ifelse())
#   c) How many unique values are in lbw$race? Use unique() or table()
# ------------------------------------------------------------------------------

# ---- SESSION II: DATA STRUCTURES ---------------------------------------------
# Vectors, factors, and data frames are the building blocks of R.

# VECTOR: a list of values of the same type
birth_weights <- c(2523, 2551, 2557, 2594, 2600, 1330, 1474, 1588, 1588, 1701)
mean(birth_weights)     # Average
length(birth_weights)   # How many?
birth_weights[1]        # First value
birth_weights[birth_weights < 2500]  # All below 2500g (low birth weight)

# FACTOR: categorical variable (important for analysis!)
# Race in lbw: 1 = White, 2 = Black, 3 = Other
lbw$race_factor <- factor(lbw$race,
                           levels = c(1, 2, 3),
                           labels = c("White", "Black", "Other"))

table(lbw$race_factor)    # Frequency table - much more readable!

# Smoke as factor
lbw$smoke_factor <- factor(lbw$smoke,
                            levels = c(0, 1),
                            labels = c("Non-Smoker", "Smoker"))

# Sex in infant dataset (0 = Male, 1 = Female)
infant$sex_factor <- factor(infant$sex,
                             levels = c(0, 1),
                             labels = c("Male", "Female"))

# DATA FRAME: A table (rows = observations, columns = variables)
# Our lbw and infant objects ARE data frames
is.data.frame(lbw)    # Confirm
dim(lbw)              # Rows x Columns

# Access a single column with $
lbw$bwt               # All birth weights
infant$age            # All infant ages

# ------------------------------------------------------------------------------
# TASK 2 (Session II):
#   a) Convert lbw$ht (hypertension, 0/1) to a factor with labels "No"/"Yes"
#   b) Create a vector of the first 10 birth weights from lbw$bwt
#   c) Calculate the median of that vector
#   d) How many infants have age > 30 months? (use infant$age)
# ------------------------------------------------------------------------------

# ---- SESSION III: EXPLORING & CLEANING DATA ----------------------------------
# Before any analysis, explore your data and fix problems!

# Quick exploration
summary(lbw)          # Summary stats for all variables
summary(infant)

str(lbw)              # Structure: data types of each column
glimpse(lbw)          # Tidyverse version of str()

# Checking for missing values (NA)
sum(is.na(lbw))            # Total missing in lbw
colSums(is.na(lbw))        # Missing per column
sum(is.na(infant))
colSums(is.na(infant))

# Checking for duplicates
sum(duplicated(lbw))       # Any duplicate rows?
sum(duplicated(infant))

# Basic frequency tables
table(lbw$smoke_factor)    # Smoker vs Non-Smoker
table(lbw$low)             # Low birth weight (0=No, 1=Yes)
table(infant$sex_factor)   # Sex distribution

# Proportions
prop.table(table(lbw$low)) * 100      # % low birth weight

# ------------------------------------------------------------------------------
# TASK 3 (Session III):
#   a) How many missing values are in the entire infant dataset?
#   b) What percentage of babies in lbw have low birth weight (low == 1)?
#   c) Create a frequency table for lbw$race_factor
#   d) Use summary() on just the lbw$bwt column. What is min and max?
# ------------------------------------------------------------------------------

# ---- SESSION IV: DATA MANIPULATION WITH DPLYR --------------------------------
# dplyr (part of tidyverse) makes data manipulation easy and readable
# Main "verbs": filter(), select(), mutate(), arrange(), summarise(), group_by()

# 1. FILTER: Keep rows that meet a condition
low_bw      <- lbw %>% filter(low == 1)               # Only low birth weight cases
smokers     <- lbw %>% filter(smoke == 1)             # Only smokers
young_moms  <- lbw %>% filter(age < 20)               # Mothers under 20
smoker_lbw  <- lbw %>% filter(smoke == 1 & low == 1)  # Smokers WITH low birth weight

nrow(low_bw)       # How many low birth weight cases?
nrow(young_moms)   # Teen mothers

# 2. SELECT: Keep only specific columns
lbw_key <- lbw %>% select(ID, age, smoke_factor, race_factor, bwt, low)
head(lbw_key)

infant_key <- infant %>% select(id, age, sex_factor, wt1, ht1, wt2)
head(infant_key)

# 3. MUTATE: Create new columns
lbw <- lbw %>%
  mutate(
    bwt_kg       = bwt / 1000,                                  # grams to kg
    lbw_category = ifelse(bwt < 2500, "Low BW", "Normal BW"),  # classify
    age_group    = ifelse(age < 20, "Teen", "Adult")             # age group
  )

infant <- infant %>%
  mutate(
    wt_gain    = wt2 - wt1,                                     # weight gain
    wt_gain_cat = ifelse(wt_gain > 0, "Gained", "No Gain/Lost") # category
  )

# 4. ARRANGE: Sort rows
lbw %>% arrange(bwt) %>% head(5)        # Lightest babies first
lbw %>% arrange(desc(bwt)) %>% head(5)  # Heaviest babies first

# 5. SUMMARISE + GROUP_BY: Summary statistics by group
lbw %>%
  group_by(smoke_factor) %>%
  summarise(
    Count        = n(),
    Avg_BWT      = round(mean(bwt), 1),
    Min_BWT      = min(bwt),
    Max_BWT      = max(bwt),
    LBW_percent  = round(mean(low) * 100, 1)
  )

# Infant weight by sex
infant %>%
  group_by(sex_factor) %>%
  summarise(
    Count    = n(),
    Avg_WT1  = round(mean(wt1), 1),
    Avg_WT2  = round(mean(wt2), 1),
    Avg_gain = round(mean(wt_gain), 1)
  )

# 6. EXPORT cleaned data to Excel (optional)
# library(writexl)
# write_xlsx(lbw, "lbw_cleaned.xlsx")
# write_xlsx(infant, "infant_cleaned.xlsx")

# ------------------------------------------------------------------------------
# TASK 4 (Session IV):
#   a) Filter lbw to only include mothers with hypertension (ht == 1)
#      How many are there?
#   b) Create a new column in infant: "height_category"
#        ht1 < 80 = "Short",  ht1 >= 80 = "Normal"
#   c) Using group_by + summarise, find the average birth weight (bwt)
#      for each race group (race_factor)
#   d) Sort infant by wt1 in descending order and show the top 5 rows
# ------------------------------------------------------------------------------

# ==============================================================================
# END OF DAY 2
# Today you learned:
#   - Data types (numeric, character, logical, factor)
#   - Vectors, factors, and data frames
#   - Exploring data with summary(), str(), table()
#   - Data wrangling: filter, select, mutate, arrange, summarise
# ==============================================================================
