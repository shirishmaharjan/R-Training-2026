# --- MASTER DATA GENERATION ---
if(!require(writexl)) install.packages("writexl")
library(writexl)

set.seed(42)
nepal_master_data <- data.frame(
  Patient_ID = 101:600,
  Province = sample(c("Koshi", "Madhesh", "Bagmati", "Gandaki", "Lumbini", "Karnali", "Sudurpashchim"), 500, replace = TRUE),
  Age = round(runif(500, 18, 75), 0),
  Gender = sample(c("Female", "Male"), 500, replace = TRUE, prob = c(0.55, 0.45)),
  BMI = round(rnorm(500, 22, 4), 1),
  Hemoglobin = round(rnorm(500, 12, 1.5), 1),
  Smoker = sample(c("Yes", "No"), 500, replace = TRUE, prob = c(0.2, 0.8)),
  Vaccinated = sample(c("Yes", "No"), 500, replace = TRUE, prob = c(0.85, 0.15)),
  Systolic_BP = round(rnorm(500, 125, 15), 0),
  ANC_Visits = sample(0:8, 500, replace = TRUE)
)

write_xlsx(nepal_master_data, "Nepal_Health_Data.xlsx")


# Run this once
sample(1:100, 3) 
# Example output: [1] 23 89 12

# Run this again
sample(1:100, 3) 
# Example output: [1] 45 7 66 (Different results)

# Setting the seed to 123
set.seed(123)
sample(1:100, 3)
# Output: [1] 31 79 51

# Re-setting the same seed again
set.seed(123)
sample(1:100, 3)
# Output: [1] 31 79 51 (Identical results)


