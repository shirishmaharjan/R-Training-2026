# ==============================================================================
# DAY 3: STATISTICAL ANALYSIS
# ==============================================================================
library(readxl)
library(tidyverse)
#data <- read_excel("Nepal_Health_Data.xlsx")
data <- read_excel("C:/Users/Shirish Maharjan/OneDrive - HERD/Herd/R Training 2026/Data/Nepal_Health_Data.xlsx")


# --- Session I: Central Tendency ---
# Mean age of participants
mean(data$Age)
# Median BMI
median(data$BMI)
# Standard Deviation of BP
sd(data$Systolic_BP)

# Summary table by Province
province_summary <- data %>%
  group_by(Province) %>%
  summarise(Avg_Age = mean(Age),
            Avg_Hemoglobin = mean(Hemoglobin),
            Count = n())

# --- Session II: Hypothesis Testing & Regression ---
# T-Test: Is there a difference in BMI between Smoker vs Non-Smoker?
t.test(BMI ~ Smoker, data = data)

# Linear Regression: Does Age predict Systolic BP?
bp_model <- lm(Systolic_BP ~ Age, data = data)
summary(bp_model)

# --- Session III: Correlation, Chi-Square & ANOVA ---
# Correlation: BMI vs Systolic BP
cor.test(data$BMI, data$Systolic_BP)

# Chi-Square: Is Vaccination status associated with Province?
table_vaccine <- table(data$Vaccinated, data$Province)
chisq.test(table_vaccine)

# ANOVA: Does ANC Visits count differ by Province?
anova_res <- aov(ANC_Visits ~ Province, data = data)
summary(anova_res)

# --- Session IV: Parametric vs Non-Parametric ---
# Check for Normality
shapiro.test(data$Hemoglobin[1:50]) # If p < 0.05, data is not normal

# Non-parametric version of t-test (Wilcoxon)
wilcox.test(Hemoglobin ~ Smoker, data = data)
