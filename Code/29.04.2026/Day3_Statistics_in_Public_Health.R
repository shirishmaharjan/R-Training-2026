# ==============================================================================
# DAY 3: STATISTICS IN PUBLIC HEALTH
# Workshop: Data Analysis for Public Health Using R
# Datasets: infant.xls | lbw.xlsx
# ==============================================================================

library(readxl)
library(tidyverse)

# Load data
infant <- read_excel("infant.xls")
lbw    <- read_excel("lbw.xlsx")

# Create factor variables (as done in Day 2)
lbw <- lbw %>%
  mutate(
    smoke_factor = factor(smoke, levels = c(0,1), labels = c("Non-Smoker","Smoker")),
    race_factor  = factor(race,  levels = c(1,2,3), labels = c("White","Black","Other")),
    ht_factor    = factor(ht,    levels = c(0,1), labels = c("No HT","Hypertension")),
    low_factor   = factor(low,   levels = c(0,1), labels = c("Normal BW","Low BW")),
    bwt_kg       = bwt / 1000
  )

infant <- infant %>%
  mutate(
    sex_factor   = factor(sex, levels = c(0,1), labels = c("Male","Female")),
    water_factor = factor(water, levels = c(0,1), labels = c("No Clean Water","Clean Water")),
    wt_gain      = wt2 - wt1
  )

# ---- SESSION I: DESCRIPTIVE STATISTICS ---------------------------------------
# Summarise the data BEFORE jumping to tests.
# Always answer: What does my data look like?

# Central tendency
mean(lbw$bwt)           # Mean birth weight (grams)
median(lbw$bwt)         # Median birth weight
mean(infant$wt1)        # Mean weight at baseline (infants)

# Spread
sd(lbw$bwt)             # Standard deviation of birth weight
var(lbw$bwt)            # Variance
range(lbw$bwt)          # Min and Max
IQR(lbw$bwt)            # Interquartile range (Q3 - Q1)

# Quantiles
quantile(lbw$bwt)                      # 0%, 25%, 50%, 75%, 100%
quantile(lbw$bwt, probs = c(0.1, 0.9)) # 10th and 90th percentile

# Full summary
summary(lbw$bwt)
summary(infant$wt1)

# Grouped descriptive stats
lbw %>%
  group_by(smoke_factor) %>%
  summarise(
    n           = n(),
    Mean_BWT    = round(mean(bwt), 1),
    SD_BWT      = round(sd(bwt), 1),
    Median_BWT  = median(bwt),
    LBW_n       = sum(low),
    LBW_pct     = round(mean(low)*100, 1)
  )

infant %>%
  group_by(sex_factor) %>%
  summarise(
    n         = n(),
    Mean_WT1  = round(mean(wt1), 1),
    Mean_WT2  = round(mean(wt2), 1),
    Mean_gain = round(mean(wt_gain), 1),
    SD_gain   = round(sd(wt_gain), 1)
  )

# Frequency tables for categorical variables
table(lbw$smoke_factor)
table(lbw$race_factor)
prop.table(table(lbw$low_factor)) * 100  # Percentage low birth weight

# Cross-tabulation
table(lbw$smoke_factor, lbw$low_factor)

# ------------------------------------------------------------------------------
# TASK 1 (Session I):
#   a) Calculate the mean, median, and SD of infant$ht1 (height)
#   b) What is the IQR of lbw$lwt (last weight before delivery)?
#   c) Make a grouped summary of infant data by sex_factor showing
#      average ht1, wt1, and wt2
#   d) What percentage of infants had access to clean water (water == 1)?
# ------------------------------------------------------------------------------

# ---- SESSION II: NORMALITY & CHOOSING THE RIGHT TEST -------------------------
# Before doing a t-test or ANOVA, check if your data is normally distributed.

# Shapiro-Wilk test for normality
# H0: Data is normally distributed
# If p < 0.05: NOT normal -> use non-parametric test
# If p >= 0.05: Normal distribution -> can use parametric test

shapiro.test(lbw$bwt)          # Is birth weight normally distributed?
shapiro.test(infant$wt1)       # Is infant weight normally distributed?
shapiro.test(infant$wt_gain)   # Is weight gain normally distributed?

# Interpretation guide:
# p-value >= 0.05: assume normality (use t-test, ANOVA, correlation)
# p-value < 0.05 : not normal (use Wilcoxon, Kruskal-Wallis, Spearman)

# Visual check - histogram (quick)
hist(lbw$bwt,   main = "Distribution of Birth Weight", xlab = "Birth Weight (g)")
hist(infant$wt1, main = "Distribution of Infant Weight", xlab = "Weight")

# ------------------------------------------------------------------------------
# TASK 2 (Session II):
#   a) Run shapiro.test on lbw$age (mother's age). Is it normal?
#   b) Run shapiro.test on infant$ht1 (infant height). Is it normal?
#   c) Based on your results, would you use a parametric or
#      non-parametric test to compare height between male and female infants?
# ------------------------------------------------------------------------------

# ---- SESSION III: HYPOTHESIS TESTING -----------------------------------------
# The golden rule: set your hypothesis BEFORE looking at results.
# H0 = Null hypothesis (no difference, no effect)
# H1 = Alternative hypothesis (there IS a difference/effect)

# --- Independent Samples T-Test ---
# Question: Is birth weight (bwt) different between smokers and non-smokers?
# H0: No difference in mean BWT between smokers and non-smokers
# H1: There IS a difference

t.test(bwt ~ smoke_factor, data = lbw)
# Read the output: t statistic, df, p-value, 95% CI, group means

# Question: Is infant weight at baseline (wt1) different by sex?
t.test(wt1 ~ sex_factor, data = infant)

# --- Paired T-Test ---
# Question: Did infant weight change significantly from wt1 to wt2?
# Same infants measured twice -> paired test!
t.test(infant$wt1, infant$wt2, paired = TRUE)
# H0: Mean weight gain = 0

# --- Wilcoxon Test (Non-Parametric t-test alternative) ---
# Use when data is NOT normally distributed
wilcox.test(bwt ~ smoke_factor, data = lbw)
wilcox.test(infant$wt1, infant$wt2, paired = TRUE)  # Non-parametric paired

# --- Chi-Square Test ---
# Question: Is smoking associated with low birth weight?
# Both variables are categorical -> Chi-Square
chisq_table <- table(lbw$smoke_factor, lbw$low_factor)
chisq_table
chisq.test(chisq_table)
# p < 0.05: there IS a significant association

# Question: Is clean water access associated with sex in infant dataset?
chisq.test(table(infant$water_factor, infant$sex_factor))

# ------------------------------------------------------------------------------
# TASK 3 (Session III):
#   a) Is birth weight (bwt) significantly different by hypertension status (ht)?
#      Run a t.test and interpret: what is the p-value? What does it mean?
#   b) Is there a significant association between race (race_factor) and
#      low birth weight (low_factor)? Run a chi-square test.
#   c) Did infant height (ht1) differ significantly by sex? Run t.test.
#   d) Run a paired t.test to check if lbw mothers' weight (lwt) differs
#      from average birth weight -- does this make conceptual sense?
# ------------------------------------------------------------------------------

# ---- SESSION IV: ANOVA, CORRELATION & SIMPLE REGRESSION ---------------------

# --- One-Way ANOVA ---
# Question: Does birth weight differ across racial groups?
# H0: All group means are equal
anova_bwt <- aov(bwt ~ race_factor, data = lbw)
summary(anova_bwt)
# If p < 0.05: at least one group is different -> run post-hoc test

# Post-hoc: Tukey's HSD (which groups differ?)
TukeyHSD(anova_bwt)

# Kruskal-Wallis (non-parametric ANOVA)
kruskal.test(bwt ~ race_factor, data = lbw)

# --- Pearson Correlation ---
# Question: Is mother's weight (lwt) correlated with baby's weight (bwt)?
# H0: No linear correlation (r = 0)
cor.test(lbw$lwt, lbw$bwt, method = "pearson")
# r close to 1 or -1 = strong; near 0 = weak

# Spearman Correlation (non-parametric)
cor.test(lbw$lwt, lbw$bwt, method = "spearman")

# Infant: correlation between height (ht1) and weight (wt1)
cor.test(infant$ht1, infant$wt1, method = "pearson")

# --- Simple Linear Regression ---
# Question: Does mother's age predict baby's birth weight?
# Y (outcome) = birth weight; X (predictor) = age
model_lbw <- lm(bwt ~ age, data = lbw)
summary(model_lbw)
# Read: Estimate (slope), p-value for age, R-squared

# For infants: does age (in months) predict weight gain?
model_infant <- lm(wt_gain ~ age, data = infant)
summary(model_infant)

# Multiple Regression: more than one predictor
model_multi <- lm(bwt ~ age + lwt + smoke, data = lbw)
summary(model_multi)

# ------------------------------------------------------------------------------
# TASK 4 (Session IV):
#   a) Run ANOVA: Does infant weight at follow-up (wt2) differ by
#      mother's education level (maeduc)? Check the p-value.
#   b) Calculate Pearson correlation between infant age and wt1.
#      Is it positive or negative? Strong or weak?
#   c) Build a simple linear regression: wt2 ~ wt1 (in infant data)
#      What is the R-squared? What does it tell you?
#   d) CHALLENGE: Add age as a second predictor to the model above.
#      Did R-squared improve?
# ------------------------------------------------------------------------------

# ==============================================================================
# END OF DAY 3
# Today you learned:
#   - Descriptive statistics (mean, median, SD, IQR)
#   - Normality testing with Shapiro-Wilk
#   - T-tests (independent, paired) and Wilcoxon alternatives
#   - Chi-square test for categorical associations
#   - ANOVA, Tukey post-hoc, Kruskal-Wallis
#   - Correlation (Pearson & Spearman)
#   - Simple and multiple linear regression
# ==============================================================================
