# ==============================================================================
# DAY 5: LOGISTIC REGRESSION, REPORTING & GROUP PROJECT
# Workshop: Data Analysis for Public Health Using R
# Datasets: infant.xls | lbw.xlsx
# ==============================================================================
# DAY 5 STRUCTURE:
#   Session I   : Logistic Regression (lbw dataset)
#   Session II  : Creating Summary Tables & Reports
#   Session III : Putting It All Together (combined workflow)
#   Session IV  : GROUP WORK PROJECT (see end of script)
# ==============================================================================

library(readxl)
library(tidyverse)

# Install these if you haven't yet (remove the # to run once)
# install.packages("gtsummary")   # Beautiful summary tables
# install.packages("broom")       # Tidy statistical output
# install.packages("writexl")     # Export to Excel

library(broom)       # Tidies model output into data frames

# Load data
infant <- read_excel("infant.xls")
lbw    <- read_excel("lbw.xlsx")

# Prepare variables (from Day 2)
lbw <- lbw %>%
  mutate(
    smoke_factor = factor(smoke, levels=c(0,1), labels=c("Non-Smoker","Smoker")),
    race_factor  = factor(race,  levels=c(1,2,3), labels=c("White","Black","Other")),
    ht_factor    = factor(ht,    levels=c(0,1), labels=c("No HT","Hypertension")),
    ui_factor    = factor(ui,    levels=c(0,1), labels=c("No UI","Uterine Irritability")),
    low_factor   = factor(low,   levels=c(0,1), labels=c("Normal BW","Low BW")),
    bwt_kg       = bwt / 1000,
    age_group    = ifelse(age < 25, "Under 25", "25 and above")
  )

infant <- infant %>%
  mutate(
    sex_factor   = factor(sex,   levels=c(0,1), labels=c("Male","Female")),
    water_factor = factor(water, levels=c(0,1), labels=c("No","Yes")),
    wt_gain      = wt2 - wt1,
    gained_weight = ifelse(wt_gain > 0, 1, 0)  # binary outcome for logistic
  )

# ---- SESSION I: LOGISTIC REGRESSION ------------------------------------------
# Use logistic regression when your OUTCOME is binary (Yes/No, 0/1).
# Examples: Low birth weight (yes/no), Disease (yes/no), Death (yes/no)

# --- SIMPLE LOGISTIC REGRESSION ---
# Question: Does maternal smoking predict low birth weight?
# Outcome (Y): low (0 = normal, 1 = low birth weight)
# Predictor (X): smoke (0/1)

model1 <- glm(low ~ smoke, data = lbw, family = binomial)
summary(model1)

# The output shows:
# Estimate    : log(odds ratio) - can be hard to interpret
# Pr(>|z|)    : p-value
# Convert to Odds Ratios (OR) - easier to interpret in public health!
exp(coef(model1))          # Odds Ratios
exp(confint(model1))       # 95% Confidence Intervals

# How to interpret OR:
# OR > 1 : risk factor (increases odds of low BW)
# OR < 1 : protective factor (decreases odds)
# OR = 1 : no association
# OR for smoking ~ 2.0 means smokers have ~2x higher odds of low BW

# Tidy version using broom (easier to read!)
tidy(model1, exponentiate = TRUE, conf.int = TRUE)

# --- MULTIPLE LOGISTIC REGRESSION ---
# Question: What factors predict low birth weight?
# Controlling for: age, smoking, race, hypertension, uterine irritability

model2 <- glm(low ~ age + smoke + race + ht + ui + lwt,
              data   = lbw,
              family = binomial)
summary(model2)

# Odds ratios with 95% CI for all predictors
or_table <- tidy(model2, exponentiate = TRUE, conf.int = TRUE) %>%
  select(term, estimate, conf.low, conf.high, p.value) %>%
  mutate(across(c(estimate, conf.low, conf.high), round, 2),
         p.value = round(p.value, 3),
         Significant = ifelse(p.value < 0.05, "Yes *", "No"))

print(or_table)

# Model fit: AIC (lower = better fit between models)
AIC(model1)
AIC(model2)

# --- LOGISTIC REGRESSION ON INFANT DATA ---
# Question: Does access to clean water predict weight gain?
# Outcome: gained_weight (1 = gained, 0 = did not gain)

model3 <- glm(gained_weight ~ water + sex + age + wt1,
              data   = infant,
              family = binomial)
summary(model3)
tidy(model3, exponentiate = TRUE, conf.int = TRUE)

# ------------------------------------------------------------------------------
# TASK 1 (Session I):
#   a) Run a simple logistic regression: low ~ ht (hypertension)
#      What is the OR for hypertension? Is it significant?
#   b) Add race to the model from (a). Does the OR for ht change?
#      What does this tell you about confounding?
#   c) In the infant dataset: run logistic regression
#      gained_weight ~ sex + age
#      Which variable is a significant predictor?
#   d) Compare AIC of model with and without age. Which is better?
# ------------------------------------------------------------------------------

# ---- SESSION II: CREATING SUMMARY TABLES & CLEAN OUTPUT ---------------------
# Good research communicates results clearly in tables.

# --- Method 1: Manual summary table using dplyr ---
# Descriptive table: lbw dataset
descriptive_lbw <- lbw %>%
  summarise(
    `n`                  = n(),
    `Mean Age (SD)`      = paste0(round(mean(age),1), " (", round(sd(age),1), ")"),
    `Mean BWT (SD)`      = paste0(round(mean(bwt),1), " (", round(sd(bwt),1), ")"),
    `Smokers n (%)`      = paste0(sum(smoke), " (", round(mean(smoke)*100,1), "%)"),
    `Low BW n (%)`       = paste0(sum(low),   " (", round(mean(low)*100,1),   "%)"),
    `Hypertension n (%)` = paste0(sum(ht),    " (", round(mean(ht)*100,1),    "%)")
  )
print(descriptive_lbw)

# By smoking group
lbw %>%
  group_by(smoke_factor) %>%
  summarise(
    n            = n(),
    Mean_Age     = round(mean(age), 1),
    Mean_BWT_g   = round(mean(bwt), 0),
    LBW_pct      = round(mean(low)*100, 1),
    HT_pct       = round(mean(ht)*100, 1)
  )

# --- Method 2: OR table formatted for report ---
final_or_table <- tidy(model2, exponentiate = TRUE, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    OR    = round(estimate, 2),
    CI    = paste0("(", round(conf.low,2), ", ", round(conf.high,2), ")"),
    p     = round(p.value, 3)
  ) %>%
  select(Predictor = term, OR, `95% CI` = CI, `p-value` = p)
print(final_or_table)

# Save to CSV (easy to copy into Word/Excel)
write.csv(final_or_table, "logistic_regression_results.csv", row.names = FALSE)

# --- Visualization of Odds Ratios (Forest Plot style) ---
or_plot_data <- tidy(model2, exponentiate = TRUE, conf.int = TRUE) %>%
  filter(term != "(Intercept)")

ggplot(or_plot_data, aes(x = estimate, y = term)) +
  geom_point(size = 3, color = "darkblue") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  labs(title    = "Odds Ratios: Predictors of Low Birth Weight",
       subtitle = "Vertical dashed line = OR of 1 (no effect)",
       x        = "Odds Ratio (95% CI)",
       y        = "Predictor") +
  theme_classic()

# ------------------------------------------------------------------------------
# TASK 2 (Session II):
#   a) Create a descriptive summary table for the infant dataset showing:
#      n, mean age, mean wt1, mean wt2, mean wt_gain, % male, % clean water
#   b) Make a summary table grouped by sex_factor
#   c) Using model3 (infant logistic regression), create a formatted OR table
#      and save it as "infant_logistic_results.csv"
#   d) Plot the ORs from model3 as a forest plot
# ------------------------------------------------------------------------------

# ---- SESSION III: COMPLETE WORKFLOW ------------------------------------------
# Putting everything together: a mini-analysis from start to finish.

# STEP 1: Load and clean data
lbw_analysis <- read_excel("lbw.xlsx") %>%
  mutate(
    smoke_f  = factor(smoke, levels=c(0,1), labels=c("Non-Smoker","Smoker")),
    race_f   = factor(race,  levels=c(1,2,3), labels=c("White","Black","Other")),
    ht_f     = factor(ht,    levels=c(0,1), labels=c("No","Yes")),
    low_f    = factor(low,   levels=c(0,1), labels=c("Normal","Low BW")),
    bwt_kg   = bwt / 1000
  )

# STEP 2: Descriptive summary by outcome group
lbw_analysis %>%
  group_by(low_f) %>%
  summarise(
    n          = n(),
    Pct        = round(n()/nrow(lbw_analysis)*100, 1),
    Mean_Age   = round(mean(age), 1),
    Mean_lwt   = round(mean(lwt), 1),
    Smoke_pct  = round(mean(smoke)*100, 1),
    HT_pct     = round(mean(ht)*100, 1),
    Mean_BWT_g = round(mean(bwt), 0)
  )

# STEP 3: Statistical test (chi-square for categorical)
chisq.test(table(lbw_analysis$smoke_f, lbw_analysis$low_f))

# STEP 4: Logistic regression
final_model <- glm(low ~ age + smoke + race + ht + lwt,
                   data=lbw_analysis, family=binomial)

# STEP 5: Forest plot of results
tidy(final_model, exponentiate=TRUE, conf.int=TRUE) %>%
  filter(term != "(Intercept)") %>%
  ggplot(aes(x=estimate, y=reorder(term, estimate))) +
  geom_point(size=3, color="darkblue") +
  geom_errorbarh(aes(xmin=conf.low, xmax=conf.high), height=0.2) +
  geom_vline(xintercept=1, linetype="dashed", color="red") +
  labs(title="Predictors of Low Birth Weight: Odds Ratios",
       x="OR (95% CI)", y="Variable") +
  theme_classic()

# STEP 6: Save results
ggsave("day5_forest_plot.png", dpi=300, width=8, height=5)

# ------------------------------------------------------------------------------
# TASK 3 (Session III):
#   Run the same complete workflow for the infant dataset:
#   a) Outcome: gained_weight (did the infant gain weight?)
#   b) Descriptive summary by gained_weight group
#   c) Chi-square: water access vs weight gain
#   d) Logistic regression: gained_weight ~ water + sex + age + wt1
#   e) Forest plot of ORs
#   f) Save your forest plot as "infant_analysis_forest.png"
# ------------------------------------------------------------------------------

# ==============================================================================
# SESSION IV: GROUP WORK PROJECT
# ==============================================================================
# INSTRUCTIONS:
#   - Divide into groups of 3-4 participants
#   - Each group picks ONE dataset: infant OR lbw
#   - Complete the analysis steps below and present your findings
#   - Presentation: 10 minutes per group
# ==============================================================================

# --- GROUP WORK TEMPLATE (fill in your code below each step) ---

# GROUP NUMBER: ___
# DATASET CHOSEN: ___  (infant or lbw)
# RESEARCH QUESTION: Write ONE research question your group will answer.
# Example: "Is maternal smoking associated with low birth weight after
#            controlling for race and hypertension?"

# STEP 1: Load and prepare your dataset
# (Add factor labels for all categorical variables)



# STEP 2: Descriptive Statistics
# Summarize key variables. Include at least:
# - Sample size, mean/SD for continuous variables
# - Frequency/percentage for categorical variables



# STEP 3: Exploratory Visualization
# Create at least 2 meaningful plots:
# Plot 1: Distribution of your main outcome
# Plot 2: Relationship between exposure and outcome



# STEP 4: Statistical Test
# Choose the appropriate test based on your variables:
# - Two groups, continuous outcome -> t.test()
# - Two categorical variables -> chisq.test()
# - Continuous outcome, multiple groups -> aov()
# State your H0 and H1 before running!



# STEP 5: Logistic Regression (if outcome is binary)
#      OR Linear Regression (if outcome is continuous)
# Include at least 2-3 predictors.
# Report: OR (or Beta), 95% CI, p-value, interpretation



# STEP 6: Summary Table
# Create a clean table summarizing your main findings
# Save it as a CSV file



# STEP 7: Forest Plot or Final Visualization
# Create one final polished plot that communicates your key finding
# Save it as a high-resolution PNG (dpi=300)
# This is what you will show during your presentation!



# ==============================================================================
# GROUP PRESENTATION CHECKLIST:
#   [ ] Research question stated clearly
#   [ ] Dataset described (n, key variables)
#   [ ] At least 2 plots shown
#   [ ] Statistical test result reported (test statistic, p-value)
#   [ ] Regression table with OR/Beta and 95% CI
#   [ ] One clear conclusion sentence
# ==============================================================================

# ==============================================================================
# END OF DAY 5 — CONGRATULATIONS!
# Over 5 days you have learned:
#   Day 1: R basics, RStudio, loading data
#   Day 2: Data types, wrangling, dplyr verbs
#   Day 3: Descriptive stats, t-tests, chi-square, ANOVA, correlation, regression
#   Day 4: ggplot2 visualization — histograms, boxplots, scatter, facets
#   Day 5: Logistic regression, summary tables, forest plots, full workflow
#
# NEXT STEPS after this workshop:
#   - Practice with your own datasets
#   - Explore: gtsummary, ggpubr, rmarkdown, shiny
#   - Community: stackoverflow.com, r-bloggers.com
#   - Books: "R for Data Science" (free at r4ds.had.co.nz)
# ==============================================================================
