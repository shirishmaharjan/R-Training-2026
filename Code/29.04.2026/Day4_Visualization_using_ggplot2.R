# ==============================================================================
# DAY 4: DATA VISUALIZATION USING GGPLOT2
# Workshop: Data Analysis for Public Health Using R
# Datasets: infant.xls | lbw.xlsx
# ==============================================================================

library(readxl)
library(tidyverse)   # includes ggplot2

# Load and prepare data
infant <- read_excel("infant.xls")
lbw    <- read_excel("lbw.xlsx")

# Create factor labels (from Day 2)
lbw <- lbw %>%
  mutate(
    smoke_factor = factor(smoke, levels=c(0,1), labels=c("Non-Smoker","Smoker")),
    race_factor  = factor(race,  levels=c(1,2,3), labels=c("White","Black","Other")),
    low_factor   = factor(low,   levels=c(0,1), labels=c("Normal BW","Low BW")),
    ht_factor    = factor(ht,    levels=c(0,1), labels=c("No HT","Hypertension")),
    bwt_kg       = bwt / 1000
  )

infant <- infant %>%
  mutate(
    sex_factor    = factor(sex,   levels=c(0,1), labels=c("Male","Female")),
    water_factor  = factor(water, levels=c(0,1), labels=c("No Clean Water","Clean Water")),
    wt_gain       = wt2 - wt1
  )

# ---- SESSION I: INTRODUCTION TO GGPLOT2 -------------------------------------
# ggplot2 builds plots in LAYERS using a consistent grammar:
#
#   ggplot(data, aes(x = var1, y = var2)) +   <- Layer 1: Canvas + axes
#     geom_point()                            <- Layer 2: What to draw
#     labs(title = "...", x = "...", y="...") <- Layer 3: Labels
#     theme_classic()                         <- Layer 4: Look/feel
#
# Think of it like building a sandwich: each + adds a new layer.

# The simplest possible plot (just to see the structure)
ggplot(lbw, aes(x = bwt)) +
  geom_histogram()

# Now make it look better step by step
ggplot(lbw, aes(x = bwt)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 20) +
  labs(title = "Distribution of Birth Weight",
       x = "Birth Weight (grams)",
       y = "Number of Babies") +
  theme_classic()

# aes() = aesthetics: maps variables to visual properties
# fill  = inside color
# color = border color
# bins  = number of bars

# ------------------------------------------------------------------------------
# TASK 1 (Session I):
#   a) Create a histogram of infant$wt1 (baseline weight)
#   b) Make the bars green with white borders
#   c) Add a proper title and axis labels
#   d) Try theme_bw() instead of theme_classic(). What changes?
# ------------------------------------------------------------------------------

# ---- SESSION II: BASIC PLOT TYPES -------------------------------------------

# --- 1. HISTOGRAM: Distribution of a continuous variable ---
ggplot(lbw, aes(x = bwt)) +
  geom_histogram(binwidth = 250, fill = "coral", color = "white") +
  labs(title = "Birth Weight Distribution",
       subtitle = paste("n =", nrow(lbw), "births"),
       x = "Birth Weight (grams)",
       y = "Frequency") +
  theme_minimal()

# --- 2. BAR CHART: Frequency of a categorical variable ---
ggplot(lbw, aes(x = race_factor)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribution of Race in LBW Study",
       x = "Race",
       y = "Count") +
  theme_classic()

# --- 3. BOXPLOT: Compare groups ---
ggplot(lbw, aes(x = smoke_factor, y = bwt, fill = smoke_factor)) +
  geom_boxplot() +
  labs(title = "Birth Weight by Smoking Status",
       x = "Smoking Status",
       y = "Birth Weight (grams)",
       fill = "Smoking Status") +
  theme_classic()

# --- 4. SCATTER PLOT: Relationship between two continuous variables ---
ggplot(lbw, aes(x = lwt, y = bwt)) +
  geom_point(alpha = 0.5, color = "darkblue") +
  labs(title = "Mother's Weight vs Baby's Birth Weight",
       x = "Mother's Last Weight (lbs)",
       y = "Baby's Birth Weight (grams)") +
  theme_classic()

# ------------------------------------------------------------------------------
# TASK 2 (Session II):
#   a) Create a bar chart showing count of low birth weight vs normal
#      (use low_factor as x-axis). Use different fill colors.
#   b) Make a boxplot comparing infant wt1 by sex_factor
#   c) Create a scatter plot: infant age (x) vs wt1 (y)
#   d) Change the point color to "tomato" and set alpha = 0.6
# ------------------------------------------------------------------------------

# ---- SESSION III: CUSTOMIZING PLOTS ------------------------------------------
# Good public health graphics communicate clearly and look professional.

# Grouped bar chart (two categorical variables)
ggplot(lbw, aes(x = race_factor, fill = low_factor)) +
  geom_bar(position = "dodge") +  # "dodge" = side by side; "fill" = proportional
  labs(title  = "Low Birth Weight Status by Race",
       x      = "Race",
       y      = "Count",
       fill   = "Birth Weight Category") +
  scale_fill_manual(values = c("Normal BW" = "#2196F3", "Low BW" = "#F44336")) +
  theme_classic() +
  theme(axis.text.x = element_text(size = 11),
        plot.title  = element_text(face = "bold", size = 13))

# Customized boxplot with jitter (show all data points)
ggplot(lbw, aes(x = smoke_factor, y = bwt, fill = smoke_factor)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 1.5) +
  labs(title   = "Birth Weight by Smoking Status",
       subtitle = "Boxes show median ± IQR; dots show individual births",
       x        = "Smoking Status",
       y        = "Birth Weight (grams)") +
  scale_fill_manual(values = c("Non-Smoker" = "#4CAF50", "Smoker" = "#E91E63")) +
  theme_classic() +
  guides(fill = "none")  # Remove legend (already on x-axis)

# Scatter plot with regression line and color grouping
ggplot(lbw, aes(x = age, y = bwt, color = smoke_factor)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title  = "Mother's Age vs Birth Weight by Smoking Status",
       x      = "Mother's Age (years)",
       y      = "Birth Weight (grams)",
       color  = "Smoking Status") +
  theme_classic()

# Violin plot (shows distribution shape better than boxplot)
ggplot(infant, aes(x = sex_factor, y = wt1, fill = sex_factor)) +
  geom_violin(alpha = 0.6) +
  geom_boxplot(width = 0.1, fill = "white") +  # boxplot inside violin
  labs(title = "Infant Weight at Baseline by Sex",
       x     = "Sex",
       y     = "Weight (units)") +
  theme_classic() +
  guides(fill = "none")

# ------------------------------------------------------------------------------
# TASK 3 (Session III):
#   a) Create a grouped bar chart: x = water_factor, fill = sex_factor
#      (infant dataset). Use position = "fill" for proportions.
#   b) Make a boxplot of infant wt_gain by water_factor.
#      Add individual data points using geom_jitter().
#   c) Add a regression line to the infant age vs wt2 scatter plot.
#      Color the points by sex_factor.
#   d) Change the theme to theme_bw() and add a bold title.
# ------------------------------------------------------------------------------

# ---- SESSION IV: ADVANCED PLOTS & SAVING ------------------------------------

# --- Faceting: Multiple panels ---
# facet_wrap creates separate plots for each level of a variable
ggplot(lbw, aes(x = age, y = bwt, color = low_factor)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ race_factor) +  # One panel per race
  labs(title  = "Age vs Birth Weight by Race",
       x      = "Mother's Age",
       y      = "Birth Weight (grams)",
       color  = "BW Status") +
  theme_bw()

# Infant: weight gain by sex and water access
ggplot(infant, aes(x = sex_factor, y = wt_gain, fill = sex_factor)) +
  geom_boxplot() +
  facet_wrap(~ water_factor) +
  labs(title = "Infant Weight Gain by Sex and Water Access",
       x     = "Sex",
       y     = "Weight Gain (wt2 - wt1)") +
  theme_classic() +
  guides(fill = "none")

# --- Density plot: smooth distribution ---
ggplot(lbw, aes(x = bwt, fill = smoke_factor)) +
  geom_density(alpha = 0.4) +
  labs(title = "Birth Weight Density by Smoking Status",
       x     = "Birth Weight (grams)",
       fill  = "Smoking Status") +
  theme_classic()

# --- Stacked bar chart (proportions) ---
lbw %>%
  group_by(race_factor, low_factor) %>%
  summarise(count = n(), .groups = "drop") %>%
  ggplot(aes(x = race_factor, y = count, fill = low_factor)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Proportion of Low Birth Weight by Race",
       x     = "Race",
       y     = "Proportion",
       fill  = "BW Status") +
  scale_fill_manual(values = c("Normal BW"="#2196F3","Low BW"="#F44336")) +
  theme_classic()

# --- SAVING PLOTS ---
# Always save your final plots!

final_plot <- ggplot(lbw, aes(x = smoke_factor, y = bwt, fill = smoke_factor)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3) +
  labs(title   = "Birth Weight by Smoking Status",
       x        = "Smoking Status",
       y        = "Birth Weight (grams)") +
  scale_fill_manual(values = c("Non-Smoker"="#4CAF50","Smoker"="#E91E63")) +
  theme_classic() +
  guides(fill = "none")

ggsave("birthweight_smoking.png",
       plot   = final_plot,
       dpi    = 300,
       width  = 8,
       height = 6)

# Save as PDF (better for publications)
ggsave("birthweight_smoking.pdf",
       plot   = final_plot,
       width  = 8,
       height = 6)

# ------------------------------------------------------------------------------
# TASK 4 (Session IV):
#   a) Create a faceted density plot of lbw$bwt,
#      faceted by race_factor, colored by smoke_factor
#   b) Build a stacked proportional bar chart for the infant dataset:
#      x = sex_factor, fill = water_factor
#   c) Save your best plot as a PNG file with dpi = 300
#   d) CHALLENGE: Combine age vs bwt scatter, faceted by race_factor
#      AND colored by smoke_factor (3 variables in one plot!)
# ------------------------------------------------------------------------------

# ==============================================================================
# END OF DAY 4
# Today you learned:
#   - ggplot2 grammar of graphics (layers)
#   - Histograms, bar charts, boxplots, scatter plots
#   - Grouping with fill/color aesthetics
#   - Faceting to show multiple panels
#   - Customizing themes, colors, and labels
#   - Saving plots with ggsave()
# ==============================================================================
