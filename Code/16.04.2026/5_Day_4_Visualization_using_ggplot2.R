# ==============================================================================
# DAY 4: DATA VISUALIZATION
# ==============================================================================
library(readxl)
library(ggplot2)
#data <- read_excel("Nepal_Health_Data.xlsx")
data <- read_excel("C:/Users/Shirish Maharjan/OneDrive - HERD/Herd/R Training 2026/Data/Nepal_Health_Data.xlsx")


# --- Session I: Introduction to ggplot2 ---
# Basic Logic: ggplot(data, aes(x, y)) + geom_function()

# --- Session II: Creating Plots ---
# 1. Histogram of Ages
ggplot(data, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "orange", color = "black")

# 2. Bar chart: Count of patients per Province
ggplot(data, aes(x = Province)) +
  geom_bar(fill = "steelblue")

# --- Session III: Customizing Appearance ---
ggplot(data, aes(x = Province, y = Hemoglobin, fill = Province)) +
  geom_boxplot() +
  labs(title = "Hemoglobin Levels by Province in Nepal",
       subtitle = "Analysis of 500 health records",
       x = "Province Name", 
       y = "Hemoglobin (g/dL)") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# --- Session IV: Advanced Plotting ---
# 1. Scatter plot with Regression line
ggplot(data, aes(x = Age, y = Systolic_BP, color = Gender)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap(~Smoker) + # Creates two panels: Smoker vs Non-Smoker
  labs(title = "Age vs Blood Pressure Faceted by Smoking Status")

# 2. Density Plot: Distribution of BMI
ggplot(data, aes(x = BMI, fill = Gender)) +
  geom_density(alpha = 0.4) +
  labs(title = "BMI Density by Gender")

# 3. Save your plot
my_plot <- ggplot(data, aes(x = BMI, fill = Gender)) +
  geom_density(alpha = 0.4) +
  labs(title = "BMI Density by Gender")

ggsave("my_plot.png", plot = my_plot, dpi = 300, width = 10, height = 7)


# FINAL TASK: Create a plot showing the correlation between 
# ANC_Visits and Hemoglobin, colored by Province.