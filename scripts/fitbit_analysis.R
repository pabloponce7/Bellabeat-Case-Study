# =========================================
# FitBit Fitness Tracker Data Analysis
# Author: Juan Pablo Ponce
# Date: 08/25/2025
# Description: Data cleaning, exploration, and visualization of FitBit activity and calories datasets.
# =========================================

# Load essential libraries for data manipulation, cleaning, and visualization
library(tidyverse) 
library(lubridate) 
library(janitor)

# -------------------------------
# Load and clean daily_activity1 (3/12-4/11)
# -------------------------------
daily_activity1 <- read_csv("D://Pablo//Data//Case Study//FitBit Fitness Tracker Data//Fitabase Data 3.12.16-4.11.16//dailyActivity_merged.csv") %>% 
  # Remove redundant or unclear distance columns
  select(-TotalDistance, 
         -TrackerDistance, 
         -LoggedActivitiesDistance, 
         -VeryActiveDistance, 
         -ModeratelyActiveDistance, 
         -LightActiveDistance, 
         -SedentaryActiveDistance) %>%  
  clean_names() %>%   # Convert column names to snake_case
  mutate(
    activity_date = as.Date(activity_date, format = "%m/%d/%Y"), # Convert activity_date to Date type
    id = as.character(id),                                        # Convert id to character
    active_minutes = very_active_minutes + fairly_active_minutes # Create active_minutes column
  )

# -------------------------------
# Load and clean daily_activity2 (4/12-5/11)
# -------------------------------
daily_activity2 <- read_csv("D://Pablo//Data//Case Study//FitBit Fitness Tracker Data//Fitabase Data 4.12.16-5.12.16//dailyActivity_merged.csv") %>% 
  select(-TotalDistance, 
         -TrackerDistance, 
         -LoggedActivitiesDistance, 
         -VeryActiveDistance, 
         -ModeratelyActiveDistance, 
         -LightActiveDistance, 
         -SedentaryActiveDistance) %>%  
  clean_names() %>%  
  mutate(
    activity_date = as.Date(activity_date, format = "%m/%d/%Y"), 
    id = as.character(id), 
    active_minutes = very_active_minutes + fairly_active_minutes
  )

# -------------------------------
# Combine daily_activity1 & daily_activity2
# -------------------------------
daily_activity <- bind_rows(daily_activity1, daily_activity2) %>%
  distinct() %>%  # Remove duplicate rows
  drop_na(activity_date, calories, active_minutes) # Remove rows with missing key values

# Remove potential outlier: day with maximum active_minutes
daily_activity <- daily_activity %>%
  filter(active_minutes != max(active_minutes, na.rm = TRUE))

# -------------------------------
# Load and clean hourly_calories dataset
# -------------------------------
hourly_calories <- read_csv("D:\\Pablo\\Data\\Case Study\\FitBit Fitness Tracker Data\\Fitabase Data 4.12.16-5.12.16\\hourlyCalories_merged.csv") %>% 
  clean_names() %>%  
  mutate(
    id = as.character(id),                                         # Convert id to character
    activity_hour = as.POSIXct(activity_hour, format = "%m/%d/%Y %I:%M:%S %p") # Convert to datetime
  ) %>% 
  distinct() %>% 
  drop_na()  # Remove rows with missing values

# -------------------------------
# Visualization: Hour of Day vs Average Calories
# -------------------------------
# Calculate overall mean calories per hour for reference line
mean_hourly_calories <- hourly_calories %>% 
  summarise(mean_hourly_calories = mean(calories)) %>% 
  pull(mean_hourly_calories)

# Prepare data and create bar plot
hourly_calories %>%
  mutate(hour = hour(activity_hour)) %>%     
  group_by(hour) %>%                          
  summarise(avg_calories = mean(calories), .groups = "drop") %>% 
  ggplot(aes(x = hour, y = avg_calories)) +
  geom_col(fill = "steelblue") +
  geom_hline(yintercept = mean_hourly_calories, color = "red", linetype = "dashed", size = 1) +
  annotate(
    "text",
    x = 0.5,  
    y = mean_hourly_calories + 7, 
    label = "average cal/hour",
    hjust = 0,  
    color = "red",
    size = 5
  ) +
  scale_x_continuous(breaks = 0:23) +
  scale_y_continuous(breaks = seq(0, 120, by = 10)) + 
  labs(
    title = "Average Calories Burned by Hour of Day",
    x = "Hour of Day",
    y = "Average Calories Burned"
  ) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

# -------------------------------
# Visualization: Average Active Minutes & Calories by Day of the Week
# -------------------------------
daily_summary <- daily_activity %>%
  mutate(weekday = wday(activity_date, label = TRUE, abbr = FALSE)) %>% # Add weekday column
  group_by(weekday) %>%
  summarise(
    avg_active_minutes = mean(active_minutes),
    avg_calories = mean(calories),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(avg_active_minutes, avg_calories), # Convert to long format for faceted plot
    names_to = "metric",
    values_to = "value"
  )

ggplot(daily_summary, aes(x = weekday, y = value, fill = metric)) +
  geom_col() +
  geom_text(aes(label = round(value, 1)), vjust = 1.5, color = "white", size = 3) +
  facet_wrap(~ metric, scales = "free_y", 
             labeller = labeller(metric = c(
               avg_active_minutes = "Average Active Minutes",
               avg_calories = "Average Calories Burned"
             ))) +
  labs(
    title = "Average Active Minutes & Calories by Day of the Week",
    x = "Day of the Week",
    y = "Value"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none")

# -------------------------------
# Visualization: Active Minutes vs Calories (Outliers Removed)
# -------------------------------
# Calculate IQR bounds to remove outliers
Q1 <- quantile(daily_activity$active_minutes, 0.25, na.rm = TRUE)
Q3 <- quantile(daily_activity$active_minutes, 0.75, na.rm = TRUE)
IQR_value <- IQR(daily_activity$active_minutes, na.rm = TRUE)

lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

# Filter dataset to remove outliers
daily_activity_iqr <- daily_activity %>%
  filter(active_minutes >= lower_bound & active_minutes <= upper_bound & active_minutes > 0)

# Calculate correlation between active minutes and calories
cor_value <- cor(daily_activity_iqr$active_minutes, 
                 daily_activity_iqr$calories,
                 use = "complete.obs")

# Scatter plot with regression line
ggplot(daily_activity_iqr, aes(x = active_minutes, y = calories)) +
  geom_point(size = 1, alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = "Relationship Between Active Minutes and Calories Burned (Outliers Removed)",
    x = "Active Minutes",
    y = "Calories Burned",
    subtitle = paste("Correlation (Pearson's r) =", round(cor_value, 2))
  ) +
  theme_minimal()

