library(tidyverse) 
library(lubridate) 
library(janitor)
library(ggplot2)


# load and clean daily_activity1 (3/12-4/11) data set

daily_activity1 <- read_csv("D://Pablo//Data//Case Study//FitBit Fitness Tracker Data//Fitabase Data 3.12.16-4.11.16//dailyActivity_merged.csv") %>% 
  select(-TotalDistance, 
         -TrackerDistance, 
         -LoggedActivitiesDistance, 
         -VeryActiveDistance, 
         -ModeratelyActiveDistance, 
         -LightActiveDistance, 
         -SedentaryActiveDistance) %>%  # remove unwanted/unclear columns
  clean_names() %>%   # convert to snake_case
  mutate(
    activity_date = as.Date(activity_date, format = "%m/%d/%Y"), # change column type to date
    id = as.character(id), # change column type to char
    active_minutes = very_active_minutes + fairly_active_minutes) # create a new column with active minutes in that day



# load and clean daily_activity2 (4/12-5/11) data set

daily_activity2 <- read_csv("D://Pablo//Data//Case Study//FitBit Fitness Tracker Data//Fitabase Data 4.12.16-5.12.16//dailyActivity_merged.csv") %>% 
  select(-TotalDistance, 
         -TrackerDistance, 
         -LoggedActivitiesDistance, 
         -VeryActiveDistance, 
         -ModeratelyActiveDistance, 
         -LightActiveDistance, 
         -SedentaryActiveDistance) %>%  # remove unwanted/unclear columns
  clean_names() %>%   # convert to snake_case
  mutate(
    activity_date = as.Date(activity_date, format = "%m/%d/%Y"), #change column type to date
    id = as.character(id), # change column type to char
    active_minutes = very_active_minutes + fairly_active_minutes) # create a new column with active minutes in that day

# combine both data frames into one and delete duplicates and missing values

daily_activity <- bind_rows(daily_activity1, daily_activity2) %>%
  distinct() %>% 
  drop_na(activity_date, calories, active_minutes)

daily_activity <- daily_activity %>%
  filter(active_minutes != max(active_minutes, na.rm = TRUE))


# load and clean hourly_calories data set

hourly_calories <- read_csv("D:\\Pablo\\Data\\Case Study\\FitBit Fitness Tracker Data\\Fitabase Data 4.12.16-5.12.16\\hourlyCalories_merged.csv") %>% 
  clean_names() %>% 
  mutate(
    id = as.character(id),
    activity_hour = as.POSIXct(activity_hour, format = "%m/%d/%Y %I:%M:%S %p")
  ) %>% 
  distinct() %>% 
  drop_na()

# exploratory

# VIZ

# calculate mean_hourly_calories for the red line
mean_hourly_calories <- hourly_calories %>% 
  summarise(mean_hourly_calories = mean(calories)) %>% 
  pull(mean_hourly_calories)

# plot hour of day vs average cal/hour

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
    size = 5,) +
  scale_x_continuous(breaks = 0:23) +
  scale_y_continuous(breaks = seq(0, 120, by = 10)) + 
  labs(
    title = "Average Calories Burned by Hour of Day",
    x = "Hour of Day",
    y = "Average Calories burned"
  ) +
  theme_minimal()+
  theme(panel.grid.minor = element_blank())

# day of the week vs active minutes & calories burned

# Prepare data for plot
daily_summary <- daily_activity %>%
  mutate(weekday = wday(activity_date, label = TRUE, abbr = FALSE)) %>%
  group_by(weekday) %>%
  summarise(
    avg_active_minutes = mean(very_active_minutes),
    avg_calories = mean(calories),
    .groups = "drop"
  ) %>%
  pivot_longer( # transform data set to long format
    cols = c(avg_active_minutes, avg_calories),
    names_to = "metric",
    values_to = "value"
  )

# Plot with facets

ggplot(daily_summary, aes(x = weekday, y = value, fill = metric)) +
  geom_col() + # bar graph
  geom_text(aes(label = round(value, 1)), # labels in columns
            vjust = 1.5,
            color = "white",
            size = 3) +
  facet_wrap(~ metric, 
             scales = "free_y",
             labeller = labeller(metric = c( # change the facet labels for more descriptive ones
               avg_active_minutes = "Average Active Minutes",
               avg_calories = "Average Calories burned"))) + 
  labs(
    title = "Average Active Minutes & Calories by Day of the Week",
    x = "Day of the Week",
    y = "Value"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none")  # Remove redundant legend

# plot active_minutes vs calories burt by day of the week

# Calculate averages by day of the week (wide format)

daily_summary_wide <- daily_activity %>%
  mutate(weekday = wday(activity_date, label = TRUE, abbr = FALSE)) %>%
  group_by(weekday) %>%
  summarise(
    avg_active_minutes = mean(very_active_minutes),
    avg_calories = mean(calories),
    .groups = "drop"
  )

# average calories burned vs average active minutes by day of the week


## calculate correlation
cor_value <- cor(daily_summary_wide$avg_active_minutes, 
                 daily_summary_wide$avg_calories)

## scatter Plot
ggplot(daily_activity, aes(x = active_minutes, 
                           y = calories, )) +
  geom_point(size = 1, alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = "Relationship Between Active Minutes and Calories Burned",
    x = "Active Minutes",
    y = "Calories Burned",
    subtitle = paste("Correlation (Pearson's r) =", round(cor_value, 2))
  ) +
  theme_minimal()

