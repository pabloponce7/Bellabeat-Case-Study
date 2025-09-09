# FitBit Fitness Tracker Data Analysis

## Overview
This project explores activity and calorie data from FitBit trackers to uncover trends in daily and hourly activity. Using R, the analysis includes:

- Cleaning and combining multiple datasets
- Calculating new features (e.g., active minutes)
- Visualizing trends by hour of day and day of the week
- Examining the relationship between active minutes and calories burned

---

## Repository Structure
```
fitbit-analysis/
│
├── data/
│   ├── raw/                  # Original CSV datasets
│   └── cleaned/              # Cleaned CSV files
│
├── scripts/
│   └── fitbit_analysis.R     # Main analysis script
│
├── output/
│   ├── figures/              # Exported plots (PNG)
│   └── data_frames/          # Aggregated or cleaned datasets
│
└── README.txt
```

---

## Example Visualizations

### Average Calories Burned by Hour of Day
(output/figures/hourly_calories.png)

### Average Active Minutes & Calories by Day of the Week
(output/figures/daily_summary.png)

### Active Minutes vs Calories Burned (Outliers Removed)
(output/figures/active_vs_calories.png)

> Plots are saved in the `output/figures/` folder.

---

## Getting Started

### Prerequisites
- R & RStudio
- R packages: `tidyverse`, `lubridate`, `janitor`
```
install.packages(c("tidyverse", "lubridate", "janitor"))
```

### Clone the Repository
```
git clone https://github.com/USERNAME/fitbit-analysis.git
cd fitbit-analysis
```

---

## Analysis Script (`fitbit_analysis.R`)
The script includes:

1. **Data Loading & Cleaning**
   - Reads CSV files
   - Converts columns to appropriate types (`Date`, `character`, `POSIXct`)
   - Removes duplicates, handles missing values
   - Creates `active_minutes`

2. **Exploratory Analysis**
   - Hourly calorie trends
   - Weekly averages of active minutes and calories
   - Active minutes vs calories relationship (outliers removed)

3. **Visualizations**
   - Bar plots, faceted plots, scatter plots with regression lines

---

## Contributing
Contributions are welcome! Please open an issue or submit a pull request.

---

## License
MIT License

---

## Contact
Juan Pablo Ponce– jpabloponce72@gmail.com  
Repository: [https://github.com/USERNAME/fitbit-analysis](https://github.com/USERNAME/fitbit-analysis)

