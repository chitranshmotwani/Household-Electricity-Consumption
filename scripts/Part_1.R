# Load necessary libraries
library(ggplot2)
library(reshape2)
library(dplyr)
library(ggcorrplot)
library(gridExtra)

# ---------------------------- Question 1 -----------------------------------
# Load the dataset with error handling
load_data <- function(file_path) {
  data <- tryCatch({
    read.csv(file_path)
  }, error = function(e) {
    cat("Error loading data:", e$message, "\n")
    return(NULL)
  })
  
  return(data)
}

# Function to fill NA values using linear interpolation
fill_na_values <- function(feature) {
  non_na_indices <- which(!is.na(feature))
  x <- non_na_indices
  y <- feature[non_na_indices]
  xout <- seq_along(feature)
  
  # Perform linear interpolation
  interpolated_feature <- approx(x, y, xout)$y
  return(interpolated_feature)
}

# Function to detect anomalies using Z-scores
detect_anomalies <- function(feature) {
  z_scores <- (feature - mean(feature, na.rm = TRUE)) / sd(feature, na.rm = TRUE)
  anomalies <- which(abs(z_scores) > 3)
  return(anomalies)
}

# Load the dataset
data <- load_data('./Group_Assignment_Dataset.txt')

# Fill NA values for each feature
data$Global_active_power <- fill_na_values(data$Global_active_power)
data$Global_reactive_power <- fill_na_values(data$Global_reactive_power)
data$Voltage <- fill_na_values(data$Voltage)
data$Global_intensity <- fill_na_values(data$Global_intensity)
data$Sub_metering_1 <- fill_na_values(data$Sub_metering_1)
data$Sub_metering_2 <- fill_na_values(data$Sub_metering_2)
data$Sub_metering_3 <- fill_na_values(data$Sub_metering_3)
  
# Detect anomalies for each feature
anomalies_global_active_power <- detect_anomalies(data$Global_active_power)
anomalies_global_reactive_power <- detect_anomalies(data$Global_reactive_power)
anomalies_voltage <- detect_anomalies(data$Voltage)
anomalies_global_intensity <- detect_anomalies(data$Global_intensity)
anomalies_sub_metering_1 <- detect_anomalies(data$Sub_metering_1)
anomalies_sub_metering_2 <- detect_anomalies(data$Sub_metering_2)
anomalies_sub_metering_3 <- detect_anomalies(data$Sub_metering_3)
  
  # Calculate percentage of anomalies per feature
percent_anomalies_global_active_power <- length(anomalies_global_active_power) / nrow(data) * 100
percent_anomalies_global_reactive_power <- length(anomalies_global_reactive_power) / nrow(data) * 100
percent_anomalies_voltage <- length(anomalies_voltage) / nrow(data) * 100
percent_anomalies_global_intensity <- length(anomalies_global_intensity) / nrow(data) * 100
percent_anomalies_sub_metering_1 <- length(anomalies_sub_metering_1) / nrow(data) * 100
percent_anomalies_sub_metering_2 <- length(anomalies_sub_metering_2) / nrow(data) * 100
percent_anomalies_sub_metering_3 <- length(anomalies_sub_metering_3) / nrow(data) * 100
  
# Combine all anomalies
all_anomalies <- unique(c(
  anomalies_global_active_power,
  anomalies_global_reactive_power,
  anomalies_voltage,
  anomalies_global_intensity,
  anomalies_sub_metering_1,
  anomalies_sub_metering_2,
  anomalies_sub_metering_3
))
  
# Total percentage of anomalies
total_percent_anomalies <- length(all_anomalies) / nrow(data) * 100
  
# Print percentage of anomalies for each feature
cat("Percentage of anomalies for each feature:\n")
cat("Global_active_power: ", percent_anomalies_global_active_power, "%\n")
cat("Global_reactive_power: ", percent_anomalies_global_reactive_power, "%\n")
cat("Voltage: ", percent_anomalies_voltage, "%\n")
cat("Global_intensity: ", percent_anomalies_global_intensity, "%\n")
cat("Sub_metering_1: ", percent_anomalies_sub_metering_1, "%\n")
cat("Sub_metering_2: ", percent_anomalies_sub_metering_2, "%\n")
cat("Sub_metering_3: ", percent_anomalies_sub_metering_3, "%\n")
cat("Total percentage of anomalies: ", total_percent_anomalies, "%\n")

# ---------------------------- Question 2 -----------------------------------

# Read data
data <- load_data('./Group_Assignment_Dataset.txt')

# Convert Date and Time into a single datetime object
data$DateTime <- as.POSIXlt(paste(data$Date, data$Time), format = "%d/%m/%Y %H:%M:%S")

# Filter data for Week 6 (Feb 05, 2007 - Feb 11, 2007)
week6_data <- subset(data, DateTime >= as.POSIXlt("2007-02-05") & DateTime <= as.POSIXlt("2007-02-11"))

# Select relevant columns for correlation analysis
features <- c("Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity",
              "Sub_metering_1", "Sub_metering_2", "Sub_metering_3")

# Compute correlation matrix
cor_matrix <- cor(week6_data[, features], method = "pearson", use = "complete.obs")

# Print correlations in a structured format
cat("Correlation Coefficients:\n")
print(round(cor_matrix, 3))

# Plot correlation matrix
ggcorrplot(cor_matrix, lab = TRUE, title = "Feature Correlation Matrix", colors = c("blue", "white", "red"))

# ------------------------ Question 3 ----------------------------

# Convert Date and Time into a single POSIXct timestamp
data$Datetime <- as.POSIXct(paste(data$Date, data$Time), format="%d/%m/%Y %H:%M:%S")

# Extract weekday and weekend information
data$Weekday <- weekdays(data$Datetime)
data$IsWeekend <- ifelse(data$Weekday %in% c("Saturday", "Sunday"), TRUE, FALSE)
# Extract Week Number (UK convention: Monday as first day)
data$Week <- as.numeric(format(data$Datetime, "%W"))

# Filter data for Week 6 (Feb 05, 2007 - Feb 11, 2007)
week6_data <- subset(data, DateTime >= as.POSIXlt("2007-02-05") & DateTime <= as.POSIXlt("2007-02-11"))

# Define time windows for daytime and nighttime
daytime_start <- "07:30:00"
daytime_end <- "17:00:00"
nighttime_start <- "23:00:00"
nighttime_end <- "06:00:00"

# Extract data for defined time windows
daytime_weekdays <- week6_data %>%
  filter(format(Datetime, "%H:%M:%S") >= daytime_start &
           format(Datetime, "%H:%M:%S") <= daytime_end &
           !IsWeekend)

nighttime_weekdays <- week6_data %>%
  filter((format(Datetime, "%H:%M:%S") >= nighttime_start |
            format(Datetime, "%H:%M:%S") <= nighttime_end) &
           !IsWeekend)

daytime_weekends <- week6_data %>%
  filter(format(Datetime, "%H:%M:%S") >= daytime_start &
           format(Datetime, "%H:%M:%S") <= daytime_end &
           IsWeekend)

nighttime_weekends <- week6_data %>%
  filter((format(Datetime, "%H:%M:%S") >= nighttime_start |
            format(Datetime, "%H:%M:%S") <= nighttime_end) &
           IsWeekend)

# Function to calculate averages and prepare data for regression
prepare_data_for_regression <- function(data) {
  data$time_numeric <- as.numeric(format(data$Datetime, "%H")) +
                       as.numeric(format(data$Datetime, "%M")) / 60  # Convert time to hours
  averages <- data %>%
    group_by(time_numeric) %>%
    summarise(Global_intensity = mean(Global_intensity, na.rm = TRUE))
  return(averages)
}

# Prepare data for regression
avg_daytime_weekdays <- prepare_data_for_regression(daytime_weekdays)
avg_nighttime_weekdays <- prepare_data_for_regression(nighttime_weekdays)
avg_daytime_weekends <- prepare_data_for_regression(daytime_weekends)
avg_nighttime_weekends <- prepare_data_for_regression(nighttime_weekends)

# Define a function to plot linear regression results
plot_linear_regression <- function(data, title_prefix) {
  fit_linear <- lm(Global_intensity ~ time_numeric, data = data)
  
  p <- ggplot(data, aes(x = time_numeric, y = Global_intensity)) +
    geom_point(color = "black") +
    geom_smooth(method = "lm", se = FALSE, color = "red") + # Linear fit
    labs(title = paste(title_prefix, "Linear Regression"), x = "Time (Hours)", y = "Global Intensity") +
    theme_minimal()
  
  return(p)
}

# Define a function to plot polynomial regression results
plot_polynomial_regression <- function(data, title_prefix) {
  fit_polynomial <- lm(Global_intensity ~ poly(time_numeric, 2, raw = TRUE), data = data)
  
  p <- ggplot(data, aes(x = time_numeric, y = Global_intensity)) +
    geom_point(color = "black") +
    geom_smooth(method = "lm", formula = y ~ poly(x, 2, raw = TRUE), se = FALSE, color = "blue") + # Polynomial fit
    labs(title = paste(title_prefix, "Polynomial Regression"), x = "Time (Hours)", y = "Global Intensity") +
    theme_minimal()
  
  return(p)
}

# Plotting Linear Regression in one window
linear_plots <- list(
  plot_linear_regression(avg_daytime_weekdays, "Daytime Weekdays"),
  plot_linear_regression(avg_nighttime_weekdays, "Nighttime Weekdays"),
  plot_linear_regression(avg_daytime_weekends, "Daytime Weekends"),
  plot_linear_regression(avg_nighttime_weekends, "Nighttime Weekends")
)

# Plotting Polynomial Regression in another window
polynomial_plots <- list(
  plot_polynomial_regression(avg_daytime_weekdays, "Daytime Weekdays"),
  plot_polynomial_regression(avg_nighttime_weekdays, "Nighttime Weekdays"),
  plot_polynomial_regression(avg_daytime_weekends, "Daytime Weekends"),
  plot_polynomial_regression(avg_nighttime_weekends, "Nighttime Weekends")
)

# Linear Regression Plots
gridExtra::grid.arrange(grobs = linear_plots, ncol = 2)

# Polynomial Regression Plots
gridExtra::grid.arrange(grobs = polynomial_plots, ncol = 2)
