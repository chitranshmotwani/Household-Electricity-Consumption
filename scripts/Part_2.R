# Load necessary libraries
library(TTR)    
library(zoo)    
library(ggplot2)  
library(dplyr)
library(lubridate)

# Function to load dataset with error handling
load_data <- function(file_path) {
  data <- tryCatch({
    read.csv(file_path, stringsAsFactors = FALSE)  # Ensure strings are not converted to factors
  }, error = function(e) {
    cat("Error loading data:", e$message, "\n")
    return(NULL)
  })
  return(data)
}

# Function to fill NA values using linear interpolation
fill_na_values <- function(feature) {
  if (all(is.na(feature))) return(rep(NA, length(feature)))  # Handle all NA columns
  non_na_indices <- which(!is.na(feature))
  x <- non_na_indices
  y <- feature[non_na_indices]
  xout <- seq_along(feature)
  interpolated_feature <- approx(x, y, xout, rule=2)$y
  return(interpolated_feature)
}

# Function to detect anomalies using Z-scores
detect_anomalies <- function(feature) {
  z_scores <- (feature - mean(feature, na.rm = TRUE)) / sd(feature, na.rm = TRUE)
  anomalies <- which(abs(z_scores) > 3)  # Identify values beyond 3 standard deviations
  return(anomalies)
}

# Load dataset
data <- load_data('./Group_Assignment_Dataset.txt')

# Ensure Date is parsed correctly
data$Date <- dmy(data$Date)

# Convert Date and Time into a single datetime object
data$DateTime <- as.POSIXct(paste(data$Date, data$Time), format = "%Y-%m-%d %H:%M:%S")

# Fill missing values using interpolation for specified columns
columns <- c("Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity",
             "Sub_metering_1", "Sub_metering_2", "Sub_metering_3")

for (col in columns) {
  data[[col]] <- fill_na_values(data[[col]])
}

# Remove anomalies from Global_intensity
anomalies <- detect_anomalies(data$Global_intensity)
if (length(anomalies) > 0) {
  data <- data[-anomalies, ]
}

# Assign week numbers (ISO weeks)
data$Week <- isoweek(data$Date)

# Count occurrences per week
week_counts <- table(data$Week)

# Keep weeks with at least 5 days (instead of 7)
full_weeks <- as.numeric(names(week_counts[week_counts >= 5]))

# Filter dataset to keep only these weeks
data <- data[data$Week %in% full_weeks, ]

# Compute moving average for Global_intensity
window_size <- 7  
data$Smooth_Global_intensity <- rollmean(data$Global_intensity, k=window_size, fill=NA, align='center')

# Compute the average smoothened week
smoothened_weeks <- aggregate(Smooth_Global_intensity ~ Week, data, function(x) mean(x, na.rm = TRUE))

# Compute baseline normal week
average_smoothened_week <- mean(smoothened_weeks$Smooth_Global_intensity, na.rm = TRUE)

# Compute anomaly scores (absolute deviation from average week)
smoothened_weeks$Anomaly_Score <- abs(smoothened_weeks$Smooth_Global_intensity - average_smoothened_week)

# Identify most and least anomalous weeks
most_anomalous_week <- smoothened_weeks[which.max(smoothened_weeks$Anomaly_Score), "Week"]
least_anomalous_week <- smoothened_weeks[which.min(smoothened_weeks$Anomaly_Score), "Week"]

# Plot the smoothened weeks
ggplot() +
  geom_line(data = data[data$Week == most_anomalous_week, ], 
            aes(x = Date, y = Smooth_Global_intensity, color = "Most Anomalous Week"), linewidth = 1) +
  geom_line(data = data[data$Week == least_anomalous_week, ], 
            aes(x = Date, y = Smooth_Global_intensity, color = "Least Anomalous Week"), linewidth = 1) +
  geom_hline(yintercept = average_smoothened_week, color = "black", linetype = "dashed", linewidth = 1) +
  labs(title = "Comparison of Most and Least Anomalous Weeks",
       x = "Date", y = "Smoothened Global Intensity") +
  scale_color_manual(values = c("Most Anomalous Week" = "red", "Least Anomalous Week" = "blue")) +
  theme_minimal()

# Print anomaly scores
print(smoothened_weeks)
