library(depmixS4)
library(ggplot2)
library(dplyr)
library(reshape2)
library(lubridate)
library(zoo)
library(ggplot2)
Sys.setlocale("LC_TIME", "C")  # Forces English weekday names

# Load the dataset
data <- read.csv("Group_Assignment_Dataset.txt", header=TRUE, sep=",", stringsAsFactors=FALSE)

#------------------------------- Question 1 ------------------------
#To be able to perform the HMM training model we need to have a complete dataset
#We will reuse the question 1 from the first assignment where we filled all the Nan values with linear interpolation

# Fill missing values using linear interpolation
fill_na_values <- function(feature) {
  non_na_indices <- which(!is.na(feature))
  x <- non_na_indices
  y <- feature[non_na_indices]
  xout <- seq_along(feature)
  approx(x, y, xout)$y  # Linear interpolation
}

data$Global_active_power <- fill_na_values(data$Global_active_power)
data$Global_reactive_power <- fill_na_values(data$Global_reactive_power)
data$Voltage <- fill_na_values(data$Voltage)
data$Global_intensity <- fill_na_values(data$Global_intensity)
data$Sub_metering_1 <- fill_na_values(data$Sub_metering_1)
data$Sub_metering_2 <- fill_na_values(data$Sub_metering_2)
data$Sub_metering_3 <- fill_na_values(data$Sub_metering_3)

# Convert Date and Time into a single POSIXct timestamp
data$Datetime <- as.POSIXct(paste(data$Date, data$Time), format="%d/%m/%Y %H:%M:%S", tz= "UTC")
sum(is.na(data$Datetime))

#Now we create a new column to store the hours of each day so we can pick the time window we'd like
data$Hour <- hour(data$Datetime)
sum(is.na(data$Hour))

#We create a filtered dataset within the time window of 7 to 17. We could modify the values for any desired time window
data_filtered <- data %>%
  filter(Hour >= 7 & Hour < 17)

#We modify the dataset to obtain a new one with one row per observation, a column indicating the calendar date it belongs to and an index counting rows within that date

data_filtered_c <- data_filtered %>%
  mutate(Date_only = as.Date(Datetime)) %>%
  group_by(Date_only) %>%
  mutate(row_in_day = row_number())

#We define an ntimes vector to tell depmixS4 how to partition the data into multiple segments
# 51 full weeks + 1 "extended" 8-day week to fit the 365 days
ntimes_vec <- c(rep(4200, 51), 4800)

# Check row counts:
sum(ntimes_vec) == nrow(data_filtered_c)

#Now we are ready to train the HMM model with the dataset
#We will start by creating a dataframe to store the results and compare them

results1 <- data.frame(
  nstates = integer(),
  logLik  = double(),
  BIC     = double(),
  stringsAsFactors = FALSE
)

#Now we iterate through a loop to compute the HMM for different number of states
for (nstates in 4:16){
  #We create the depmix model
  model_c <- depmix(
    response = Global_active_power ~ 1,
    data     = data_filtered_c,
    nstates  = nstates,
    family   = gaussian(),
    ntimes   = ntimes_vec)
  
  # We fit the model
  fitmodel<-fit(model_c)

  
  # We store the results on the dataframe
  results1 <- rbind(
    results1,
    data.frame(nstates = nstates, logLik = logLik(fitmodel), BIC = BIC(fitmodel))
  )
}

#And we finally visualize the results

results1

#And we plot them to compare them and look for the best model

# Log-likelihood plot
ggplot(results1, aes(x = nstates, y = logLik)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(title = "Log-Likelihood vs. Number of States",
       x = "Number of States",
       y = "Log-Likelihood") +
  theme_minimal()

# BIC plot
ggplot(results1, aes(x = nstates, y = BIC)) +
  geom_line(color = "red") +
  geom_point(color = "red") +
  labs(title = "BIC vs. Number of States",
       x = "Number of States",
       y = "BIC") +
  theme_minimal()

#------------------------------- Question 2 ------------------------

# Discretize continuous variable by rounding to nearest half-integer

data_filtered_d <- data_filtered %>%
  mutate(Date_only = as.Date(Datetime)) %>%
  group_by(Date_only) %>%
  mutate(row_in_day = row_number())

data_filtered_d$Global_Active_Power_Discrete <- round(data_filtered_d$Global_active_power *2) /2

results2 <- data.frame(
  nstates = integer(),
  logLik  = double(),
  BIC     = double(),
  stringsAsFactors = FALSE
)

#Now we iterate through a loop to compute the HMM for different number of states
for (nstates in 4:16){
  #We create the depmix model
  model_d <- depmix(
    response = Global_Active_Power_Discrete ~ 1,
    data     = data_filtered_d,
    nstates  = nstates,
    family   = multinomial(),
    ntimes   = ntimes_vec)
  
  # We fit the model
  fitmodel_d<-fit(model_d)
  
  # We store the results on the dataframe
  results2 <- rbind(
    results2,
    data.frame(nstates = nstates, logLik = logLik(fitmodel_d), BIC = BIC(fitmodel_d))
  )
}

results2

# Log-likelihood plot
ggplot(results2, aes(x = nstates, y = logLik)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(title = "Log-Likelihood vs Number of States",
       x = "Number of States",
       y = "Log-Likelihood") +
  theme_minimal()

# BIC plot
ggplot(results2, aes(x = nstates, y = BIC)) +
  geom_line(color = "red") +
  geom_point(color = "red") +
  labs(title = "BIC vs Number of States",
       x = "Number of States",
       y = "BIC") +
  theme_minimal()
