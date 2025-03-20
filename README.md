# Household Electricity Consumption Analysis

This project analyzes household electricity consumption data using **Hidden Markov Models (HMMs)**. The dataset represents a multivariate time series with minute-level measurements of electricity consumption. The project is divided into three parts, each focusing on different aspects of data exploration, preprocessing, and modeling.

---

## **Project Structure**
```
Household-Electricity-Consumption/
├── scripts/ # Contains all code files
│ ├── part_1.R # Code for Assignment 1: Data Exploration and Preparation
│ ├── part_2.R # Code for Assignment 2: Feature Scaling and Anomaly Detection
│ └── part_3.R # Code for Assignment 3: HMM Training and Analysis
├── reports/ # Contains the combined report
│ └── combined_report.pdf # Combined report for all three assignments
├── data/ # Contains the dataset
│ └── household_power_consumption.txt
└── README.md # This file
```

---

## **Code Files**

1. **`part_1.R`**  
   - Focuses on data exploration and preparation.  
   - Tasks:  
     - Handling missing data using linear interpolation.  
     - Detecting anomalies using Z-scores.  
     - Performing correlation analysis and time window analysis.  

2. **`part_2.R`**  
   - Focuses on preprocessing techniques and anomaly detection.  
   - Tasks:  
     - Feature scaling (normalization and standardization).  
     - Smoothing time series data and computing anomaly scores.  
     - Analyzing the impact of discretization on model performance.  

3. **`part_3.R`**  
   - Focuses on training and analyzing Hidden Markov Models (HMMs).  
   - Tasks:  
     - Determining the optimal number of states for the HMM.  
     - Understanding log-likelihood for discrete and continuous variables.  
     - Evaluating the impact of discretization on HMM performance.  

---

## **Combined Report**

The combined report integrates the findings from all three assignments into a single document. It is structured as follows:

1. **Introduction**  
   - Overview of the dataset and project objectives.  

2. **Data Exploration and Preparation**  
   - Handling missing data.  
   - Anomaly detection using Z-scores.  
   - Correlation analysis and time window analysis.  

3. **Preprocessing and Anomaly Detection**  
   - Feature scaling techniques (normalization and standardization).  
   - Smoothing time series data and computing anomaly scores.  
   - Impact of discretization on model performance.  

4. **Hidden Markov Models (HMMs)**  
   - Determining the optimal number of states.  
   - Understanding log-likelihood for discrete and continuous variables.  
   - Evaluating the impact of discretization on HMM performance.  

5. **Conclusion**  
   - Summary of key findings and insights.  

---

## **How to Run the Code**

1. Clone the repository:
   ```bash
   git clone https://github.com/chitranshmotwani/Household-Electricity-Consumption.git

2. Navigate to the project directory:
   ```bash
   cd Household-Electricity-Consumption

3. Install the required R packages::
   ```bash
   install.packages(c("dplyr", "ggplot2", "HMM", "forecast"))

4. Run the scripts in order:
   ```bash
   Rscript scripts/part_1.R
   Rscript scripts/part_2.R
   Rscript scripts/part_3.R
