# Libraries
library(ggplot2)
library(dplyr)
library(data.table)
library(tidyverse)
library(coefplot)
library(gamlss)
library(keras)
library(tensorflow)
library(reticulate)
library(kableExtra)
library(gridExtra)
library(corrplot)

# Clear existing environment
rm(list = ls())

# Set appropriate working directory
setwd("~/PhD/YEAR 3 2024-2025/Paper Revision")

# Set an initial seed
seed <- 1999
set.seed(seed)

# Load required libraries
library(dplyr)

# Read the data and perform all transformations in a single pipeline
BvDatabase <- read.csv("BVDatabase.csv") %>%
  # Filter out Avg_Cost <= 0 and remove duplicates by Policykey
  dplyr::filter(Avg_Cost > 0) %>%
  dplyr::distinct(Policykey, .keep_all = TRUE) %>%
  # Select relevant columns and create Building_Age
  dplyr::select(Avg_Cost, SubItemType_C, builtYear, nrOfFloors, Construction_Material_C, Item_Type_C,
                FloorsType_C, Floors_No, floodvulnerability, windvulnerability, postcode_area, postcode_perimeter,
                postcode_coast_len, postcode_coast_flg, postcode_alt_mean, postcode_slo_mean, postcode_rgh_mean) %>%
  dplyr::mutate(Building_Age = 2022 - builtYear) %>%
  dplyr::select(-builtYear) %>%
  # Set data types for variables
  dplyr::mutate(across(c(SubItemType_C, Construction_Material_C, FloorsType_C, Item_Type_C, nrOfFloors, 
                         floodvulnerability, windvulnerability, postcode_coast_flg), as.factor),
                across(c(Building_Age, postcode_area, postcode_perimeter, 
                         postcode_coast_len, postcode_alt_mean, postcode_slo_mean, postcode_rgh_mean), as.numeric)) %>%
  # Handle missing values and transformations
  dplyr::mutate(postcode_coast_len = ifelse(is.na(postcode_coast_len), 1, postcode_coast_len),
                log_postcode_area = log(postcode_area),
                log_postcode_perimeter = log(postcode_perimeter),
                log_postcode_coast_len = log(postcode_coast_len),
                Floors_No = as.factor(pmin(Floors_No, 4))) %>%
  dplyr::select(-postcode_area, -postcode_perimeter, -postcode_coast_len) %>%
  na.omit()

################################################################################
# ----------------- Plots to visualize data ------------------------------------
################################################################################

summary(BvDatabase$Avg_Cost)

sd(BvDatabase$Avg_Cost)
#table(data$Avg_Cost)
# Load ggplot2

# Total Cost
p1 <- ggplot(BvDatabase, aes(x = Avg_Cost)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 100) +
  labs(x = "Total Cost of Claim", y = "Frequency") +
  xlim(0, 10000)
p1

# Continuous Variables

# Mean Altitude of Postcode
p2 <- ggplot(BvDatabase, aes(x = postcode_alt_mean)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 100) +
  labs(x = "Mean Altitude of Postcode (m)", y = "Frequency") 
  
# Mean Slope of Postcode
p3 <- ggplot(BvDatabase, aes(x = postcode_slo_mean)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 100) +
  labs(x = "Mean Slope of Postcode (C)", y = "Frequency") 

# Mean Surface Roughness of Postcode
p4 <- ggplot(BvDatabase, aes(x = postcode_rgh_mean)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 100) +
  labs(x = "Mean Surface Roughness Postcode", y = "Frequency") 

# Histogram for Logarithmic Postcode_area
p5 <- ggplot(BvDatabase, aes(x = log_postcode_area)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 100) +
  labs(x = "Logarithmic Postcode Area (squared m)", y = "Frequency") 

# Histogram for Postcode_perimeter
p6 <- ggplot(BvDatabase, aes(x = log_postcode_perimeter)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 100) +
  labs(x = "Logarithmic Postcode Perimeter (m)", y = "Frequency") 

# Histogram for Logarithmic Postcode Coastal Length > 0
p7 <- ggplot(BvDatabase, aes(x = log_postcode_coast_len)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 100) +
  labs(x = "Logarithmic Postcode Coastal Length > 0 (m)", y = "Frequency")


# Arrange plots in a 3x2 grid
grid.arrange(p2, p3, p4, p5, p6, p7, nrow = 3, ncol = 2)

# Ordinal & Numerical Variables

# Bar Chart for nrOfFloors
p8 <- ggplot(BvDatabase, aes(x = nrOfFloors)) +
  geom_bar(fill = "lightblue") +
  labs(x = "Number of Floors of the Building", y = "Frequency") 

# Bar chart for Item_Type_C
p9 <- ggplot(BvDatabase, aes(x = factor(Item_Type_C))) +
  geom_bar(fill = "lightblue") +
  labs(x = "Type of Insured Item", y = "Frequency") 

# Bar chart for Floors_No
p10 <- ggplot(BvDatabase, aes(x = factor(Floors_No))) +
  geom_bar(fill = "lightblue") +
  labs(x = "Number of Floors Insured", y = "Frequency") 

# Bar chart for floodvulnerability
p11 <- ggplot(BvDatabase, aes(x = factor(floodvulnerability))) +
  geom_bar(fill = "lightblue") +
  labs(x = "Flood Vulnerability Level", y = "Frequency") 

# Bar chart for windvulnerability
p12 <- ggplot(BvDatabase, aes(x = factor(windvulnerability))) +
  geom_bar(fill = "lightblue") +
  labs(x = "Wind Vulnerability Level", y = "Frequency") 

# Bar chart for Building_Age
p13 <- ggplot(BvDatabase, aes(x = Building_Age)) +
  geom_bar(fill = "lightblue") +
  labs(x = "Age of Building = 2022 - Build Year", y = "Frequency") 

grid.arrange(p8, p9, p10, p11, p12, p13, nrow = 3, ncol = 2)

# Binary Variables

# Create individual data frames for each variable
data_subitemtype <- data.frame(
  Category = c("Buildings", "Contents"),
  Count = c(1350, 213)
)

data_construction_material <- data.frame(
  Category = c("Concrete", "Steel & other"),
  Count = c(1465, 98)
)

data_floors_type <- data.frame(
  Category = c("Ground/Basement/Roof", "Other"),
  Count = c(882, 681)
)

data_postcode_coast <- data.frame(
  Category = c("Not Coastal Area", "Coastal Area"),
  Count = c(852, 711)
)

# Plot for SubItemType_C
p14 <- ggplot(data_subitemtype, aes(x = Category, y = Count, fill = Category)) +
  geom_bar(stat = "identity") +
  labs(x = "Sub-item Insured", y = "Frequency") +
  theme(legend.position = "none")

# Plot for Construction_Material_C
p15 <- ggplot(data_construction_material, aes(x = Category, y = Count, fill = Category)) +
  geom_bar(stat = "identity") +
  labs(x = "Material Used for Construction", y = "Frequency") +
  theme(legend.position = "none")

# Plot for FloorsType_C
p16 <- ggplot(data_floors_type, aes(x = Category, y = Count, fill = Category)) +
  geom_bar(stat = "identity") +
  labs(x = "Type of the Floors in the Insured Building", y = "Frequency") +
  theme(legend.position = "none")

# Plot for postcode_coast_flg
p17 <- ggplot(data_postcode_coast, aes(x = Category, y = Count, fill = Category)) +
  geom_bar(stat = "identity") +
  labs(x = "Coastal Area Indicator", y = "Frequency") +
  theme(legend.position = "none")

# Arrange the plots in a grid (2 rows, 2 columns)
grid.arrange(p14, p15, p16, p17, nrow = 2, ncol = 2)


# Naming of Variables
features <- c("SubItemType_C", "nrOfFloors", "Construction_Material_C", 
       "Item_Type_C", "FloorsType_C", "Floors_No", 
       "floodvulnerability", "windvulnerability", "postcode_coast_flg", 
       "postcode_alt_mean", "postcode_slo_mean", "postcode_rgh_mean", 
       "Building_Age", "log_postcode_area", "log_postcode_perimeter", 
       "log_postcode_coast_len")

Y <- "Avg_Cost"

### Data Partitioning

# Create a learning sample index for cross-validation
ll <- sample(c(1:nrow(BvDatabase)), round(0.9*nrow(BvDatabase)), replace = FALSE)

# learning sample
learn <- BvDatabase[ll,]

# testing sample
test <- BvDatabase[-ll,]
