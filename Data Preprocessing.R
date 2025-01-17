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

# Clear existing environment
#rm(list = ls())

# Set appropriate working directory
setwd("~/PhD/YEAR 3 2024-2025/Paper Revision")

# Set a local directory to save the model weights
local_path <- "~/PhD/YEAR 3 2024-2025/Paper Revision/TensorFlowCheckpoints"

# Create the directory if it doesn't exist
if (!dir.exists(local_path)) {
  dir.create(local_path, recursive = TRUE)
}

# Set an initial seed
seed <- 1999
set.seed(seed)

# Read the data
data <- read.csv("BVDatabase.csv")
#data <- BvDatabase
# Combining Property and Contents to Avg_Cost
# property_and_contents <- data %>%
#   group_by(Policykey) %>% 
#   summarise(Avg_Cost = sum(Avg_Cost, na.rm = TRUE)) 

# Create data1 with Avg_Cost with zeroes removed
data1 <- data %>%
  filter(Avg_Cost > 0) %>%
  distinct(Policykey, .keep_all = TRUE)

# Create data2 with all the columns we wish to select
data2 <- data1 %>% dplyr::select(Avg_Cost, SubItemType_C, builtYear, nrOfFloors, Construction_Material_C, Item_Type_C,
                                 FloorsType_C, Floors_No, floodvulnerability, windvulnerability, postcode_area, postcode_perimeter,
                                 postcode_coast_len, postcode_coast_flg, postcode_alt_mean, postcode_slo_mean, postcode_rgh_mean) 

# Change builtYear to Building Age for better interpretability
data2$Building_Age <- 2022 - data2$builtYear

# Remove builtYear
data2 <- data2 %>% dplyr::select(-builtYear)

# Setting data type right for variables
data2 <- data2 %>% 
  dplyr::mutate_at(vars(SubItemType_C, Construction_Material_C, Item_Type_C, 
                        FloorsType_C, floodvulnerability, windvulnerability, 
                        postcode_coast_flg), as.factor) %>%
  dplyr::mutate_at(vars(Building_Age, nrOfFloors, Floors_No, postcode_area, postcode_perimeter, 
                        postcode_coast_len, postcode_alt_mean, postcode_slo_mean, 
                        postcode_rgh_mean), as.numeric) %>% 
  dplyr::mutate_at(vars(SubItemType_C, Construction_Material_C, FloorsType_C, postcode_coast_flg), as.integer) %>%
  data.frame


# Create data3 which has the categorical variables set to dummies
data3 <- data2 

# Setting dummies for "Construction_Material_C", "Item_Type_C" and "FloorsType_C" 
# data3$Construction_Material_C.C1 <- ifelse(data3$Construction_Material_C == "C1", 1, 0)
# data3$Construction_Material_C.C2 <- ifelse(data3$Construction_Material_C == "C2", 1, 0)
# data3$Item_Type_C.C1 <- ifelse(data3$Item_Type_C == "C1", 1, 0)
# data3$Item_Type_C.C2 <- ifelse(data3$Item_Type_C == "C2", 1, 0)
# data3$Item_Type_C.C3 <- ifelse(data3$Item_Type_C == "C3", 1, 0)
# data3$FloorsType_C.C1 <- ifelse(data3$FloorsType_C == "C1", 1, 0)
# data3$FloorsType_C.C2 <- ifelse(data3$FloorsType_C == "C2", 1, 0)



# Remove Original Columns
# data3 <- data3 %>% dplyr::select(-Construction_Material_C, -Item_Type_C, -FloorsType_C)
data3$postcode_coast_len[is.na(data3$postcode_coast_len)] <- 0

# Take the logs of postcode_area, postcode_perimeter and postcode_coast_len
data3 <- data3 %>%
  dplyr::mutate(
    log_postcode_area = log(postcode_area),
    log_postcode_perimeter = log(postcode_perimeter),
    log_postcode_coast_len = ifelse(is.infinite(log(postcode_coast_len)), 0, log(postcode_coast_len))
  ) %>%
  dplyr::select(-postcode_area, -postcode_perimeter, -postcode_coast_len)


data3 <- data3 %>%
  dplyr::mutate(Item_Type_C = as.numeric(factor(Item_Type_C, levels = c("C1", "C2", "C3"))))



# Omit all NA values
data3 <- na.omit(data3)

# In Floors_No, set all floors above 4 to 4 due to limited variables above 4. We will name it as 4 and above
data3 <- data3 %>% 
  mutate(Floors_No = pmin(Floors_No, 4))

# Create data4 which is for Neural Networks
# Continous variables are processed with a min-max scalar 

# Define Min - Max scalar to rescale the continous predictors between [ -1 , 1]
PreProcess.Continuous <- function(data){
  2*(data - min(data))/(max(data) - min(data)) - 1
} 

data4 <- data3

data4$Building_Age <- PreProcess.Continuous(data4$Building_Age)
data4$Floors_No <-PreProcess.Continuous(data4$Floors_No)

# Preprocess data2 as well
data2$Building_Age <- PreProcess.Continuous(data2$Building_Age)
data2$Floors_No <-PreProcess.Continuous(data2$Floors_No)


# Create a learning sample index for cross-validation
ll <- sample(c(1:nrow(data3)), round(0.9*nrow(data3)), replace = FALSE)
ll1 <- sample(c(1:nrow(data4)), round(0.9*nrow(data4)), replace = FALSE)

# For Pareto Regression and Neural Networks
# learning sample
learn <- data3[ll,]
learn1 <- data4[ll1,]
learn2 <- data3[ll,]
# testing sample
test <- data3[-ll,]
test1 <- data4[-ll1,]
test2 <- data3[-ll,]



# Select the feature space
features <- c("Floors_No", "Building_Age", "Construction_Material_C", "Item_Type_C", 
              "FloorsType_C", "SubItemType_C", "nrOfFloors", "floodvulnerability",
              "windvulnerability", "postcode_coast_flg", "postcode_alt_mean",
              "postcode_slo_mean", "postcode_rgh_mean", "log_postcode_area",
              "log_postcode_perimeter", "log_postcode_coast_len")

# Feature matrices for training and test samples
Xlearn <- as.matrix(learn2[, features])
Xtest <- as.matrix(test2[, features])

# Rename categorical features by transforming and removing original columns
learn2 <- learn2 %>%
  dplyr::mutate(
    Construction_MaterialX = as.integer(factor(Construction_Material_C)) - 1,
    Item_TypeX = as.integer(factor(Item_Type_C)) - 1,
    FloorsTypeX = as.integer(factor(FloorsType_C)) - 1,
    SubItemTypeX = as.integer(factor(SubItemType_C)) - 1,
    postcode_coast_flgX = as.integer(factor(postcode_coast_flg)) - 1
  ) %>%
  dplyr::select(-Construction_Material_C, -Item_Type_C, -FloorsType_C, -SubItemType_C, -postcode_coast_flg)

# Rename numerical features and directly assign them to new variables
Floors_Nolearn <- learn2$Floors_No
Building_Agelearn <- learn2$Building_Age
Construction_Materiallearn <- learn2$Construction_MaterialX
Item_Typelearn <- learn2$Item_TypeX
FloorsTypelearn <- learn2$FloorsTypeX
SubItemTypelearn <- learn2$SubItemTypeX
postcode_coast_flglearn <- learn2$postcode_coast_flgX
nrOfFloorslearn <- learn2$nrOfFloors
floodvulnerabilitylearn <- learn2$floodvulnerability
windvulnerabilitylearn <- learn2$windvulnerability
postcode_alt_meanlearn <- learn2$postcode_alt_mean
postcode_slo_meanlearn <- learn2$postcode_slo_mean
postcode_rgh_meanlearn <- learn2$postcode_rgh_mean
log_postcode_arealearn <- learn2$log_postcode_area
log_postcode_perimeterlearn <- learn2$log_postcode_perimeter
log_postcode_coast_lenlearn <- learn2$log_postcode_coast_len

# Combine renamed variables into a feature matrix
Xlearn2 <- as.matrix(cbind(Floors_Nolearn, Building_Agelearn, Construction_Materiallearn, 
                           Item_Typelearn, FloorsTypelearn, SubItemTypelearn, postcode_coast_flglearn, 
                           nrOfFloorslearn, floodvulnerabilitylearn, windvulnerabilitylearn,
                           postcode_alt_meanlearn, postcode_slo_meanlearn, postcode_rgh_meanlearn, 
                           log_postcode_arealearn, log_postcode_perimeterlearn, log_postcode_coast_lenlearn))




# Rename categorical features by transforming and removing original columns in test2
test2 <- test2 %>%
  dplyr::mutate(
    Construction_MaterialX = as.integer(factor(Construction_Material_C)) - 1,
    Item_TypeX = as.integer(factor(Item_Type_C)) - 1,
    FloorsTypeX = as.integer(factor(FloorsType_C)) - 1,
    SubItemTypeX = as.integer(factor(SubItemType_C)) - 1,
    postcode_coast_flgX = as.integer(factor(postcode_coast_flg)) - 1
  ) %>%
  dplyr::select(-Construction_Material_C, -Item_Type_C, -FloorsType_C, -SubItemType_C, -postcode_coast_flg)

# Rename numerical features and directly assign them to new variables for test set
Floors_Notest <- test2$Floors_No
Building_Agetest <- test2$Building_Age
Construction_Materialtest <- test2$Construction_MaterialX
Item_Typetest <- test2$Item_TypeX
FloorsTypetest <- test2$FloorsTypeX
SubItemTypetest <- test2$SubItemTypeX
postcode_coast_flgtest <- test2$postcode_coast_flgX
nrOfFloorstest <- test2$nrOfFloors
floodvulnerabilitytest <- test2$floodvulnerability
windvulnerabilitytest <- test2$windvulnerability
postcode_alt_meantest <- test2$postcode_alt_mean
postcode_slo_meantest <- test2$postcode_slo_mean
postcode_rgh_meantest <- test2$postcode_rgh_mean
log_postcode_areatest <- test2$log_postcode_area
log_postcode_perimetertest <- test2$log_postcode_perimeter
log_postcode_coast_lentest <- test2$log_postcode_coast_len

# Combine renamed variables into a feature matrix for the test set
Xtest2 <- as.matrix(cbind(Floors_Notest, Building_Agetest, Construction_Materialtest, 
                          Item_Typetest, FloorsTypetest, SubItemTypetest, postcode_coast_flgtest, 
                          nrOfFloorstest, floodvulnerabilitytest, windvulnerabilitytest,
                          postcode_alt_meantest, postcode_slo_meantest, postcode_rgh_meantest, 
                          log_postcode_areatest, log_postcode_perimetertest, log_postcode_coast_lentest))


# dimension embedding layers for categorical features
d <- 2 

#SubItemTypeLabel <- length(unique(learn3$SubItemTypeX))
Construction_MaterialLabel <- length(unique(learn2$Construction_MaterialX))
Item_TypeLabel <- length(unique(learn2$Item_TypeX))
FloorsTypeLabel <- length(unique(learn2$FloorsTypeX))
SubItemTypeLabel <- length(unique(learn2$SubItemTypeX))
postcode_coast_flgLabel <- length(unique(learn2$postcode_coast_flgX))

