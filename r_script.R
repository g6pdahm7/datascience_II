#' Final Project
#' Prepared by Mausam, Ahmed, and Vian.

#' Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(survival)
library(car)
library(funModeling)

#' Columns that were not highlighted were removed a priori
#' on Excel. Since we are using Git, the same file should 
#' be available for all users. Should someone prefer to
#' remove the columns through code, you can find the 
#' raw file in the repo, and use the commented code.

#' Uploading the data
data <- read_excel("transfusion_data.xlsx")

#' Preliminary view of the data
View(data)
status(data)

#' Remove unnecessary columns
#' columns_to_remove <- c(
#  "TX DB ID", "OR Date", "Coronary Artery Disease", "Hypertension",
#  "Diabetes (insulin)", "Diabetes (diet/OHGs)", "GERD/PUD", "Renal Failure",
#  "Stroke/CVA", "Liver Disease", "Thyroid Disease", "DCD vs DBD",
#  "Protamine (Y=1 N=0)", "Intra_Albumin 5% (mL)", "Intra_Crystalloid (mL)",
#  "Intra_Cell Saver returned (mL)", "Intra_PCC/Octaplex", "Blood Loss",
#  "Urine Output", "Fluid Balance", "Tranexamic Acid Used",
#  "ICU Admission Date/Time", "ICU Discharge Date/Time", "Date of Extubation",
#  "Duration of Ventilation", "PostImmediate_PTT", "PostImmediate_Fibrinogen",
#  "PostImmediate_Creatinine", "PostDay1_Hb", "PostDay1_Hct",
#  "PostDay1_Platelets", "PostDay1_PT", "PostDay1_INR", "PostDay1_PTT",
#  "PostDay1_Fibrinogen", "PostDay1_Creatinine",
#  "Need for reoperation for bleeding within 24h"
#)

#' Remove the columns
#data <- data %>% select(-all_of(columns_to_remove))

#' For neatness, we will rename the columns to remove any
#' spaces or brackets that may exist. 
colnames(data) <- gsub("[[:space:]]|[()]", "_", colnames(data))

#' Next we will do some basic cleaning, without any imputation.

#' First we will start by reformatting the gender column. 
#' Column will be renamed, and appropriate gender will be inputted.
colnames(data)[colnames(data) == "Gender__male_"] <- "Gender"
data$Gender <- ifelse(data$Gender == "TRUE", "Male", "Female")

#' Next, we will look at the proportions of NA for each column.
# LAS_score                         0.062500000
# Pre_PTT                           0.005208333
# Pre_Fibrinogen                    0.973958333
# Duration_of_ICU_Stay__days_       0.005208333
# DEATH_DATE                        0.833333333
# RBC_0-24hrs                       0.687500000
# RBC_24-48hrs                      0.729166667     0     0
# RBC_48-72hrs                      0.750000000
# FFP_0-24hrs                       0.802083333     0     0
# FFP_24-48hrs                      0.828125000     0     0
# FFP_48-72hrs                      0.828125000
# Plt_0-24hrs                       0.802083333     0     0
# Plt_24-48hrs                      0.812500000     0     0
# Plt_48-72hrs                      0.822916667
# Cryo_0-24hrs                      0.807291667     0     0
# Cryo_24-48hrs                     0.828125000     0     0
# Cryo_48-72hrs                     0.828125000

#' Some initial thoughts: 
#' Pre_Fibrinogen will have to be excluded due to 
#' missingness greater than 30%.












