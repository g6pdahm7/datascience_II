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

