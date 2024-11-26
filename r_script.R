#' Final Project
#' Prepared by Mausam, Ahmed, and Vian.

#' Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(survival)
library(car)
library(funModeling)
library(corrplot)

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

#' Next, we will perform a simple EDA prior to 
#' handling the any missingness.

#' Before that, I want to create a logical column
#' that states whether or not someone had a transplant.
data <- data %>%
  mutate(Transfusion = ifelse(Total_24hr_RBC > 0, TRUE, FALSE))

#' Bar plot for the number of people with and without transfusions
ggplot(data, aes(x = Transfusion)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Number of People Who Had Transfusions",
       x = "Transfusion (TRUE = Yes, FALSE = No)",
       y = "Count") +
  theme_minimal()

#' Bar plot for Type of transplant
ggplot(data, aes(x = Type)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribution of Transplant Types", x = "Type", y = "Count")

#' Histogram of `Age`
ggplot(data, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Age Distribution of Patients", x = "Age", y = "Frequency")

#' Relationship between BMI and ICU Length of Stay
ggplot(data, aes(x = BMI, y = Duration_of_ICU_Stay__days_)) +
  geom_point(color = "darkblue") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "BMI vs ICU Length of Stay", x = "BMI", y = "ICU Stay Duration (days)")

#' Stacked bar chart for Gender by Type
ggplot(data, aes(x = Gender, fill = Type)) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of Transplant Types by Gender", x = "Gender", y = "Proportion")

#' Boxplot of total RBC transfusion based on 30-day survival
ggplot(data, aes(x = ALIVE_30DAYS_YN, y = Total_24hr_RBC)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Total RBC Transfusion by 30-Day Survival", x = "Survived 30 Days (Y/N)", y = "Total RBC Units")

#' Next, I want to create a simple correlation plot of some of 
#' the patient characteristics, and some of the general outcomes:

#' We will start by making sure everything is numeric
data$Gender <- as.numeric(data$Gender == "Male")  
data$Type <- as.numeric(as.factor(data$Type))   
data$Transfusion <- as.numeric(data$Transfusion) 
data$ALIVE_30DAYS_YN <- as.numeric(data$ALIVE_30DAYS_YN == "Y")  
data$ALIVE_90DAYS_YN <- as.numeric(data$ALIVE_90DAYS_YN == "Y")  
data$ALIVE_12MTHS_YN <- as.numeric(data$ALIVE_12MTHS_YN == "Y") 

#' Define groups of variables
group1 <- data %>%
  select(Age, Gender, Weight, Height, BMI, Type)

group2 <- data %>%
  select(Transfusion, ICU_LOS, HOSPITAL_LOS, ALIVE_30DAYS_YN, ALIVE_90DAYS_YN, ALIVE_12MTHS_YN)

#' Compute correlation matrix between group1 and group2
cor_matrix <- cor(group1, group2, use = "pairwise.complete.obs")

#' Plot the correlation heatmap
corrplot(cor_matrix, method = "color", is.corr = TRUE, 
         tl.cex = 0.8, number.cex = 0.7,
         title = "Correlation Plot Between Predictors and Outcomes",
         mar = c(0, 0, 1, 0))




