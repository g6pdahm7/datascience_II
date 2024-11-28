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
library(mice)
library(glmnet)

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
# LAS_score                         0.062500000     #IMPUTE (numeric)
# Pre_PTT                           0.005208333     #IMPUTE (numeric)
# Pre_Fibrinogen                    0.973958333     #REMOVE >30% missingness
# Duration_of_ICU_Stay__days_       0.005208333     #REMOVE (redundant)
# DEATH_DATE                        0.833333333     # Leave as NA
# RBC_0-24hrs                       0.687500000     # Replace NA with 0
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

#' Create a new data set to view correction
correlation_data <- data

#' We will start by making sure everything is numeric
correlation_data$Gender <- as.numeric(data$Gender == "Male")  
correlation_data$Type <- as.numeric(as.factor(data$Type))   
correlation_data$Transfusion <- as.numeric(data$Transfusion) 
correlation_data$ALIVE_30DAYS_YN <- as.numeric(data$ALIVE_30DAYS_YN == "Y")  
correlation_data$ALIVE_90DAYS_YN <- as.numeric(data$ALIVE_90DAYS_YN == "Y")  
correlation_data$ALIVE_12MTHS_YN <- as.numeric(data$ALIVE_12MTHS_YN == "Y")

#' Define groups of variables
group1 <- correlation_data %>%
  select(Age, Gender, Weight, Height, BMI, Type)

group2 <- correlation_data %>%
  select(Transfusion, ICU_LOS, HOSPITAL_LOS, ALIVE_30DAYS_YN, ALIVE_90DAYS_YN, ALIVE_12MTHS_YN)

#' Compute correlation matrix between group1 and group2
cor_matrix <- cor(group1, group2, use = "pairwise.complete.obs")

#' Plot the correlation heatmap
corrplot(cor_matrix, method = "color", is.corr = TRUE, 
         tl.cex = 0.8, number.cex = 0.7,
         title = "Correlation Plot Between Predictors and Outcomes",
         mar = c(0, 0, 1, 0))


#' We will now create histograms for the two columns we will impute.
#' This will help inform the imputation method we will use.

#' Histogram for LAS_score
ggplot(data, aes(x = LAS_score)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Distribution of LAS Score", x = "LAS Score", y = "Frequency") +
  theme_minimal()
#' Slightly right skewed 

#' Histogram for Pre_PTT
ggplot(data, aes(x = Pre_PTT)) +
  geom_histogram(binwidth = 5, fill = "lightgreen", color = "black") +
  labs(title = "Distribution of Pre PTT", x = "Pre PTT", y = "Frequency") +
  theme_minimal()
#' Right skewed

#' Additional cleaning and Imputation

#' We will start by removing an unnecessary column that is 
#' redundant with another column for icu stay.
data <- data %>% select(-Duration_of_ICU_Stay__days_)

#' Next, we are going to remove this column "Pre_Fibrinogen" because
#' it has missingness greater than 30%.
data <- data %>% select(-Pre_Fibrinogen)

#' List of column names to replace NAs with 0
placeholder <- c(
  "RBC_0-24hrs", "RBC_24-48hrs", "RBC_48-72hrs", "RBC_72hr_Total",
  "FFP_0-24hrs", "FFP_24-48hrs", "FFP_48-72hrs", "FFP_72hr_Total",
  "Plt_0-24hrs", "Plt_24-48hrs", "Plt_48-72hrs", "Plt_72hr_Total",
  "Cryo_0-24hrs", "Cryo_24-48hrs", "Cryo_48-72hrs", "Cryo_72hr_Total"
)

# Replace NAs with 0 in the specified columns
data[placeholder] <- lapply(data[placeholder], function(x) {
  x[is.na(x)] <- 0
  return(x)
})

#' Some additional cleaning: 
#' Removing redundant columns:
data <- data %>% select(-First_Lung_Transplant)

#' Creating a new column with the type of life support
#' Combining 3 columns into one.
#' Set everything in the new column as none.
data$ECLS_Type <- "None"

#' Assign "ECMO" where ECLS_ECMO is TRUE and ECLS_CPB is FALSE
data$ECLS_Type[data$ECLS_ECMO == TRUE & data$ECLS_CPB == FALSE] <- "ECMO"

#' Assign "CPB" where ECLS_CPB is TRUE and ECLS_ECMO is FALSE
data$ECLS_Type[data$ECLS_CPB == TRUE & data$ECLS_ECMO == FALSE] <- "CPB"

#' Convert ECLS_Type to a factor with specified levels
data$ECLS_Type <- factor(data$ECLS_Type, levels = c("None", "ECMO", "CPB"))

#' Verify the distribution
table(data$ECLS_Type)

#' Now that we have solved the redundancy, we will
#' remove the unnecessary columns.
data <- data %>% select(-Intraoperative_ECLS)
data <- data %>% select(-ECLS_ECMO)
data <- data %>% select(-ECLS_CPB)


data <- data %>% mutate_if(is.character, as.factor)
colnames(data) <- gsub("[#-]", "_", colnames(data))

#' Imputation


#' Converting character variables to factors
data <- data %>% mutate_if(is.character, as.factor)


#' Generate default methods vector
methods <- make.method(data)

# Set all methods to "" (no imputation)
methods[] <- ""

# Set methods for variables to impute
methods["LAS_score"] <- "pmm"
methods["Pre_PTT"] <- "pmm"

# Predictors for LAS_score
predictors_LAS <- c("Age", "Gender", "BMI", "COPD", "Type")

# Predictors for Pre_PTT
predictors_PTT <- c("Pre_Hb", "Pre_Hct", "Pre_Platelets", "Pre_PT", "Pre_INR", "Pre_Creatinine")

# Initialize predictor matrix with zeros
pred_matrix <- make.predictorMatrix(data)
pred_matrix[,] <- 0  # Set all entries to 0

# Set predictors for LAS_score
pred_matrix["LAS_score", predictors_LAS] <- 1

# Set predictors for Pre_PTT
pred_matrix["Pre_PTT", predictors_PTT] <- 1

# Perform single imputation
imputed111 <- mice(
  data,
  method = methods,
  predictorMatrix = pred_matrix,
  m = 1,
  maxit = 5,
  seed = 123
)

# Complete the data
data_imputed <- complete(imputed111)

# Update the original data
data$LAS_score <- data_imputed$LAS_score
data$Pre_PTT <- data_imputed$Pre_PTT



#' The following plots are used to visualize the imputed 
#' data. 
xyplot(imputed111, LAS_score ~ Gender)
xyplot(imputed111, Pre_PTT ~ Pre_Hct)






#' Analysis 



################# LASSO CLASSIFICATION

#' Next we are going to identify the predictors that 
#' we will be using in the Lasso classification model. 
x <- c(
  "Type", "Gender", "Height", "Weight", "Age", "BMI", "COPD",
  "alpha1_Antitrypsin_Deficiency", "Cystic_Fibrosis",
  "Idiopathic_Pulmonary_Hypertension", "Interstitial_Lung_Disease",
  "Pulm_Other", "Redo_Lung_Transplant", "ExVIVO_Lung_Perfusion",
  "Preoperative_ECLS", "LAS_score", "Pre_Hb", "Pre_Hct",
  "Pre_Platelets", "Pre_PT", "Pre_INR", "Pre_PTT", "Pre_Creatinine"
)

#' Subsetting the model data
model1data <- data[, c(x, "Transfusion")]

#' Next we need to make the matrix for the predictors, 
#' with dummy variables.
x <- model.matrix(Transfusion ~ ., data = model1data)

y <- as.numeric(model1data$Transfusion) - 1

#' Train the model (hopefully it works lol)

modelxx1 <- glmnet(x, y, family = "binomial")
plot(modelxx1,label = T, xvar = "lambda")


#' Setting seed for reproducibility and doing cross-validation.
set.seed(123)
cv.lasso <- cv.glmnet(x, y, nfolds = 5)

#' Plotting MSE vs log lambda
plot(cv.lasso)


#' Optimal lambda value that minimizes MSE
optimal_lambda <- cv.lasso$lambda.min
# MSE corresponding to optimal lambda
optimal_mse <- cv.lasso$cvm[cv.lasso$lambda == optimal_lambda]

#' Coefficients at optimal lambda
optimal_coefs <- coef(cv.lasso, s = "lambda.min")
print(optimal_coefs)




################### Model 2: Continuous outcome.
#' Identical process to the one used above, just 
#' different family.

# Define predictors and outcome with updated variable names
predictors22 <- c(
  "Type", "Gender", "Height", "Weight", "Age", "BMI", "COPD",
  "alpha1_Antitrypsin_Deficiency", "Cystic_Fibrosis",
  "Idiopathic_Pulmonary_Hypertension", "Interstitial_Lung_Disease",
  "Pulm_Other", "Redo_Lung_Transplant", "ExVIVO_Lung_Perfusion",
  "Preoperative_ECLS", "LAS_score", "Pre_Hb", "Pre_Hct",
  "Pre_Platelets", "Pre_PT", "Pre_INR", "Pre_PTT", "Pre_Creatinine"
)

# Subset data for the new model
model22data <- data[, c(predictors22, "Total_24hr_RBC")]

# Create design matrix for predictors and outcome with updated variable names
x22 <- model.matrix(Total_24hr_RBC ~ ., data = model22data)[, -1]
y22 <- model22data$Total_24hr_RBC

# Train Lasso model
lasso_model22 <- glmnet(x22, y22, family = "gaussian")

# Plot coefficients vs log(lambda)
plot(lasso_model22, xvar = "lambda", label = TRUE)

# Cross-validation to find optimal lambda
set.seed(123)
cv_lasso22 <- cv.glmnet(x22, y22, family = "gaussian", nfolds = 5)

# Plot cross-validation curve
plot(cv_lasso22)

# Extract optimal lambda
optimal_lambda22 <- cv_lasso22$lambda.min
print(paste("Optimal Lambda:", optimal_lambda22))

# Coefficients at optimal lambda
optimal_coefs22 <- coef(cv_lasso22, s = "lambda.min")
print(optimal_coefs22)













###############


# Load pROC package if not already loaded
library(pROC)

# Create ROC curve and calculate AUC
roc_lasso <- roc(y_test, as.numeric(pred_probs_lasso))
auc_lasso <- auc(roc_lasso)

# Plot ROC curve
plot(roc_lasso, main = paste("Lasso Regression ROC Curve (AUC =", round(auc_lasso, 2), ")"))





################## TREE STUFF
data$Transfusion <- as.factor(data$Transfusion)

# Construct the formula
tree_formula <- as.formula(paste("Transfusion ~", paste(x, collapse = " + ")))


# Build the classification tree model
tree_model <- tree(tree_formula, data = data)

# Plot the tree
plot(tree_model)
text(tree_model, pretty = 0)








