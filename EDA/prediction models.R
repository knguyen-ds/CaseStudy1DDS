# Load necessary libraries
library(dplyr)
library(e1071)
library(caret)

# Load the dataset
data <- read.csv("/Users/katie/Documents/MSDS/Doing Data Science/Unit 8/CaseStudy1-data.csv")

# Preprocessing: Convert categorical variables to factors
data$Attrition <- as.factor(ifelse(data$Attrition == "Yes", 1, 0))
data$Department <- as.factor(data$Department)
data$JobRole <- as.factor(data$JobRole)
data$Gender <- as.factor(data$Gender)
data$MaritalStatus <- as.factor(data$MaritalStatus)
data$OverTime <- as.factor(data$OverTime)

# Check for NA values and remove them
data <- na.omit(data)

# Select relevant features for modeling
features <- data %>%
  select(Age, DailyRate, DistanceFromHome, Education, EnvironmentSatisfaction,
         HourlyRate, JobInvolvement, JobLevel, JobRole, JobSatisfaction,
         MonthlyIncome, OverTime, PercentSalaryHike, PerformanceRating,
         RelationshipSatisfaction, StandardHours, TotalWorkingYears,
         TrainingTimesLastYear, WorkLifeBalance, YearsAtCompany,
         YearsInCurrentRole, YearsSinceLastPromotion, YearsWithCurrManager,
         Attrition)

# Split the data into training and testing sets
set.seed(123)  # For reproducibility
trainIndex <- createDataPartition(features$Attrition, p = 0.8, list = FALSE, times = 1)
train_data <- features[trainIndex, ]
val_data <- features[-trainIndex, ]

# Build the first Naive Bayes model (default)
nb_model1 <- naiveBayes(Attrition ~ ., data = train_data)

# Build the second Naive Bayes model with Laplace smoothing
nb_model2 <- naiveBayes(Attrition ~ ., data = train_data, laplace = 1)

# Make predictions on the test set
predictions_model1 <- predict(nb_model1, val_data)
predictions_model2 <- predict(nb_model2, val_data)

# Create confusion matrices for both models
confusion_matrix1 <- confusionMatrix(as.factor(predictions_model1), val_data$Attrition, mode = "everything")
confusion_matrix2 <- confusionMatrix(as.factor(predictions_model2), val_data$Attrition, mode = "everything")

# Extract evaluation metrics
results <- data.frame(
  Metric = c("Accuracy", "Sensitivity", "Specificity", "F1 Score"),
  Model1 = c(confusion_matrix1$overall['Accuracy'],
             confusion_matrix1$byClass['Sensitivity'],
             confusion_matrix1$byClass['Specificity'],
             confusion_matrix1$byClass['F1']),
  Model2 = c(confusion_matrix2$overall['Accuracy'],
             confusion_matrix2$byClass['Sensitivity'],
             confusion_matrix2$byClass['Specificity'],
             confusion_matrix1$byClass['F1'])
)

# Print the results
print(results)

plot_confusion_matrix <- function(cm, title = "Confusion Matrix") {
  cm_table <- as.data.frame(cm$table)
  ggplot(cm_table, aes(x = Reference, y = Prediction)) +
    geom_tile(aes(fill = Freq), color = "black") +
    geom_text(aes(label = Freq), vjust = 1, color = "black") +
    scale_fill_gradient(low = "white", high = "salmon") +
    labs(title = title, x = "Actual", y = "Predicted") +
    theme_minimal() +
    theme(axis.text.x = element_text(hjust = 1))
}

# Plot confusion matrices for both models
plot_confusion_matrix(confusion_matrix2, title = "Confusion Matrix for Model 2")


##### kNN Model
# Load necessary libraries
library(dplyr)
library(class)       # For kNN
library(caret)       # For model evaluation

# Load the dataset
DDSAnalytics <- read.csv('/kaggle/input/ibm-hr-analytics-attrition-dataset/WA_Fn-UseC_-HR-Employee-Attrition.csv')

# Preprocessing: Convert Attrition to numeric (0 and 1)
DDSAnalytics$Attrition <- ifelse(DDSAnalytics$Attrition == "Yes", 1, 0)

# Select only numeric variables (including the target variable)
numeric_data <- data %>%
  select(where(is.numeric), Attrition)

# Check for NA values and remove them
numeric_data <- na.omit(numeric_data)

# Split the data into training and testing sets
set.seed(123)  # For reproducibility
trainIndex <- createDataPartition(numeric_data$Attrition, p = .8, 
                                  list = FALSE, 
                                  times = 1)
train_data <- numeric_data[trainIndex, ]
val_data <- numeric_data[-trainIndex, ]

# Build the kNN model (k = 5)
k <- 5
knn_predictions <- knn(train = train_data[, -ncol(train_data)], 
                       test = val_data[, -ncol(val_data)], 
                       cl = train_data$Attrition, k = k)

# Create confusion matrix for kNN model
confusion_matrix_knn <- confusionMatrix(as.factor(knn_predictions), as.factor(val_data$Attrition))

# Extract evaluation metrics for kNN
knn_results <- data.frame(
  Metric = c("Accuracy", "Sensitivity", "Specificity", "F1 Score"),
  Value = c(confusion_matrix_knn$overall['Accuracy'],
            confusion_matrix_knn$byClass['Sensitivity'],
            confusion_matrix_knn$byClass['Specificity'],
            (2 * confusion_matrix_knn$byClass['Sensitivity'] * confusion_matrix_knn$byClass['Positive Predictive Value']) /
              (confusion_matrix_knn$byClass['Sensitivity'] + confusion_matrix_knn$byClass['Positive Predictive Value']))
)

# Print the kNN evaluation results
print(knn_results)
