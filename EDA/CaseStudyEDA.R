#Load necessary libraries
library(tidyverse)   # For data manipulation and visualization
library(caret)       # For model training and evaluation
library(GGally)      # For ggpairs visualization
library(dplyr)
library(reshape2)

#Load data
DDSAnalytics_original <-read.csv("/Users/katie/Documents/MSDS/Doing Data Science/Unit 8/CaseStudy1-data.csv")

#Check for and remove missing values/unnecessary columns
colSums(is.na(DDSAnalytics_original))
DDSAnalytics <- DDSAnalytics_original %>%
select(-c(ID, EmployeeCount, EmployeeNumber, Over18, StandardHours))

# Handle missing values (example: replacing with median for numerical, mode for categorical)
DDSAnalytics$some_numeric_var[is.na(DDSAnalytics$some_numeric_var)] <- median(DDSAnalytics$some_numeric_var, na.rm = TRUE)

# Replace NA for categorical variable with the most frequent value
mode_val <- as.character(names(sort(table(DDSAnalytics$some_categorical_var), decreasing = TRUE)[1]))
DDSAnalytics$some_categorical_var[is.na(DDSAnalytics$some_categorical_var)] <- mode_val

# Convert categorical variables to factors
DDSAnalytics$Attrition <- as.factor(DDSAnalytics$Attrition)
DDSAnalytics$JobRole <- as.factor(DDSAnalytics$JobRole)

# Plot the distribution of attrition
ggplot(DDSAnalytics, aes(x = Attrition)) +
  geom_bar() +
  labs(title = "Distribution of Attrition", x = "Attrition", y = "Count")

# Calculate attrition rate
attrition_counts <- DDSAnalytics %>%
  group_by(Attrition) %>%
  summarise(Count = n(), .groups = 'drop')

attrition_rate <- attrition_counts %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Create Pie Chart for Attrition Rate
pie_chart <- ggplot(attrition_rate, aes(x = "", y = Percentage, fill = Attrition)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Attrition Rate") +
  scale_fill_brewer(palette = "Set3") +
  theme_void()

# Select only numerical variables
DDSAnalytics <- DDSAnalytics %>%
  mutate(OverTime = ifelse(OverTime == "Yes", 1, 0))
numeric_data <- select_if(DDSAnalytics, is.numeric)

# Calculate the correlation matrix
cor_matrix <- cor(numeric_data, use = "pairwise.complete.obs")

# Convert the correlation matrix to a long format for ggplot
cor_melted <- melt(cor_matrix)

# Generate the heatmap
ggplot(data = cor_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "lightblue", high = "salmon", mid = "white", 
                       limit = c(-1, 1), name = "Correlation") +
  theme_minimal() +
  labs(title = "Correlation Heatmap", x = "Variables", y = "Variables") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#plot attrition yes/no against all numeric variables
numeric_data$Attrition <- DDSAnalytics$Attrition  # Add the Attrition column

ggplot(DDSAnalytics, aes(x = NumCompaniesWorked, fill = Attrition)) +
  geom_histogram(position = "identity", alpha = 0.6, bins = 30) +  # Overlapping bars
  labs(title = "Histogram of NumCompaniesWorked by Attrition", x = "NumCompaniesWorked", y = "Count") +
  scale_fill_manual(values = c("Yes" = "steelblue", "No" = "salmon")) +  # Custom colors
  theme_minimal() +  # Minimal theme for better aesthetics
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) 


##
selected_vars <- c("Age", "DailyRate", "DistanceFromHome", "Education", 
                   "EnvironmentSatisfaction", "HourlyRate", "JobInvolvement", 
                   "JobLevel", "JobSatisfaction", "MonthlyIncome", 
                   "MonthlyRate", "NumCompaniesWorked")

# Set up the plotting area to have multiple plots
par(mfrow = c(4, 3))

# Create bar plots of attrition (yes vs no) for selected numeric variables
for (var in selected_vars) {
  # Calculate counts for each attrition status
  counts <- DDSAnalytics %>%
    group_by(!!sym(var), Attrition) %>%
    summarise(count = n(), .groups = 'drop')
  
  # Create the plot
  barplot_heights <- counts$count[counts$Attrition == "Yes"]
  barplot_heights_no <- counts$count[counts$Attrition == "No"]
  
  barplot_height <- c(barplot_heights, barplot_heights_no)
  names(barplot_height) <- c(paste(var, "Yes"), paste(var, "No"))
  
  # Create a bar plot
  barplot(barplot_height, 
          beside = FALSE, 
          col = c("skyblue", "salmon"),
          main = paste( var), 
          ylab = "Count", 
          xlab = var)
}

selected_vars2 <- c("OverTime","PercentSalaryHike", 
                   "PerformanceRating", "RelationshipSatisfaction", 
                   "StockOptionLevel", "TotalWorkingYears", 
                   "TrainingTimesLastYear", "WorkLifeBalance", 
                   "YearsAtCompany", "YearsInCurrentRole", 
                   "YearsSinceLastPromotion", "YearsWithCurrManager")

par(mfrow = c(4, 3))

# Create bar plots of attrition (yes vs no) for selected numeric variables
for (var in selected_vars2) {
  # Calculate counts for each attrition status
  counts <- DDSAnalytics %>%
    group_by(!!sym(var), Attrition) %>%
    summarise(count = n(), .groups = 'drop')
  
  # Create the plot
  barplot_heights <- counts$count[counts$Attrition == "Yes"]
  barplot_heights_no <- counts$count[counts$Attrition == "No"]
  
  barplot_height <- c(barplot_heights, barplot_heights_no)
  names(barplot_height) <- c(paste(var, "Yes"), paste(var, "No"))
  
  # Create a bar plot
  barplot(barplot_height, 
          beside = FALSE, 
          col = c("skyblue", "salmon"),
          main = paste( var), 
          ylab = "Count", 
          xlab = var)
}

# Set up the graphics layout
par(mfrow = c(4, 3))  # Adjust rows and columns as needed


# Loop through each variable to create histograms
for (var in selected_vars2) {
  # Create a histogram for each selected variable
  hist(DDSAnalytics[[var]][DDSAnalytics$Attrition == "Yes"],
       col = rgb(1, 0, 0, 0.5),  # Red for attrition "Yes"
       xlim = range(DDSAnalytics[[var]], na.rm = TRUE),
       main = paste(var),
       xlab = var,
       ylab = "Count",
       freq = TRUE)
  
  hist(DDSAnalytics[[var]][DDSAnalytics$Attrition == "No"],
       col = rgb(0, 0, 1, 0.5),  # Blue for attrition "No"
       add = TRUE)
}

# Add a legend
legend("right", legend = c("Yes", "No"),
       fill = c(rgb(1, 0, 0, 0.5),rgb(0, 0, 1, 0.5)),bty = "n")

# Business Travel vs Attrition
traveldata <- DDSAnalytics %>%
  group_by(BusinessTravel, Attrition) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Plot the histogram
ggplot(traveldata, aes(x = BusinessTravel, y = Count, fill = Attrition)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5) +
  labs(title = "Business Travel vs Attrition",
       x = "Business Travel",
       y = "Count") +
  theme_minimal() +
  scale_fill_manual(values = c("Yes" = "red", "No" = "blue")) # Optional colors

# Business Travel vs Attrition
travelTime <- DDSAnalytics_original %>%
  group_by(BusinessTravel, Attrition) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Plot the histogram
ggplot(travelTime, aes(x = BusinessTravel, y = Count, fill = Attrition)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5) +
  labs(title = "BusinessTravel vs Attrition",
       x = "BusinessTravel",
       y = "Count") +
  theme_minimal() +
  scale_fill_manual(values = c("Yes" = "red", "No" = "blue")) # Optional colors

average_age_table <- DDSAnalytics_original %>%
  +     group_by(Department, JobRole, Gender) %>%
  +     summarise(AverageAge = mean(Age, na.rm = TRUE), .groups = 'drop') %>%
  +     pivot_wider(names_from = Gender, values_from = AverageAge, names_prefix = "AverageAge_") %>%
  +     rename(AverageAge_Female = AverageAge_Female, AverageAge_Male = AverageAge_Male)
