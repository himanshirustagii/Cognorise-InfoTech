# Load necessary libraries
install.packages("tidyr")
install.packages("lubridate")
install.packages("car")

library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(car)

# Load the dataset
data <- read_excel(file.choose())

# Data Cleaning
installed.packages("dplyr")
library(dplyr)

# Convert Dt_Customer to Date type for time-based analysis
data$Dt_Customer <- as.Date(data$Dt_Customer)

# Impute missing values in the Income column with the median
data$Income[is.na(data$Income)] <- median(data$Income, na.rm = TRUE)

# Calculate Age from Year_Birth for demographic analysis
data$Age <- as.numeric(format(Sys.Date(), "%Y")) - data$Year_Birth

# Remove columns with constant values as they do not contribute to the analysis
data <- select(data, -c(Z_CostContact, Z_Revenue))

# Data Exploration and Visualization

# Age Distribution of Customers
ggplot(data, aes(x = Age)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  theme_minimal() +
  ggtitle("Age Distribution of Customers") +
  xlab("Age") +
  ylab("Frequency")

# Inference: Shows a diverse age range of customers with a focus on the 
# 30-60 age group.

# Income Distribution of Customers
ggplot(data, aes(x = Income)) +
  geom_histogram(bins = 30, fill = "green", color = "black") +
  theme_minimal() +
  ggtitle("Income Distribution of Customers") +
  xlab("Income") +
  ylab("Frequency")

# Inference: Income is right-skewed, indicating most customers are in the moderate 
# income range.

# Education Level of Customers
ggplot(data) +
  geom_bar(aes(x = reorder(Education, Education, function(x)-length(x))), 
           fill = "orange") +
  theme_minimal() +
  ggtitle("Education Level of Customers") +
  xlab("Education Level") +
  ylab("Count") +
  coord_flip()

# Inference: Most customers are well-educated, with 'Graduation' being the
# most common.

# Marital Status of Customers
ggplot(data) +
  geom_bar(aes(x = reorder(Marital_Status, Marital_Status, function(x)-length(x))),
           fill = "purple") +
  theme_minimal() +
  ggtitle("Marital Status of Customers") +
  xlab("Marital Status") +
  ylab("Count") +
  coord_flip()

# Inference: A significant number of customers are married or living together.

# Total Spending in Different Categories
data$Total_Spending <- rowSums(data[,c("MntWines", "MntFruits", "MntMeatProducts", 
                                       "MntFishProducts", "MntSweetProducts", "MntGoldProds")])

# Relationships Between Demographics and Spending Habits

# Total Spending vs Age
ggplot(data, aes(x = Age, y = Total_Spending)) +
  geom_point(color = "red") +
  theme_minimal() +
  ggtitle("Total Spending vs Age") +
  xlab("Age") +
  ylab("Total Spending")

# Inference: Middle-aged customers tend to spend more, with a decline in spending
# for older age groups.

# Total Spending vs Income
ggplot(data, aes(x = Income, y = Total_Spending)) +
  geom_point(color = "red") +
  theme_minimal() +
  ggtitle("Total Spending vs Income") +
  xlab("Income") +
  ylab("Total Spending")

# Inference: Positive correlation between income and total spending.

# Total Spending by Education Level
ggplot(data, aes(x = Total_Spending, y = Education)) +
  geom_boxplot(fill = "lightblue") +
  theme_minimal() +
  ggtitle("Total Spending by Education Level") +
  xlab("Total Spending") +
  ylab("Education")

# Inference: Higher education levels, particularly PhD, correspond to higher spending.

# Total Spending by Marital Status
ggplot(data, aes(x = Total_Spending, y = Marital_Status)) +
  geom_boxplot(fill = "lightgreen") +
  theme_minimal() +
  ggtitle("Total Spending by Marital Status") +
  xlab("Total Spending") +
  ylab("Marital Status")

# Inference: Marital status does not significantly affect spending habits.

# Correlation Analysis
numeric_columns <- sapply(data, is.numeric)
correlation_matrix <- cor(data[, numeric_columns], use = "complete.obs")
corrplot(correlation_matrix, method = "color")

#Inference 
"Income and Total Spending: 
There is a strong positive correlation between 'Income' and 'Total Spending'.
This indicates that as income increases, total spending on products also tends
to increase.

Age and Spending:
Age shows moderate correlations with different spending categories. For example,
there might be a noticeable relationship between age and spending on wines.

Recency and Spending :
The 'Recency' variable, indicating the number of days since the last purchase,
shows low to moderate negative correlations with spending categories. 
This might suggest that recent customers tend to spend more."

# ANOVA Test - Total Spending by Education Level
anova_model <- aov(Total_Spending ~ Education, data = data)
anova_result <- anova(anova_model)
print(anova_result)

# Interpretation:
# If the p-value is less than 0.05, it indicates significant differences in total
# spending across education levels.
