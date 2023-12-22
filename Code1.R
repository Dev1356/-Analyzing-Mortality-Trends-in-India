# Remove all objects from the current environment
rm(list=ls(all=T))

# Load required libraries
library(readxl)
library(dplyr)
library(tree)

# Read data from an Excel file
d_M <- read_xlsx("C:\\Users\\Shiv\\Documents\\data mining\\ASS_3\\Observed Mortality Rates.xlsx", sheet = 1)

# Remove rows with missing values
d_M <- na.omit(d_M)

# Display the first few rows of the data
head(d_M)

# Convert data to a data frame
d1 <- data.frame(d_M)

# Remove rows with missing values
d1 <- na.omit(d1)

# Convert columns to appropriate data types
d1$Mortality_rate <- as.numeric(d1$Mortality_rate)
d1$Gender <- as.factor(d1$Gender)
d1$AGE.GROUP <- as.factor(d1$AGE.GROUP)
d1$Year <- as.factor(d1$Year)

# Load the necessary library for the boxcox() function
library(MASS)

# Calculate Box-Cox transformation parameter
k <- boxcox()  # You might want to provide the required arguments for the boxcox() function

# Get the number of rows in the data
n <- dim(d1)[1]

# Define the value of k (you might want to use a different value)
k <- 3

# Create a vector of sample indices
sample_data <- sample(1:n, size = n, replace = F)

# Create an empty matrix for storing results
output <- matrix(, nrow = 4, ncol = 2)

# Create a matrix for storing fold indices
Fold <- matrix(sample_data, ncol = k)

# Create empty vectors for storing MSE values
MSE_train_data <- MSE_test_data <- c()

#-------------------------------------- Decision tree
# Perform k-fold cross-validation
for(i in 1:k) {
  fisrtFold <- Fold[, i]
  train_data <- d1[fisrtFold, ]
  test_data <- d1[-fisrtFold, ]
  T1 <- tree(Mortality_rate ~ ., data = train_data)
  plot(T1)
  text(T1)
  PT1 <- predict(T1, data = train_data)
  MSE_train_data[i] <- mean((PT1 - train_data$Mortality_rate)^2)
  PT2 <- predict(T1, newdata = test_data)
  MSE_test_data[i] <- mean((PT2 - test_data$Mortality_rate)^2)
}

# Calculate the mean MSE for training and test data
output[1, 2] <- mean(MSE_test_data)
output[1, 1] <- mean(MSE_train_data)
output

#---------------------------------------- Random Forest
# Perform k-fold cross-validation
for(i in 1:k) {
  fisrtFold <- Fold[, i]
  train_data <- d1[fisrtFold, ]
  test_data <- d1[-fisrtFold, ]
  R1 <- randomForest(Mortality_rate ~ ., data = train_data, ntree = 50)
  RT1 <- predict(R1, data = train_data)
  MSE_train_data[i] <- mean((RT1 - train_data$Mortality_rate)^2)
  RT2 <- predict(R1, newdata = test_data)
  MSE_test_data[i] <- mean((RT2 - test_data$Mortality_rate)^2)
}

# Calculate the mean MSE for training and test data
output[2, 2] <- mean(MSE_test_data)
output[2, 1] <- mean(MSE_train_data)
output

#---------------------------------------- Bagging
# Perform k-fold cross-validation
for(i in 1:k) {
  fisrtFold <- Fold[, i]
  train_data <- d1[fisrtFold, ]
  test_data <- d1[-fisrtFold, ]
  b1 <- randomForest(Mortality_rate ~ ., data = train_data, ntree = 50, mtry = 3)
  bT1 <- predict(b1, data = train_data)
  MSE_train_data[i] <- mean((bT1 - train_data$Mortality_rate)^2)
  bT2 <- predict(b1, newdata = test_data)
  MSE_test_data[i] <- mean((bT2 - test_data$Mortality_rate)^2)
}

# Calculate the mean MSE for training and test data
output[3, 2] <- mean(MSE_test_data)
output[3, 1] <- mean(MSE_train_data)
output

#--------------------------------------- Boosting
# Load the necessary library for the gbm() function
library(gbm)

# Perform k-fold cross-validation
for(i in 1:k) {
  fisrtFold <- Fold[, i]
  train_data <- d1[fisrtFold, ]
  test_data <- d1[-fisrtFold, ]
  B1 <- gbm(Mortality_rate ~ ., data = train_data, n.tree = 50, distribution = "gaussian")
  BT1 <- predict(B1, data = train_data)
  MSE_train_data[i] <- mean((BT1 - train_data$Mortality_rate)^2)
  BT2 <- predict(B1, newdata = test_data)
  MSE_test_data[i] <- mean((BT2 - test_data$Mortality_rate)^2)
}

# Calculate the mean MSE for training and test data
output[4, 2] <- mean(MSE_test_data)
output[4, 1] <- mean(MSE_train_data)
output

# Define row and column names for the output matrix
g1 <- c("MSE_TrainData", "MSE_TestData")
g2 <- c("DecisionTree", "RandomForest", "Bagging", "Boosting")
rownames(output) <- g2
colnames(output) <- g1
output


# Insights on data

# 1) Perform linear regression on a different data sheet
data <- read_xlsx("C:\\Users\\Shiv\\Documents\\data mining\\ASS_3\\Observed Mortality Rates.xlsx", sheet = 2)
age_grp <- data$`AGE GROUP`
data
all <- rowMeans(data[, 2:6])
all
m <- lm(data$Year6 ~ all - 1 + as.factor(age_grp))
m
p <- predict(m, data = Male_data)  # It seems that Male_data is not defined
p
cor(Male_data[, 6], p)
plot(m$residuals, type = "b")
View(data)  # Opens the data in a spreadsheet-like viewer

# 2) Calculate correlation between each year and a specific column
d_small <- d_M[, -c(1, 7)]
d_small
correlation <- c()
for(i in 1:ncol(d_small)) {
  correlation[i] <- cor(d_M$X2011.2015, d_small[, i])
}
correlation

# Convert a column to a factor
d_M$AGE.GROUP <- as.factor(d_M$AGE.GROUP)
str(d_M)

# Perform linear regression on the d_M data
m1 <- lm(AGE.GROUP ~ ., data = d_M)
p1 <- predict(m1, data = m_D)  # It seems that m_D is not defined








