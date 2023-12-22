# Clear the environment
rm(list = ls(all = TRUE))

# Load required libraries
library(readxl)
library(topsis)
library(factoextra)
library(cluster)

# Read the Male mortality data from Excel
Male_data <- read_xlsx("C:\\Users\\Shiv\\Documents\\data mining\\ASS_3\\Mortality_data.xlsx", sheet = 1)
Male_data <- data.frame(Male_data, row.names = 1)
Male_data <- as.matrix(Male_data)

# Set weights and impacts for TOPSIS analysis
wt <- rep(1, 6)
ind <- rep("+", 6)

# Perform TOPSIS analysis on Male data
T1 <- topsis(Male_data, weights = wt, impacts = ind)
rownames(T1) <- rownames(Male_data)
T1 <- T1[order(T1$rank, decreasing = FALSE),]

# Read the Female mortality data from Excel
Female_data <- read_xlsx("C:\\Users\\Shiv\\Documents\\data mining\\ASS_3\\Mortality_data.xlsx", sheet = 2)
Female_data <- data.frame(Female_data, row.names = 1)
Female_data <- as.matrix(Female_data)

# Perform TOPSIS analysis on Female data
T2 <- topsis(Female_data, weights = wt, impacts = ind)
rownames(T2) <- rownames(Female_data)
T2 <- T2[order(T2$rank, decreasing = FALSE),]

# Perform clustering on Male data using k-means
k <- kmeans(Male_data, 2)
fviz_cluster(k, data = Male_data)

# Perform clustering on Male data using PAM
h <- pam(Male_data, 3)
fviz_cluster(h, data = Male_data)

# Read Observed Mortality Rates data from Excel
data <- read_xlsx("C:\\Users\\Shiv\\Documents\\data mining\\ASS_3\\Observed Mortality Rates.xlsx", sheet = 2)
age_grp <- data$`AGE GROUP`
all <- rowMeans(data[, -1])

# Perform linear regression
m <- lm(data$Year6 ~ all - 1 + as.factor(age_grp))

# Predict mortality rates using the linear regression model
p <- predict(m, data = Male_data)

# Compute correlation between actual and predicted mortality rates
cor(Male_data[, 6], p)

# Plot residuals of the linear regression model
plot(m$residuals)

# Plot mortality rates for different age groups (M vs F)
for (i in 1:18) {
  plot(Male_data[i, ], type = "l", col = "red", xlab = "1986-1990 to 2011-2015", ylab = "Mortality Rate",
       main = paste("Mortality Rate M VS F for AgeGroup", i - 1, "-", i))
  lines(Female_data[i, ], col = "blue")
  legend("topright", legend = c("Male", "Female"), col = c("red", "blue"), lty = 1, lwd = 2)
}







