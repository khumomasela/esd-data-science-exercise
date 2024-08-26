library(tidyverse)
library(caret)
library(ggplot2)
install.packages("randomForest")
library(randomForest)

#Set seed for reproducibility
set.seed(123)

# Create training and testing sets from the cleaned data in part 1
# 80% of the data will be used to build the model and 20% of the will be used to test if the model is performing well.
trainIndex <- createDataPartition(cleaned_data$GDP, p=0.8, list=FALSE)
trainData <- cleaned_data[trainIndex, ]
testData <- cleaned_data[-trainIndex, ]

# Train a linear regression model
#I firstly fiited the model using all variavle except the date and GDP
#The p-value of ConsumerPrice and GFCF were not significant.
# I removed the two variables from the model and built the model using the remaing variables.
model_lm <- lm(GDP ~  UNEM + GovExp + HouseExp, data=trainData)
# Print model summary
#With this model all variables are signifant at 0.05 level of significant except for the Government expenditure variable.
summary(model_lm)

# Predicting on test data
pred_lm <- predict(model_lm, newdata=testData)
# Calculate performance metrics
rmse_lm <- sqrt(mean((testData$GDP - pred_lm)^2))
r2_lm <- cor(testData$GDP, pred_lm)^2
cat("Linear Regression RMSE:", rmse_lm, "\n")
#The value for R^2 is 0.9946, 99.46% of the variability observed in the target variable is explained by the regression model.
#The model predict GDP VERY well.
cat("Linear Regression R^2:", r2_lm, "\n")


#####Using random forest model so that I can compared with the regression model.
# Train a random forest model
model_rf <- randomForest(GDP ~ UNEM + GovExp + HouseExp, data=trainData)
# Print model summary
#Percentage of Variance Explained is 99.56%, this indicates the proportion of the variance in GDP that is explained by the model. A value close to 100% suggests that the model is fitting the data very well.
print(model_rf)

# Predicting on test data
pred_rf <- predict(model_rf, newdata=testData)
# Calculate performance metrics
rmse_rf <- sqrt(mean((testData$GDP - pred_rf)^2))
r2_rf <- cor(testData$GDP, pred_rf)^2
cat("Random Forest RMSE:", rmse_rf, "\n")
#The value for R^2 is 0.9980, 99,80% of the variability observed in the target variable is explained by the model.
#The model predict GDP VERY well.
cat("Random Forest R^2:", r2_rf, "\n")


#Both the regression model and the random forest model predict the GDP very using the variables unemployment rate, government expenditure and house expenditure.
