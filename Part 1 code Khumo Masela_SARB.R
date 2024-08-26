library(ggplot2)
install.packages("corrplot")
library(corrplot)
library(tidyr)
library(dplyr)
library(forecast)
file_path <- "C:/Users/User/OneDrive/Documents/SARB/data.csv"
# Import the CSV file
data <- read.csv(file_path)
head(data)

#######PART 1##########
#The dataset consists of 121 observations and 7 variables, each representing an economic indicator, six variables are numeric and only the date variable is a character. 
str(data)
dim(data)
# From the summary we observe that all six varuables have one missing values.
summary(data)
#There are no duplicated values
duplicate_rows <- data[duplicated(data), ]
#There are 6 missing values in the dataset
missing_values <- sum(is.na(data))
missing_values
# Each column has one missing value, except for the date variable. I also checked this using summary()
colSums(is.na(data))
####I have removed all rows contaning missing variables,115 observation remain
cleaning_data <- na.omit(data)

# Fill missing numeric values with mean 
#Repacing missing values with the mean of column might not work since the values we get are to big and are not with the range of the column values. I will there choose the method that removes the missing values.
#data$GDP <- ifelse(is.na(data$GDP), 
#                           mean(data$GDP, na.rm = TRUE), 
#                          data$GDP)

#data$GFCF <- ifelse(is.na(data$GFCF), 
#                   mean(data$GFCF, na.rm = TRUE), 
#                   data$GFCF)
#data$GFCF

#data$UNEM <- ifelse(is.na(data$UNEM), 
#                    mean(data$UNEM, na.rm = TRUE), 
#                    data$UNEM)

# Correlation matrix
cor_matrix <- cor(cleaning_data[, sapply(cleaning_data, is.numeric)], use = "complete.obs")

# Visualize the correlation matrix
#The correlation matrix you shows very low correlations between the variables, with values close to 0.
corrplot(cor_matrix, method = "circle")
print(cor_matrix)

# Checking outliers using the Z-score method
z_scores <- scale(cleaning_data$GDP)
outliers <- which(abs(z_scores) > 3)
outliers

z_scores <- scale(cleaning_data$GFCF)
outliers <- which(abs(z_scores) > 3)
outliers

z_scores <- scale(cleaning_data$UNEM)
outliers <- which(abs(z_scores) > 3)
outliers

z_scores <- scale(cleaning_data$ConsumerPrices)
outliers <- which(abs(z_scores) > 3)
outliers

z_scores <- scale(cleaning_data$GovExp)
outliers <- which(abs(z_scores) > 3)
outliers

z_scores <- scale(cleaning_data$HouseExp)
outliers <- which(abs(z_scores) > 3)
outliers

# Removing outliers from each column
cleaned_data <- cleaning_data[-c(47, 65, 101, 81, 112, 85), ]
# Gives summary of the cleaned data, now we see that there are no longer missing values.
summary(cleaned_data)

# Convert 'Date' to Date type
cleaned_data$Date <- as.Date(cleaned_data$Date)
# From the graph we see that there is a decrease in GDP in 2020, this could be due to covid-19 pandemic, this is due to the economic shutdown, decline in business activities.
ggplot(cleaned_data, aes(x = Date, y = GDP)) +
  geom_line(color = 'blue') +
  labs(title = 'GDP Over Time', x = 'Date', y = 'GDP') +
  theme_minimal()

ggplot(cleaned_data, aes(x = Date, y = ConsumerPrices)) +
  geom_line(color = 'green') +
  labs(title = 'Consumer Prices Over Time', x = 'Date', y = 'Consumer Prices') +
  theme_minimal()

# Government Expenditure
#The government expenditure has been increasing over the years.
ggplot(cleaned_data, aes(x = Date, y = GovExp)) +
  geom_line(color = 'purple') +
  labs(title = 'Government Expenditure Over Time', x = 'Date', y = 'Government Expenditure') +
  theme_minimal()

#Unemployment rate
# The unemployment rate increased significantly after covid-19 pandemic, this because there were people who lost their jobs since some business could not survive the pandemic.
ggplot(cleaned_data, aes(x = Date, y = UNEM)) +
  geom_line(color = 'red') +
  labs(title = 'Unemployment rate Over Time', x = 'Date', y = 'Unemployment rate') +
  theme_minimal()

