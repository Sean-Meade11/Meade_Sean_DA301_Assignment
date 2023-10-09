## LSE Data Analytics Online Career Accelerator 

# DA301:  Advanced Analytics for Organisational Impact

###############################################################################

# Assignment template

## Scenario
## You are a data analyst working for Turtle Games, a game manufacturer and 
## retailer. They manufacture and sell their own products, along with sourcing
## and selling products manufactured by other companies. Their product range 
## includes books, board games, video games and toys. They have a global 
## customer base and have a business objective of improving overall sales 
##performance by utilising customer trends. 

## In particular, Turtle Games wants to understand:
## - how customers accumulate loyalty points (Week 1)
## - how useful are remuneration and spending scores data (Week 2)
## - can social data (e.g. customer reviews) be used in marketing 
##     campaigns (Week 3)
## - what is the impact on sales per product (Week 4)
## - the reliability of the data (e.g. normal distribution, Skewness, Kurtosis)
##     (Week 5)
## - if there is any possible relationship(s) in sales between North America,
##     Europe, and global sales (Week 6).

################################################################################

# Week 4 assignment: EDA using R

## The sales department of Turtle games prefers R to Python. As you can perform
## data analysis in R, you will explore and prepare the data set for analysis by
## utilising basic statistics and plots. Note that you will use this data set 
## in future modules as well and it is, therefore, strongly encouraged to first
## clean the data as per provided guidelines and then save a copy of the clean 
## data for future use.

# Instructions
# 1. Load and explore the data.
##  - Remove redundant columns (Ranking, Year, Genre, Publisher) by creating 
##      a subset of the data frame.
##  - Create a summary of the new data frame.
# 2. Create plots to review and determine insights into data set.
##  - Create scatterplots, histograms and boxplots to gain insights into
##      the Sales data.
##  - Note your observations and diagrams that could be used to provide
##      insights to the business.
# 3. Include your insights and observations.

###############################################################################

# 1. Load and explore the data
getwd()
setwd('C:/Users/smead.DESKTOP-GGSL49B/Desktop/Assignment 3 - Predicting future outcomes')
getwd()
# Install and import Tidyverse.

install.packages('tidyverse')
library(tidyverse)
# Import the data set.
df <- read.csv(file.choose(), header=TRUE)
df

# Print the data frame.
View(df)

# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns. 
df_s = subset(df, select = -c(Ranking, Year, Genre, Publisher))
df_s
# View the data frame.
View(df_s)

# View the descriptive statistics.

str(df_s)


# Check the type of the data frame.
typeof(df_s)


# Check the class of the data frame.
class(df_s)


# Check the dimensions of the data frame
dim(df_s)
summary(df_s)


################################################################################

# 2. Review plots to determine insights into the data set.

## 2a) Scatterplots
# Create scatterplots.
qplot(Platform,Global_Sales, data=df_s)
qplot(Platform,NA_Sales, data=df_s)
qplot(Platform,EU_Sales, data=df_s)
qplot(NA_Sales,Global_Sales, data=df_s)
qplot(EU_Sales,Global_Sales, data=df_s)
qplot(NA_Sales,EU_Sales, data=df_s)
qplot(Product,Global_Sales, data=df_s)

qplot(y=Global_Sales, data=df_s)
qplot(y=NA_Sales, data=df_s)
qplot(y=EU_Sales, data=df_s)

## 2b) Histograms
# Create histograms.

qplot(Global_Sales, bins=5, data=df_s)
qplot(Global_Sales, data=df_s)
qplot(EU_Sales, data=df_s)
qplot(NA_Sales, data=df_s)
## 2c) Boxplots
# Create boxplots.
qplot(Global_Sales, Platform, data=df_s, geom='boxplot')
qplot(NA_Sales, Platform, data=df_s, geom='boxplot')
qplot(EU_Sales, Platform, data=df_s, geom='boxplot')
###############################################################################

# 3. Observations and insights

## Your observations and insights here ......




###############################################################################
###############################################################################


# Week 5 assignment: Cleaning and maniulating data using R

## Utilising R, you will explore, prepare and explain the normality of the data
## set based on plots, Skewness, Kurtosis, and a Shapiro-Wilk test. Note that
## you will use this data set in future modules as well and it is, therefore, 
## strongly encouraged to first clean the data as per provided guidelines and 
## then save a copy of the clean data for future use.

## Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 4 assignment. 
##  - View the data frame to sense-check the data set.
##  - Determine the `min`, `max` and `mean` values of all the sales data.
##  - Create a summary of the data frame.
# 2. Determine the impact on sales per product_id.
##  - Use the group_by and aggregate functions to sum the values grouped by
##      product.
##  - Create a summary of the new data frame.
# 3. Create plots to review and determine insights into the data set.
##  - Create scatterplots, histograms, and boxplots to gain insights into 
##     the Sales data.
##  - Note your observations and diagrams that could be used to provide 
##     insights to the business.
# 4. Determine the normality of the data set.
##  - Create and explore Q-Q plots for all sales data.
##  - Perform a Shapiro-Wilk test on all the sales data.
##  - Determine the Skewness and Kurtosis of all the sales data.
##  - Determine if there is any correlation between the sales data columns.
# 5. Create plots to gain insights into the sales data.
##  - Compare all the sales data (columns) for any correlation(s).
##  - Add a trend line to the plots for ease of interpretation.
# 6. Include your insights and observations.

################################################################################

# 1. Load and explore the data

# View data frame created in Week 4.

  View(df_s)
# Check output: Determine the min, max, and mean values.
summary(df_s)
apply(df_s, 2, min)
apply(df_s, 2, max)
apply(df_s[, -1, -2], 2, mean) --????
# View the descriptive statistics.
str(df_s)


# Check the type of the data frame.
typeof(df_s)


# Check the class of the data frame.
class(df_s)


# Check the dimensions of the data frame
dim(df_s)

###############################################################################

# 2. Determine the impact on sales per product_id.

## 2a) Use the group_by and aggregate functions.

Total_S = aggregate(NA_Sales+EU_Sales+Global_Sales~Product, df_s, sum)
View(Total_S)
# Group data based on Product and determine the sum per Product.
library(dplyr)
df_s_p <- df_s %>% group_by(Product) %>% summarise(sum_NA=sum(NA_Sales),sum_EU=sum(EU_Sales),
                                sum_global=sum(Global_Sales))
df_s_p                          

df_s_p_p <- df_s %>% group_by(Product, Platform) %>% summarise(sum_NA=sum(NA_Sales),sum_EU=sum(EU_Sales),
                                                   sum_global=sum(Global_Sales))
# View the data frame.

View(df_s_p)
View(df_s_p_p)
# Explore the data frame.
summary(df_s_p)
dim(df_s_p)


## 2b) Determine which plot is the best to compare game sales.
# Create scatterplots.
qplot(Product,sum_EU, data=df_s_p)
qplot(Product,sum_NA, data=df_s_p)`
qplot(Product,sum_global, data=df_s_p)


qplot(y=sum_global, data=df_s_p)
qplot(y=sum_NA, data=df_s_p)
qplot(y=sum_EU, data=df_s_p)

# Create histograms.
qplot(sum_global, bins=5, data=df_s_p)
qplot(sum_global, data=df_s_p)
qplot(sum_EU, data=df_s_p)
qplot(sum_NA, data=df_s_p)

# Create boxplots.
qplot(sum_global, Product, data=df_s_p, geom='boxplot')
qplot(sum_EU, Product, data=df_s_p, geom='boxplot')
qplot(sum_NA, Product, data=df_s_p, geom='boxplot')
qplot(sum_global, Platform, data=df_s_p_p, geom='boxplot')
qplot(sum_NA, Platform, data=df_s_p_p, geom='boxplot')
qplot(sum_EU, Platform, data=df_s_p_p, geom='boxplot')

###############################################################################


# 3. Determine the normality of the data set.
install.packages('moments')
library (moments)
## 3a) Create Q-Q Plots
# Create Q-Q Plots.
# Specify the qqnorm function.
# Draw a qqplot using the sum_global data.
qqnorm(df_s_p$sum_global,
       col='blue',
       xlab="z Value",
       ylab='sales global')

# Specify the qqline function.
# Add a reference line to the qqplot.
qqline(df_s_p$sum_global,
       col='red',
       lwd=2) 


## 3b) Perform Shapiro-Wilk test
# Install and import Moments.


# Perform Shapiro-Wilk test.

# Run a Shapiro-Wilk test.
shapiro.test(df_s_p$sum_global)

## 3c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.






qqnorm(df_s_p_p$sum_EU,
       col='blue',
       xlab="z Value",
       ylab='sales EU')

qqnorm(df_s_p_p$sum_NA,
       col='blue',
       xlab="z Value",
       ylab='sales NA')


# Specify the skewness and kurtosis functions.
skewness(df_s_p$sum_global)
kurtosis(df_s_p$sum_global)

## 3a) Create Q-Q Plots
# Create Q-Q Plots.
# Specify the qqnorm function.
# Draw a qqplot using the sum_EU data.
qqnorm(df_s_p$sum_EU,
       col='blue',
       xlab="z Value",
       ylab='sales EU')
# Specify the qqline function.
# Add a reference line to the qqplot.
qqline(df_s_p$sum_EU,
       col='red',
       lwd=2) 


## 3b) Perform Shapiro-Wilk test
# Install and import Moments.


# Perform Shapiro-Wilk test.

# Run a Shapiro-Wilk test.
shapiro.test(df_s_p$sum_EU)

## 3c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.

# Specify the skewness and kurtosis functions.
skewness(df_s_p$sum_EU)
kurtosis(df_s_p$sum_EU)

## 3a) Create Q-Q Plots
# Create Q-Q Plots.
# Specify the qqnorm function.
# Draw a qqplot using the sum_NA data.

qqnorm(df_s_p$sum_NA,
       col='blue',
       xlab="z Value",
       ylab='sales NA')
# Specify the qqline function.
# Add a reference line to the qqplot.
qqline(df_s_p$sum_NA,
       col='red',
       lwd=2) 


## 3b) Perform Shapiro-Wilk test
# Install and import Moments.


# Perform Shapiro-Wilk test.

# Run a Shapiro-Wilk test.
shapiro.test(df_s_p$sum_NA)

## 3c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.

# Specify the skewness and kurtosis functions.
skewness(df_s_p$sum_NA)
kurtosis(df_s_p$sum_NA)



# Run a Shapiro-Wilk test.
shapiro.test(df_s_p$sum_NA)
shapiro.test(df_s_p$sum_EU)
shapiro.test(df_s_p$sum_global)

## 3d) Determine correlation
# Determine correlation.

# Specify the cor function.
# Set the first and second variables.

cor(df_s_p$sum_global, df_s_p$sum_EU)
cor(df_s_p$sum_global, df_s_p$sum_NA) 

# Determine the correlation for the whole data frame.
round (cor(df_s_p),
       digits=2)






###############################################################################

# 4. Plot the data
# Create plots to gain insights into data.
ggplot(df_s,
       mapping=aes(x=Product, y=Sales_EU)) +
  geom_point()


# Choose the type of plot you think best suits the data set and what you want 
# to investigate. Explain your answer in your report.


###############################################################################

# 5. Observations and insights
# Your observations and insights here...



###############################################################################
###############################################################################

# Week 6 assignment: Making recommendations to the business using R

## The sales department wants to better understand if there is any relationship
## between North America, Europe, and global sales. Therefore, you need to
## investigate any possible relationship(s) in the sales data by creating a 
## simple and multiple linear regression model. Based on the models and your
## previous analysis (Weeks 1-5), you will then provide recommendations to 
## Turtle Games based on:
##   - Do you have confidence in the models based on goodness of fit and
##        accuracy of predictions?
##   - What would your suggestions and recommendations be to the business?
##   - If needed, how would you improve the model(s)?
##   - Explain your answers.

# Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 5 assignment. 
# 2. Create a simple linear regression model.
##  - Determine the correlation between the sales columns.
##  - View the output.
##  - Create plots to view the linear regression.
# 3. Create a multiple linear regression model
##  - Select only the numeric columns.
##  - Determine the correlation between the sales columns.
##  - View the output.
# 4. Predict global sales based on provided values. Compare your prediction to
#      the observed value(s).
##  - NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
##  - NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
##  - NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
##  - NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
##  - NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.
# 5. Include your insights and observations.

###############################################################################

# 1. Load and explor the data
# View data frame created in Week 5.


# Determine a summary of the data frame.


###############################################################################

# 2. Create a simple linear regression model
## 2a) Determine the correlation between columns
# Create a linear regression model on the original data.



## 2b) Create a plot (simple linear regression)
# Basic visualisation.


###############################################################################

# 3. Create a multiple linear regression model
# Select only numeric columns from the original data frame.


# Multiple linear regression model.


###############################################################################

# 4. Predictions based on given values
# Compare with observed values for a number of records.


###############################################################################

# 5. Observations and insights
# Your observations and insights here...



###############################################################################
###############################################################################




