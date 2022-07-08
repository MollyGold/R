#############Ice Cream Sales Project#################

# Date - 29/04/2022

# Data used - Ice cream sales data from http://bit.ly/2OMHgFi


######################### Load data ############################
# Load the data and save as icecream_sales_data
icecream_sales_data <- read.csv("http://bit.ly/2OMHgFi")

######################## Libraries/packages ##################### 
# Library check
library(dplyr)
library(ggplot2)
library(tidyverse)

######################## Data Exploration ##################### 

# view the top ten columns of the dataset
head(icecream_sales_data, n = 10)

# view the tail of the dataset
tail(icecream_sales_data, n = 10)

# glimpse of the data
# same as above where head is used
dplyr::glimpse(icecream_sales_data)

# First exploration of variables. What kind of variables are in the data? 
# Were variables loaded in the right type (e.g., are factorial variables recorded as factors? numerical vars as num or int?)
# To see the structure of the data
str(icecream_sales_data)

# Confirm number of rows or observations
# Not necessary, just another way to check
nrow(icecream_sales_data)

# Numbers of variables on the dataset
length(icecream_sales_data)
# 3 qualitative variables: country, shopID, seasons (shopID is a nominal variable and not valuable for our exploration)
# 4 quantitative variables: icecream_sales, income, price, temperature
# No binary variable

#is there any NA in the dataset? #false
any(is.na(icecream_sales_data)) 

# How many entries for country A and B in dataset? 
table(icecream_sales_data$country)
table(icecream_sales_data$seasons)

# The type of values in the field 'seasons'
levels(icecream_sales_data$seasons)
# Not a factor as is. Need to stored as a factor to see the levels


# change to factor as its a categorical variable
icecream_sales_data$country = factor(icecream_sales_data$country)

# Ordering the levels as it occurs
# Overwrites the vector (seasons_var) created above
icecream_sales_data$seasons = factor(icecream_sales_data$seasons,levels=c("Spring", "Summer", "Autumn", "Winter"), ordered = TRUE)
# this was later reordered when fitting models

str(icecream_sales_data) # both categorical data now saved as factor

# Check for levels and the number of observations for each level.
table(icecream_sales_data$seasons)

# Select the numeric variables from the icecream dataset 
icecream_sales_num <- icecream_sales_data %>%
  select(icecream_sales, income, price, temperature)


# Summary statistics of the numerical variables
summary(icecream_sales_num)

# univariate exploration of the 'Icecream_sales' variable by country
# The boxplot further down is used in my report
icecream_sales_data %>% 
  group_by(country) %>%  # group by country 
  summarise(count = n(),
            mu = mean(icecream_sales), 
            pop_med = median(icecream_sales), 
            sigma = sd(icecream_sales), 
            pop_iqr = IQR(icecream_sales),
            pop_min = min(icecream_sales), 
            pop_max = max(icecream_sales),
            pop_q1 = quantile(icecream_sales, 0.25),  # first quartile, 25th percentile
            pop_q3 = quantile(icecream_sales, 0.75))  # third quartile, 75th percentile

# Summary statistics of the quantitative variable 'icecream_sales' grouped by seasons
# This is also done visually using a boxplot further down
icecream_sales_data %>% 
  group_by(seasons) %>%  # group by seasons
  summarise(count = n(),
            mu = mean(icecream_sales), 
            pop_med = median(icecream_sales), 
            sigma = sd(icecream_sales), 
            pop_iqr = IQR(icecream_sales),
            pop_min = min(icecream_sales), 
            pop_max = max(icecream_sales),
            pop_q1 = quantile(icecream_sales, 0.25),  # first quartile, 25th percentile
            pop_q3 = quantile(icecream_sales, 0.75))  # third quartile, 75th percentile

# Summary statistics of the quantitative variable 'income' grouped by country A and B
icecream_sales_data %>% 
  group_by(country) %>%  # group by country
  summarise(count = n(),
            mu = mean(income), 
            pop_med = median(income), 
            sigma = sd(income), 
            pop_iqr = IQR(income),
            pop_min = min(income), 
            pop_max = max(income),
            pop_q1 = quantile(income, 0.25),  # first quartile, 25th percentile
            pop_q3 = quantile(income, 0.75))  # third quartile, 75th percentile


# Summary statistics of the quantitative variable 'price' grouped by country A and B
icecream_sales_data %>% 
  group_by(country) %>%  # group by country
  summarise(count = n(),
            mu = mean(price), 
            pop_med = median(price), 
            sigma = sd(price), 
            pop_iqr = IQR(price),
            pop_min = min(price), 
            pop_max = max(price),
            pop_q1 = quantile(price, 0.25),  # first quartile, 25th percentile
            pop_q3 = quantile(price, 0.75))  # third quartile, 75th percentile

# Summary statistics of the quantitative variable 'temperature' grouped by country A and B
icecream_sales_data %>% 
  group_by(seasons) %>%  # group by seasons
  summarise(count = n(),
            mu = mean(temperature), 
            pop_med = median(temperature), 
            sigma = sd(temperature), 
            pop_iqr = IQR(temperature),
            pop_iqr = IQR(temperature),
            pop_iqr = IQR(temperature),
            pop_min = min(temperature), 
            pop_max = max(temperature),
            pop_q1 = quantile(temperature, 0.25),  # first quartile, 25th percentile
            pop_q3 = quantile(temperature, 0.75))  # third quartile, 75th percentile

# What is the shape of the distribution? ~ come back to this 
# data exploration using ggplot
# visualising the data
# The general code for ggplot2 looks like this: ggplot(data=, aes(x=, y=), colour=, size=,) + geom_xxxx() + geom_yyyy(). to delete
# Scatter plot
# Boxplot
# histogram
# Bar chart

# Number of observations in the two countries
# Not in report as already included the figures 
ggplot(data = icecream_sales_data) +
  geom_bar(mapping = aes(x = country), stat = "count", fill = "Wheat", width = .3) +
  xlab("Countries") + ggtitle("Observations by Countries")

# Copy my plot above to a PNG file
dev.copy(png, file = "Observationspercountry.png")

# close the PNG device
dev.off()
#stopped here

#View the distribution of the variable price
ggplot(data = icecream_sales_data, aes(x = price)) +
  geom_histogram() +
  xlab("Price") + ggtitle("Distribution of Price")


#Store plot as png
dev.copy(png, file = "Pricedistribution.png")

# close the PNG device
dev.off()

#View the distribution of the variable Income
ggplot(data = icecream_sales_data, aes(x = income)) +
  geom_histogram() +
  xlab("Income") + ggtitle("Distribution of Income")
#Store plot as png
dev.copy(png, file = "Incomedistribution.png")

#close the PNG device
dev.off()

#View the distribution of the variable temperature
ggplot(data = icecream_sales_data, aes(x = temperature)) +
  geom_histogram() +
  xlab("Temperature (Celcius)") + ggtitle("Distribution of temperature")
#Store plot as png
dev.copy(png, file = "tempdistribution.png")

# close the PNG device
dev.off()

#View the distribution of the variable icecream_sales
ggplot(data = icecream_sales_data, aes(x = icecream_sales)) +
  geom_histogram() +
  xlab("Ice cream sales") + ggtitle("Observations of ice cream sales")
#Store plot as png
dev.copy(png, file = "Icecreamsales_distribution.png")

# close the PNG device
dev.off()

# bivariate exploration
# Scatter plot of icecream_sales with temperature
ggplot(data = icecream_sales_data, aes(x = temperature, y = icecream_sales, colour = seasons)) + #use the + sign to add a layer
  geom_point() + #layer for the visual representation of data using points. A scatterplot...this is good
  facet_wrap( ~ country) +
  xlab("Temperature in Celcius") +
  ylab("Icecream sales in (£)") + 
  ggtitle("Distribution Icecream sales with Temperature")

# Copy my plot above to a PNG file
dev.copy(png, file = "Salesvspertemp.png")

# close the PNG device
dev.off()


# Checking the relationship between the two variable: icecream_sales and Temperature
ggplot(data = icecream_sales_data, aes(x = temperature, y = icecream_sales)) + #use the + sign to add a layer
  geom_point() + #layer for the visual representation of data using points. A scatterplot...this is good
  facet_wrap( ~ country) +
  xlab("Temperature in Celcius") +
  ylab("Icecream sales in (£)") + 
  ggtitle("Relationship between Icecream sales and Temperature") +
  geom_smooth(method = "lm", size = 1.5)

# Copy my plot above to a PNG file
dev.copy(png, file = "Salesvstemprsh.png")

# close the PNG device
dev.off()


ggplot(data = icecream_sales_data, aes(x = country, y = icecream_sales)) +
  geom_boxplot()+ # creates box plots
  xlab("Countries") +
  ylab("Ice cream sales in (£)") +
  ggtitle("Box plots of Ice cream sales in the two countries")

# Copy my plot to a PNG file
dev.copy(png, file = "countrysalesplot.png")

# close the PNG device
dev.off()

# View the outliers in Country A and B
filter(icecream_sales_data, icecream_sales> 1250, country == "A")
filter(icecream_sales_data, icecream_sales> 1500, country == "B")

# Boxplot to see the distribution of Ice cream sales in each season
ggplot(data = icecream_sales_data, aes(x = seasons, y = icecream_sales)) +
  geom_boxplot()+ # creates box plots
  xlab("Seasons") +
  ylab("Ice cream sales in (£)") +
  ggtitle("Box plots of Ice cream sales in all seasons")

# Copy my plot above to a PNG file
dev.copy(png, file = "salesperseasons.png")

# close the PNG device
dev.off()

# Distribution of each categorical variable
#geom_bar creates the bar chart
ggplot(data = icecream_sales_data) +
  geom_bar(mapping = aes(x = seasons), stat = "count", fill = "Orange", width = .8) +
  xlab("Seasons") + ggtitle("Observations by seasons")

# Copy my plot above to a PNG file
dev.copy(png, file = "Observationsperseason.png")

# close the PNG device
dev.off()


#Exploration of the variable: Income
# Boxplot of Income by Country
#report outliers
ggplot(data = (icecream_sales_data), aes(x = country, y = income, colour = country))  +   
  geom_boxplot() + 
  xlab("Country") +   
  ylab("Income") +        
  ggtitle("Country vs Income")
# Copy my plot above to a PNG file
dev.copy(png, file = "income boxplot.png")

# close the PNG device
dev.off()

# View the outliers in Country A and B
filter(icecream_sales_data, income> 47000, country == "A")
filter(icecream_sales_data, income> 47000, country == "B")

ggplot(data = icecream_sales_data, aes(x = income, y = icecream_sales, by=country)) + 
  geom_point() + 
  facet_wrap( ~ country) +
  xlab("Income in pounds") +
  ylab("Icecream sales in (£)") + 
  ggtitle("Icecream sales vs Income") +
  geom_smooth(method = "lm", size = 1.5)

# Copy my plot above to a PNG file
dev.copy(png, file = "Income vs sales.png")

# close the PNG device
dev.off()

ggplot(data = icecream_sales_data, aes(x = income, y = icecream_sales)) + 
  geom_point() + 
  xlab("Income in pounds") +
  ylab("Icecream sales in (£)") + 
  ggtitle("Icecream sales vs Income") +
  geom_smooth(method = "lm", size = 1.5)


#Exploration of Numeric variables (Price) using ggplot
# Apply facets to create a separate plot for each country.
ggplot(data = icecream_sales_data, aes(x = price, y = icecream_sales, by = country)) + 
  geom_point() + 
  facet_wrap( ~ country) + 
  xlab("Price in pounds") +
  ylab("Icecream sales in (£)") + 
  ggtitle("Icecream sales vs price") +
  geom_smooth(method = "lm", size = 1.5) #` using formula 'y ~ x'

# Copy my plot above to a PNG file
dev.copy(png, file = "IcecreamsalesvsPrice.png")

# close the PNG device
dev.off()

     
-------------------------------------------------------------------------------------
# Question 2
# Hypothesis testing of the average mean of country A compared to average mean of country B
# Null Hypothesis: mu income in country A = mu income in country B
     
# Confidence Interval 
statsr::inference(y = income, x = country, data = icecream_sales_data, 
                       statistic = c("mean"), 
                       type = c("ci"), 
                       null = 0,
                       alternative = c("twosided"), 
                       method = c("theoretical"), 
                       conf_level = 0.95,
                       order = c("A","B"))

# Hypothesis test
statsr::inference(y = income, x = country, data = icecream_sales_data, 
                  statistic = c("mean"), 
                  type = c("ht"), 
                  null = 0,
                  alternative = c("twosided"), 
                  method = c("theoretical"), 
                  conf_level = 0.95,
                  order = c("A","B"))


------------------------------------------------------------------------------
#Question 3:
# Select the identified variables from the original large dataset
# Explanatory variables:
# Outcome variables: icecream_sales
  

  sales_explanatory <- icecream_sales_data  %>%
select(icecream_sales, price, temperature, income, seasons, country)

# Summarise the dataset
summary(sales_explanatory)

# Compute the correlation coefficients between the outcome variable and the numerical explanatory variables
icecream_sales_data %>%
  select(icecream_sales, price, income, temperature) %>%
  cor()

# View relationship between the outcome variable 'icecream_sales' with 'price', 'temperature' and income
Asso1 <- ggplot(icecream_sales_data, aes(x = price, y = icecream_sales)) +
  geom_point() +
  labs(x = "Price", y = "Icecream Sales (in £)", title = "Relationship between price and ice cream sales") +
  geom_smooth(method = "lm", se = FALSE)

# viewing the plot
Asso1

# Copy my plot above to a PNG file
dev.copy(png, file = "RshpIcecreamsalesvsPrice.png")

# close the PNG device
dev.off()

# View the relationship between icecream sales, price and country
Asso1a <- ggplot(icecream_sales_data, aes(x = price, y = icecream_sales, col = country)) +
  geom_point() +
  labs(x = "Price", y = "Icecream Sales (in £)", colour = "country", title = "Relationship between price and ice cream sales") +
  geom_smooth(method = "lm", se = FALSE)

# viewing the plot
Asso1a

# Copy my plot above to a PNG file
dev.copy(png, file = "RshpIcecreamsalesvsPricevscountry.png")

# close the PNG device
dev.off()

# View the relationship between icecream sales, price and country
Asso1b <- ggplot(icecream_sales_data, aes(x = price, y = icecream_sales, col = seasons)) +
  geom_point() +
  labs(x = "Price", y = "Icecream Sales (in £)", colour = "seasons", title = "Relationship between price and ice cream sales") +
  geom_smooth(method = "lm", se = FALSE)

# viewing the plot
Asso1b

# Copy my plot above to a PNG file
dev.copy(png, file = "RshpIcecreamsalesvsPricevsseasons.png")

# close the PNG device
dev.off()

# View the relationship between icecream sales, temperature 
Asso2 <- ggplot(icecream_sales_data, aes(x = temperature, y = icecream_sales)) +
  geom_point() +
  labs(x = "temperature (celcius)", y = "icecream_sales (in £)", title = "Relationship between temperature and ice cream sales") +
  geom_smooth(method = "lm", se = FALSE)

# View the scatter plot
Asso2

# Copy my plot above to a PNG file
dev.copy(png, file = "RshpIcecreamsalesvstemp.png")

# close the PNG device
dev.off()

# View the relationship between icecream sales, temperature and country
Asso2b <- ggplot(icecream_sales_data, aes(x = temperature, y = icecream_sales, col = country)) +
  geom_point() +
  labs(x = "temperature (celcius)", y = "icecream_sales (in £)", colour = "country", title = "Relationship between temperature and ice cream sales per seasons") +
  geom_smooth(method = "lm", se = FALSE)
Asso2b

# Copy my plot above to a PNG file
dev.copy(png, file = "RshpIcecreamsalesvstempvscountry.png")

# close the PNG device
dev.off()

# View the relationship between icecream sales, temperature and seasons
Asso2c <- ggplot(icecream_sales_data, aes(x = temperature, y = icecream_sales, col = seasons)) +
  geom_point() +
  labs(x = "temperature (celcius)", y = "icecream_sales (in £)", colour = "seasons", title = "Relationship between temperature and ice cream sales per seasons") +
  geom_smooth(method = "lm", se = FALSE)
Asso2c

# Copy my plot above to a PNG file
dev.copy(png, file = "RshpIcecreamsalesvstempvsseasons.png")

# close the PNG device
dev.off()


# Relationship between income and icecream_sales
Asso3 <- ggplot(icecream_sales_data, aes(x = income, y = icecream_sales)) +
  geom_point() +
  labs(x = "income", y = "icecream_sales (in £)", title = "Relationship between Income and Icecream_sales") +
  geom_smooth(method = "lm", se = FALSE)

# viewing the plot
Asso3

# Copy my plot above to a PNG file
dev.copy(png, file = "RshpIcecreamsalesvincome.png")

# close the PNG device
dev.off()

# View the relationship between icecream sales, income and country
Asso3a <- ggplot(icecream_sales_data, aes(x = income, y = icecream_sales, col = country)) +
  geom_point() +
  labs(x = "income (in £)", y = "icecream_sales (in £)", colour = "country", title = "Relationship between Income and Icecream_sales") +
  geom_smooth(method = "lm", se = FALSE)

# viewing the plot
Asso3a

# Copy my plot above to a PNG file
dev.copy(png, file = "RshpIcecreamsalesvincomevscountry.png")

# close the PNG device
dev.off()

# View the relationship between icecream sales, income and seasons
Asso3b <- ggplot(icecream_sales_data, aes(x = income, y = icecream_sales, col = seasons)) +
  geom_point() +
  labs(x = "income (in £)", y = "icecream_sales (in £)", colour = "seasons", title = "Relationship between Income and Icecream_sales") +
  geom_smooth(method = "lm", se = FALSE)

# viewing the plot
Asso3b

# Copy my plot above to a PNG file
dev.copy(png, file = "RshpIcecreamsalesvincomevsseasons.png")

# close the PNG device
dev.off()


######################### Regression - Fitting Models ############################
# Regression equation
# Dependent variable (y) = icecream_sales
# Explanatory variable (x) = price, temperature, income, seasons, country

# reordering the variable seasons
icecream_sales_data$seasons = factor(icecream_sales_data$seasons,levels=c("Spring", "Summer", "Autumn", "Winter"), ordered = FALSE)

# Select the modelling variables from the dataset
# Boxplot shows there were outliers within the income variable
# Remove the Outliers
Icecream_sales_new <- icecream_sales_data[icecream_sales_data$income > quantile(icecream_sales_data$income, .25) - 1.5*IQR(icecream_sales_data$income) & 
          icecream_sales_data$income < quantile(icecream_sales_data$income, .75) + 1.5*IQR(icecream_sales_data$income), ] # nrows

# check the selected observations
summary(Icecream_sales_new)
# 975 observations


sales_brief <- Icecream_sales_new %>%
  select(icecream_sales, price, temperature, income, seasons, country)


# Summarise the dataset
summary(sales_brief) # output should go into my report


# C. Fit Model
Sales_model <- lm(icecream_sales ~ price + temperature + income + seasons + country, data = sales_brief)
summary(Sales_model)
# 975 observations, 6 rows (Outliers removed)


# Change the ref level for country A
sales_brief$country <- relevel(sales_brief$country, ref = "B")
Sales_model <- lm(icecream_sales ~ price + temperature + income + seasons + country, data = sales_brief)
summary(Sales_model)

# Change the ref level for spring
sales_brief$seasons <- relevel(sales_brief$seasons, ref = "Summer")
Sales_model <- lm(icecream_sales ~ price + temperature + income + seasons + country, data = sales_brief)
summary(Sales_model)

# 4.3 prediction for country A at income of 13000
predict_dataA <- data.frame(income = 13000, country = "A")
Sales_model <- lm(icecream_sales ~ income + country, data = sales_brief) # new variable without outliers
predict(Sales_model, predict_dataA)
#353.2383

# prediction for country B at income of 20000
predic_dataB <- data.frame(income = 20000, country = "B")
Sales_model <- lm(icecream_sales ~ income + country, data = sales_brief) # new variable without outliers
predict(Sales_model, predic_dataB)
#579.5633

# Difference in the predictions above
# 226.325

# 4C. Predicted change on price and temperature increase

newpred <- data.frame(price = -144.90 * 0.75, temperature = 7.7430 * 0.5)
Sales_model <- lm(icecream_sales ~ price + temperature, data = sales_brief) # new variable without outliers
predict(Sales_model, newpred)
#20977.79

# 4.5 R squared of the model 
# Sum of squares contributed by explained variability / sum of total variability
# we can conclude that this coefficients are significantly different from zero
anova(Sales_model) #come back to it


# 4.6 Statistical significance at 1%
# t-critical
qt(0.005, df = 967)
# = -2.580923 
# t-calculated > t-critical so we can reject the null hypothesis
#4.7 
install.packages("MASS")
confint(Sales_model, level = 0.99)
#              0.5 %     99.5 %
# (Intercept)  591.26759  989.37099
# price       -248.23519 -122.07211
# temperature   14.10899   19.93277

######################### Testing Conditions ############################

# 1st condition - linearity
# showing linearity between the numerical/continuous variable - price, temperature, income
Sales_model <- lm(icecream_sales ~ price + temperature + income + seasons + country, data = sales_brief) # calling back our model
x = Sales_model$residuals
y= sales_brief$temperature

plot(Sales_model$residuals ~ sales_brief$temperature)
abline(h = 0) # add horizontal line at y = 0

# Copy my plot above to a PNG file
dev.copy(png, file = "cond1.png")

# close the PNG device
dev.off()

x = Sales_model$residuals
y= sales_brief$price

plot(Sales_model$residuals ~ sales_brief$price)
abline(h = 0) # add horizontal line at y = 0

# Copy my plot above to a PNG file
dev.copy(png, file = "cond2.png")

# close the PNG device
dev.off()


x = Sales_model$residuals
y= sales_brief$income

plot(Sales_model$residuals ~ sales_brief$income)
abline(h = 0) # add horizontal line at y = 0

# Copy my plot above to a PNG file
dev.copy(png, file = "cond3.png")

# close the PNG device
dev.off()

# 2nd Condition
# Near Normal distribution around mean 0
hist(Sales_model$residuals)
dev.copy(png, file = "normal.png")

# close the PNG device
dev.off()
qqnorm(Sales_model$residuals)
qqline(Sales_model$residuals)

# No huge deviation from mean so we can say this condition seem to be fairly satisfied

# Copy my plot above to a PNG file
dev.copy(png, file = "normality.png")

# close the PNG device
dev.off()

# 3rd Condition
# Constant variability of residuals - same variability for lower and higher values of the predicted outcome variable
# the predicted outcome of y versus predicted outcome of sales_brief
# residuals scattered around the bend with a constant width around zero, but not fan shape.
plot(Sales_model$residuals ~ Sales_model$fitted)
# Residuals contained in a constant distance from the mean so the constant variabilty appears to be met

# Copy my plot above to a PNG file
dev.copy(png, file = "variability.png")

# close the PNG device
dev.off()

# 4th Condition ; Independent residuals
# observations are independent from each other rather than having a time series structure
# look at plot of residuals to ascertain whether anything strange is happening - pattern
plot(Sales_model$residuals)
# No increasing or decreasing pattern, so it appears the residuals are independent of each other
# Copy my plot above to a PNG file
dev.copy(png, file = "independent.png")

# close the PNG device
dev.off()
######################### Prediction ############################
predvalue <- data.frame(price = 2, temperature = 1, income = 20000, country = "B", seasons = "Winter")
Sales_model <- lm(icecream_sales ~ price + temperature + income + country + seasons, data = sales_brief)
predict(Sales_model, predvalue, interval = "prediction", level = 0.90)
#       fit       lwr      upr
#1 313.4694 -131.2843 758.2231
