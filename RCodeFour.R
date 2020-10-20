library(tidyverse)
setwd("C:/Users/Sanaa Mironov/Documents/UMBC/CMSC491/CourseDataSets")

# Let's take a look at the data set
glimpse(cars)


# Scatter plot of Speed vs Stopping Distance
# Note that you can pass hexidecimal codes for your 
# favorite colors into geom functions

ggplot(data = cars, mapping = aes(x = dist, y = speed)) +
  geom_point(color = "#00a0e1") +
  ylim(c(0,30)) + # set limits for y-axis for better viewing
  labs(title = "Speed vs. Stopping Distance",
       x = "Stopping Distance",
       y = "Speed") +
  theme_light()

# Fit the linear regression model and assign it to "MyModel"
MyModel = lm(speed ~ dist, data = cars)


# Coefficient Estimates, t statistics, p-values, R-squared, 
# and residual standard error
summary(MyModel)

## ANOVA (Analysis of Variance) Table
anova(MyModel)

## Obtaining the Parameter Estimates
coef(MyModel)

## 95% Confidence Intervals for the Parameters
confint(MyModel, level = 0.95)

## 95% Confidence Interval for the Mean Value of Y Given X = x
predict(MyModel,
        newdata = data.frame(dist = 80), # x value entered as a data frame
        interval = "confidence")

# We can also obtain CIs for multiple values of dist
# Now dist is entered as a vector inside of a data frame
predict(MyModel,
        newdata = data.frame(dist = c(80, 82, 90)),
        interval = "confidence")

## 95% Confidence Interval for an Individual Value of Y Given X = x
predict(MyModel,
        newdata = data.frame(dist = 80), # x value entered as a data frame
        interval = "prediction")

# We can also obtain CIs for multiple values of dist
# Now dist is entered as a vector inside of a data frame
predict(MyModel,
        newdata = data.frame(dist = c(80, 82, 90)),
        interval = "prediction")

## Plotting the Results

# Plot the data with the linear regression line
# This includes 95% confidence bands for the mean of speed
ggplot(data = cars, mapping = aes(x = dist, y = speed)) +
  geom_point(color = "orange") + 
  geom_smooth(method = "lm") +
  theme_light()

# Let's see what's available in our lm object, "MyModel"
# The names() function returns the names 
names(MyModel)

# We can extract these terms with the $ operator
# The important terms for us will be the "residuals" and "fitted.values"
MyModel$coefficients


# We build a data frame with the original data, the residuals (errors) and
# predictions from our linear model
ModelResults = data.frame(speed = cars$speed,
                          dist = cars$dist,
                          residuals = MyModel$residuals,
                          predictions = MyModel$fitted.values)

# View the results
glimpse(ModelResults)


## Histogram of Residuals

ggplot(data = ModelResults, mapping = aes(x = residuals)) +
  geom_histogram(fill = "blue", color = "white", bins = 15) +
  labs(title = "Histogram of Residuals",
       x = "Residual Value",
       y = "Number of Observations") +
  theme_light()

## Scatter Plot of Residuals
ggplot(data = ModelResults, mapping = aes(x = predictions, y = residuals)) +
  geom_point(color = "blue") +
  geom_hline(yintercept = 0, color = "red") + # adds red horizontal line at y = 0
  theme_light()


