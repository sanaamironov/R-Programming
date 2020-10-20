library(tidyverse)
library(readxl)

##setwd("C:/Users/Sanaa Mironov/Documents/UMBC/CMSC491/CourseDataSets")
############################## Categorical Predictor Example ####################################
# Import iris dataset
data(iris)

# Veiw the data
View(iris)

# Let's see the distribution of Sepal.Length by Species of plant
ggplot(data = iris, mapping = aes(x = Sepal.Length)) +
  geom_histogram(aes(fill = Species), color = "white", bins = 15) +
  facet_wrap(~Species, nrow = 3) +
  labs(title = "Distribution of Sepal Length by Species",
       x = "Sepal Length",
       y = "Number of Plants") +
  theme_light()

# Let's see the relationship between Sepal.Length and Sepal.Width
ggplot(data = iris, mapping = aes(y = Sepal.Length, x = Sepal.Width)) +
  geom_point(aes(color = Species), size = 2) +
  labs(title = "Sepal Length by Sepal Width Colored by Species of Plant",
       x = "Sepal Width",
       y = "Sepal Length") +
  theme_light()

# Add linear regression lines by Species, by default this includes interaction terms
# since the slopes can be different for each category
# However in our example, the assumption of equal slopes seems reasonable
ggplot(data = iris, mapping = aes(y = Sepal.Length, x = Sepal.Width)) +
  geom_point(aes(color = Species), size = 2) +
  geom_smooth(aes(color = Species), method = "lm", se = FALSE) +
  labs(title = "Sepal Length by Sepal Width Colored by Species of Plant",
       x = "Sepal Width",
       y = "Sepal Length") +
  theme_light()
#We are interested in the realtiosnip between sepal length and width
# Now we fit a multiple linear regression that predicts Sepal.Length
# by Species and Sepal.Width
SepalLengthModel = lm(Sepal.Length ~ Species + Sepal.Width, data = iris)

# Let's see the summary output
summary(SepalLengthModel)

##############################################################################
#output
  #Call:
  #lm(formula = Sepal.Length ~ Species + Sepal.Width, data = iris)
#Residuals:
 # Min       1Q   Median       3Q      Max 
#-1.30711 -0.25713 -0.05325  0.19542  1.41253 
#Coefficients:
 # Estimate Std. Error t value Pr(>|t|)    
#(Intercept)         2.2514     0.3698   6.089 9.57e-09 ***
 # Speciesversicolor   1.4587     0.1121  13.012  < 2e-16 ***
#  Speciesvirginica    1.9468     0.1000  19.465  < 2e-16 ***
#  Sepal.Width         0.8036     0.1063   7.557 4.19e-12 ***
 # ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.438 on 146 degrees of freedom
#Multiple R-squared:  0.7259,	Adjusted R-squared:  0.7203 
#F-statistic: 128.9 on 3 and 146 DF,  p-value: < 2.2e-16
##Is this  GOOD MODEL:  
# Why is .4 or .3 good enough.   
# r^2 .72 means the variability in the dependent variable  is a 72% is explained by an independent variable.
#This is good because its a high.
############################################################################################

# Let's build a data frame that contains the original data, residuals, and predictions
ModelOutput = data.frame(iris, # include the original data set
                         Residuals = SepalLengthModel$residuals,
                         Predictions = SepalLengthModel$fitted.values)

# Let's look at the histograms of the model residuals, by Species
ggplot(data = ModelOutput, mapping = aes(x = Residuals)) +
  geom_histogram(aes(fill = Species), color = "white", bins = 15) +
  facet_wrap(~ Species, nrow = 3) +
  labs(titlw = "Model Residuals by Species",
       y = "Number of Observations",
       x = "Residuals") +
  theme_light()

# Now let's check the residual scatter plots by Species
ggplot(data = ModelOutput, mapping = aes(x = Predictions, y = Residuals)) +
  geom_point(aes(color = Species)) + 
  geom_hline(yintercept = 0, color = "red") +
  facet_wrap(~ Species, nrow = 3) +
  labs(title = "Residual Scatter Plots by Species") +
  theme_light()

# Modeling Non-Linear Relationships
# Read in the mpg data set

mpg <- read_tsv("mpg.txt")

# We would like to predict hwy with the displ variable
# First let's plot the data
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(color = "#006EA1") +
  labs(title = "Highway Fuel Efficiency vs Displacement",
       y = "MPG Highway",
       x = "Vehicle Displacement") + 
  theme_light()

# First let try a simple linear regression
SimpleLR = lm(hwy ~ displ, data = mpg)
summary(SimpleLR)

# Let's view the residual scatter plot
SimpleLROutput = data.frame(hwy = mpg$hwy,
                            displ = mpg$displ,
                            Residuals = SimpleLR$residuals,
                            Predictions = SimpleLR$fitted.values)

# Scatter plot, notice a curve?
ggplot(data = SimpleLROutput, mapping = aes(x = Predictions, y = Residuals)) +
  geom_point(color = "#006EA1") + 
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Residual Scatter Plot for Simple Linear Fit") +
  theme_light()

# Original data with simple regression line
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(color = "#006EA1") +
  geom_smooth(method = "lm", se = FALSE, color = "orange") +
  labs(title = "Highway Fuel Efficiency vs Displacement",
       y = "MPG Highway",
       x = "Vehicle Displacement") + 
  theme_light()

# Now lets add a displ^2 term to account for the non-linearity
QuadraticFit = lm(hwy ~ displ + I(displ^2), data = mpg)

# View the summary output
summary(QuadraticFit)

# Now let's make a scatter with the quadratic fit
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(color = "#006EA1") +
  geom_line(aes(x = displ, y = QuadraticFit$fitted.values), color = "red") +
  labs(title = "Highway Fuel Efficiency vs Displacement",
       y = "MPG Highway",
       x = "Vehicle Displacement") + 
  theme_light()


# Model Interaction Between Variables
# Read in Advertising Data set
setwd("C:/Users/Sanaa Mironov/Documents/UMBC/CourseDataSets")
Advertising = read_csv("Advertising.csv")

# Let's fit an interaction term between TV and Radio to the Advertising data
Interaction = lm(Sales ~ TV + Radio + TV:Radio, data = Advertising)

# Summary Output
summary(Interaction)

# Discovering Interactions
Interaction = Advertising %>% mutate(TV_Radio = TV * Radio) %>% 
  select(Sales, TV_Radio)

cor(Interaction)

# Model Selection Techniques
# Create the Null and Full Models
Null = lm(Sales ~ 1, data = Advertising)
Full = lm(Sales ~ TV + Radio + Newspaper, data = Advertising)

# Forward Selection
step(Null, # Starts with Null Model
     scope = list(upper = Full),
     method = "forward", trace = 1)

# Backward Selection
step(Full, # starts with Full Model
     scope = list(lower = Null), 
     method = "backward", trace = 1)


# Mixed Selection
step(Null, # Starts with Null Model
     scope = list(lower = Null, upper = Full), # enter both bounds for mixed
     method = "both", trace = 1)




