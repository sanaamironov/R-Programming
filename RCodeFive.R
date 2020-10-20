# Introduction to Multiple Linear Regression
# Import packages
library(tidyverse)
library(readxl)

# Read in the data
Advertising = 
read_csv("Advertising.csv")


# Fit the full model, using all three predictor variables
FullModel = lm(Sales ~ TV + Radio + Newspaper, data = Advertising)


## F-Test: Is At Least One Predictor Related to the Response Variable?

summary(FullModel)

## Partial F-Test: Are a Subset of the Predictor Variables Related to the Response?

FullModel = lm(Sales ~ TV + Radio + Newspaper, data = Advertising)
ReducedModel = lm(Sales ~ TV, data = Advertising)

# Now pass them into the anova function, with the reduced model first
anova(ReducedModel, FullModel)


# View the Full Model Once Again
summary(FullModel)

TempModel = lm(Sales ~  Newspaper, data = Advertising)
summary(TempModel)



# Beta3 Test
FullModel = lm(Sales ~ TV + Radio + Newspaper, data = Advertising)
ReducedModelBeta3 = lm(Sales ~ TV + Radio, data = Advertising) # All but Newspaper
anova(ReducedModelBeta3, FullModel) # Remember, reduced model first

## Multicolinearity
NewspaperLR = lm(Sales ~ Newspaper, data = Advertising)

summary(NewspaperLR)

NewsRadioLR = lm(Sales ~ Newspaper + Radio, data = Advertising)

summary(NewsRadioLR)

## Finding Correlations Among Predictor Variables

pairs(Advertising, # Your data frame,
pch = 20, # point type option
col = "#006EA1") # color of your choice


cor(Advertising)





