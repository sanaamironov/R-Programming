library(tidyverse)
library(readxl)
library(caret)

setwd("c:/users/Sanaa Mironov/Documents/UMBC/CMSC491/CourseDataSets")
Heart = read_excel(path = "./Heart Disease.xlsx")



# Logistic Regression

# The I() function creates a logical vector that is TRUE when HeartDisease is "Yes"
# and FALSE otherwise
Heart = Heart %>% mutate(HeartDisease = I(HeartDisease == "Yes") %>% as.numeric())
#Separating Test and Training Data
TrainIndex = sample(1:nrow(Heart), round(0.7*nrow(Heart)))
HeartTrain = Heart[TrainIndex, ] 
HeartTest = Heart[-TrainIndex, ] 

# Let's predict HeartDisease with the Age variable
HeartLogit = glm(HeartDisease ~ ., # same as in lm()
                 data = HeartTrain,
                 family = "binomial") # for logistic, this is always set to "binomial"

summary(HeartLogit)


#creates a new column called EstimatedProb in HeartTest
HeartTest = HeartTest %>% 
  mutate(EstimatedProb = predict(HeartLogit,
                                 newdata = HeartTest,
                                 type = "response"))


# Now let's predict Y = 1 if P(Y = 1) > 0.4
HeartTest = HeartTest %>% mutate(HeartLogitPredicited = I(EstimatedProb > 0.4) %>% as.numeric())


heartTable = table(HeartTest$HeartLogitPredicited ,HeartTest$HeartDisease)

confusionMatrix(heartTable)
