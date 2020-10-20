library(tidyverse)
library(readxl)
library(kknn)
#glimsetwd("c:/users/Sanaa Mironov/My Documents/UMBC/CourseDataSets")
Heart = read_excel(path = "./Heart Disease.xlsx")

# cars is a built in dataset
data(cars)

## Regression with *knn*

ggplot(data = cars, mapping = aes(x = dist, y = speed)) +
geom_point(color = "#006EA1") +
labs(title = "Speed vs Stopping Distance",
x = "Stopping Distance",
y = "Speed") +
theme_light()


KNN_4 = kknn(speed ~ dist, # formula is the same as in lm()
train = cars,
test = cars,
k = 4)

ggplot(data = cars, mapping = aes(x = dist, y = speed)) +
geom_point(color = "#006EA1") +
geom_point(aes(x = dist, y = KNN_4$fitted.values), color = "orange", size = 2) +
labs(title = "Speed vs Stopping Distance",
x = "Stopping Distance",
y = "Speed") +
theme_light()


KNN_10 = kknn(speed ~ dist,
train = cars,
test = cars,
k = 10)

ggplot(data = cars, mapping = aes(x = dist, y = speed)) +
geom_point(color = "#006EA1") +
geom_point(aes(x = dist, y = KNN_10$fitted.values), color = "orange", size = 2) +
labs(title = "Speed vs Stopping Distance",
x = "Stopping Distance",
y = "Speed") +
theme_light()


## Classification with *knn*

# Recode the HeartDisease as a factor
Heart$HeartDisease = factor(Heart$HeartDisease,
levels = c("Yes","No"),
labels = c("Yes","No"))
# Let's try k = 8
KNNHeart = kknn(HeartDisease ~ ChestPain + Age, train = Heart, test = Heart, k = 8)

# Add predicted values to our dataset
Heart = Heart %>% mutate(HeartDiseaseKNN = KNNHeart$fitted.values)

# Let's take a look at the result
Heart %>% select(HeartDisease, HeartDiseaseKNN)

Heart %>% group_by(HeartDisease,HeartDiseaseKNN) %>% summarise(Patients = n())


# Optimal k
set.seed(12745) # This sets a start value for generating random numbers

KNNHeartOptimal = train.kknn(HeartDisease ~ ChestPain + Age,
data = Heart,
kmax = 20) # max k we are interested in trying

# Now we can view the results with the summary function
summary(KNNHeartOptimal)

###on assigment 3
# Logistic Regression

# The I() function creates a logical vector that is TRUE when HeartDisease is "Yes"
# and FALSE otherwise
Heart = Heart %>% mutate(HeartDisease = I(HeartDisease == "Yes") %>% as.numeric())

#glimpse(Heart)

# Let's predict HeartDisease with the Age variable
HeartLogit = glm(HeartDisease ~ Age, # same as in lm()
                 data = Heart,
                 family = "binomial") # for logistic, this is always set to "binomial"

summary(HeartLogit)

# Let's add our predicted probabilities to our original dataset
Heart = Heart %>% mutate(ProbHeartYes = HeartLogit$fitted.values)
#library(psych)
# describe(HeartLogit$fitted.values)


#look into using this for the project 3
#change the number to see different probabilities
# Now let's predict Y = 1 if P(Y = 1) > 0.5
Heart = Heart %>% mutate(HeartLogitPredicited = I(ProbHeartYes > 0.5) %>% as.numeric())

# How did we do? Same syntax as in KNN classification
# Make sure to add ungroup() at the end
ResultsTable = Heart %>% group_by(HeartDisease,HeartLogitPredicited) %>% 
  summarise(Patients = n()) %>% ungroup() 

#fitted value: this is the probabilty you have heart disease
#HeartLogit$fitted.values

# View ResultsTable thi is a confusion table
ResultsTable


# First we create a variable that calculates the Patients as a percentage
ResultsTable = ResultsTable %>% mutate(Percentage = Patients/sum(Patients))

# Now we can find were the predicted and observed match and 
# calculate the percentage correctly classified
ResultsTable %>% filter(HeartDisease == HeartLogitPredicited) %>% 
  summarise(PctCorrect = sum(Percentage))

# Let's choose to predicit Y = 1 when the probability that Y = 1 > 0.4
# This would be a more conservative estimate because we want
# to make sure we find people with heart disease
Heart = Heart %>% mutate(HeartLogitPredicited = I(ProbHeartYes > 0.4) %>% as.numeric())

# Let's view our classifications compared to observed values
Heart %>% group_by(HeartDisease,HeartLogitPredicited) %>% 
  summarise(Patients = n()) %>% ungroup() %>% # ungroup() before mutate()
  mutate(Percentage = Patients/ sum(Patients))

# What's our percentage correctly classified with this cut-off?
# Remember that n() counts the number of rows in your filtered dataframe
Heart %>% filter(HeartDisease == HeartLogitPredicited) %>% 
  summarise(PctCorrect = n() / nrow(Heart))


# Logistic Regression with Multiple Predictors

# Let's predict HeartDisease with the ChestPain, Gender, and Age variables
HeartLogitMultiple = glm(HeartDisease ~ ChestPain + Gender + Age,
                         data = Heart,
                         family = "binomial")

# Summary of results
summary(HeartLogitMultiple)

# Variable Selection in Logistic Regression

# Full Model you are interested in checking
Full = glm(HeartDisease ~ Gender + Age + MaxHR + RestBP + Cholesterol + Oldpeak, 
           data = Heart, 
           family = "binomial")
# Null Model
Null = glm(HeartDisease ~ 1, 
           data = Heart, 
           family = "binomial")

# Perform mixed selection
step(Null, 
     scope = list(lower = Null, upper = Full), 
     direction = "both",
     trace = 0)




