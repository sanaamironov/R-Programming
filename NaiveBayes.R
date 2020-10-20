library(mlbench)
library(e1071)
library(datasets)
data(iris)

summary(iris)

## 75% of the sample size
smp_size <- floor(0.75 * nrow(iris))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(iris)), size = smp_size)
##it choose the row and then train_ind is rows
train <- iris[train_ind, ]
##choose all the rows any of the train_ind
test <- iris[-train_ind, ]

NVmodel <- naiveBayes(Species ~ ., data = train)
##print  NVmodel
preds <- predict(NVmodel, newdata = test)
##print preds  to see what it predicted
conf_matrix <- table(preds, test$Species)
##print conf_matrix
NVmodel$tables

