
#Load all required R packages
library(car)
library(FactoMineR)
library(mlr3verse)
library(mlr3learners)
library(mlr3pipelines)

#Import the dataset
heart <- read.csv("~/Eedriz Project/heart.csv", stringsAsFactors = TRUE)

#Perform the summary statistics
summary(heart)

#build a linear model
model = lm(HeartDisease ~ Age + Sex + ChestPainType + RestingBP + Cholesterol + FastingBS + RestingECG + MaxHR + ExerciseAngina + Oldpeak + ST_Slope, heart)
summary(model)

#Test for auto correlation
durbinWatsonTest(model)

# ##Test for multicolinearity
car::vif(model)

#changing the HeartDisease column to factor
heart$HeartDisease <- factor(heart$HeartDisease, levels = c('0', '1'), labels = c("0","1"))

#Create training and test data
#splits <- partition()

seed <- sample(c(rep(0, 0.8*nrow(heart)), rep(1, 0.2*nrow(heart))))

train <- heart[seed == 0, ]

test <- heart[seed == 1, ]

# algorithm selection
task <- TaskClassif$new(id = 'Heart disease prediction', backend= train, target= "HeartDisease")

# choose the logistic regression learner for a logistic regression model fitting
mlr_learners$get("classif.log_reg")

#initialize a new learner
learner <- LearnerClassifLogReg$new()

#train the learner
model <- learner$train(task)

#view the model and the respective coefficients of the features
fitted.model <- learner$model

#make predictions with the test data
prediction <- model$predict_newdata(test)
prediction$score()

#print the cofusion matrix of the predicted values
confusion.matrix <- prediction$confusion

# From the confusion matrix, the follow value are gotten:

TP <- 85
TN <- 72
FP <- 10
FN <- 16


sensitivity <- TP/(TP + FN)
specificity <- TN/(TN + FP)

#perf <- performance(my.prediction, Measure= list(msr("classif.cm")))
