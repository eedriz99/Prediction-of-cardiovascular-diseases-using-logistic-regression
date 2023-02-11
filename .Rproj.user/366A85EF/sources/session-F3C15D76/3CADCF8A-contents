#Import the dataset
heart <- read.csv("~/Eedriz Project/heart.csv", stringsAsFactors = TRUE)

#Perform the summary statistics
summary(heart)

library(caret)

logReg_model <- train(HeartDisease ~ Age + Sex + ChestPainType + RestingBP + Cholesterol + FastingBS + RestingECG + MaxHR + ExerciseAngina + Oldpeak + ST_Slope, data = heart, method = "glm", family = "binomial")