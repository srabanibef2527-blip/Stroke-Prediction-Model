# Stroke Prediction Model 

#1] Load the neccessary libraries
library(tidyverse)
library(caTools)
library(rpart)
library(rpart.plot)
library(caret)
library(pROC)

#2] Load the dataset and do neccessary conversions which include data cleaning 
# converting the dataset from character to numeric wherever neccessary

data.frame <- read.csv("C:/Users/HP/Downloads/healthcare-dataset-stroke-data.csv")
str(data.frame)
data.frame$bmi <- as.numeric(data.frame$bmi)
data.frame$bmi[is.na(data.frame$bmi)] <- median(data.frame$bmi, na.rm = TRUE)


data.frame$gender <- factor(data.frame$gender, levels = c(0, 1), labels = c("No", "Yes"))

data.frame$ever_married   <- factor(data.frame$ever_married)
data.frame$work_type      <- factor(data.frame$work_type)
data.frame$Residence_type <- factor(data.frame$Residence_type)
data.frame$smoking_status <- factor(data.frame$smoking_status)

data.frame$hypertension  <- as.numeric(data.frame$hypertension)
data.frame$heart_disease <- as.numeric(data.frame$heart_disease)

str(data.frame)



#3] Logistic Binary Regression

data.frame$stroke <- as.factor(data.frame$stroke)

model2 <- glm(data.frame$stroke ~ data.frame$hypertension + data.frame$heart_disease +
              data.frame$avg_glucose_level,
              family = binomial)
summary(model2)



