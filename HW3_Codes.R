rm(list=ls())	

#Question 1

setwd('/Users/atureci/Documents/ITU/MachineLearning/Week3/HW3-AlpTureci-95583')

library(ISLR)		
set.seed(pi)

wine_data<-read.csv("winequality-red.csv",header = TRUE, sep=";")
scaled_wine_data<-cbind(scale(wine_data))

#function_ols <- function(wine_data){
  wine_train<-wine_data[1:1400,]
  x_wine_train<-wine_train[,1:11]
  y_wine_train<-wine_train[,12]
  
  wine_test<-wine_data[1401:dim(wine_data)[1],]
  x_wine_test<-wine_test[,1:11]
  y_wine_test<-wine_test[,12]
  
  #Don’t forget to scale each column before you create the model.??
  lm_wine_train<-lm(y_wine_train~., data = as.data.frame(x_wine_train))
  OLS_coef <- coef(lm_wine_train)
  
  
  #How well did the model predict the results for the last 199 observations?
  predicted_OLS_quality<-predict(lm_wine_train, newdata = as.data.frame(x_wine_test))
  dY<-y_wine_test - predicted_OLS_quality
  ols_testErr <- sqrt(sum(dY*dY))/(length(y_wine_test))	
  paste("using OLS the estimated test error = ", ols_testErr)
  
  #What measure did you use to evaluate how well the model did this prediction?
  # Next use the model to predict the results for the whole data set 
  # and measure how well your model worked. 
  # (hint: use the r function lm and the regression example from class)
  wine_data_x<-wine_data[,1:11]
  wine_data_y<-wine_data[,12]
  
  lm_wine_data<-predict(lm_wine_train, newdata = as.data.frame(wine_data_x))
  dYData <- wine_data_y - lm_wine_data
  dataErr <- sqrt(sum(dYData*dYData))/(length(wine_data_y))
  
  paste("using OLR the estimated test error = ",dataErr)
  #lm_wine_data
  summary(lm_wine_data)

#}

#function_ols(wine_data)
#function_ols(scaled_wine_data)


# QUESTION 2
# Perform a ridge regression on the wine quality data set 
# from problem 1 using only the first 1400 observations.

#install.packages("glmnet")
library(glmnet)
#grid=10^seq(10,-2,length=100)
wine_data<-read.csv("winequality-red.csv",header = TRUE, sep=";")

ridge_wine_train = wine_data[1:1400,]
ridge_x_wine_train = ridge_wine_train[,1:11]
ridge_y_wine_train = ridge_wine_train[,12]

test_wine_ridge = wine_data[1401:dim(wine_data)[1],]
test_x_wine = test_wine_ridge[,1:11]
test_y_wine = test_wine_ridge[,12]

cv.out=cv.glmnet(as.matrix(ridge_x_wine_train), ridge_y_wine_train, alpha = 0 )
plot(cv.out)
bestlambda=cv.out$lambda.min
bestlambda

#Make fair comraison of Error
ridgeMod=glmnet(as.matrix(ridge_x_wine_train), ridge_y_wine_train, alpha = 0, lambda = bestlambda)
predicted_Ridge_quality= predict(ridgeMod, newx = as.matrix(test_x_wine))
ridge_testErr = sqrt(sum((test_y_wine - predicted_Ridge_quality)^2))/length(predicted_Ridge_quality)
ridge_coef<-coef(ridgeMod)

paste("using Ridge the estimated test error = ", ridge_testErr)
paste("using Ridge Lambda = ", bestlambda, "alpha = 0")

# Compare the results of applying the ridge regression model 
# to the last 199 observations with the results of
# applying the ordinary least square model to these observations
# Predicted_OLS - Predicted_Ridge
# Plot all 3 Vars
#install.packages("ggplot2")
library(ggplot2)
#install.packages("dplyr")
library(dplyr)
#install.packages("tidyr")
library(tidyr)

index = c(1401:dim(wine_data)[1])
df=data.frame(index,test_y_wine, predicted_OLS_quality, predicted_Ridge_quality)

dfplot <- df %>% gather(key, value, -index)

ggplot(dfplot, mapping = aes(x = index, y = value, color = key) ) + geom_line()
diff_OLS_Ridge = predicted_OLS_quality - predicted_Ridge_quality

# Compare the coefficients resulting from the ridge regression 
# with the coefficients that were obtained in problem 1. 
# What conclusions can you make from this comparison?
summary(ridge_coef)
mean(ridge_coef)
median(ridge_coef)
summary(OLS_coef)