
#Question 1
setwd('/Users/atureci/Documents/ITU/MachineLearning/Week3/HW3-AlpTureci-95583')

wine_data<-read.csv("winequality-red.csv",header = TRUE, sep=";")
wine_train<-wine_data[1:1400,]
x_wine_train<-wine_train[,1:11]
y_wine_train<-wine_train[,12]

wine_test<-wine_data[1401:dim(wine_data)[1],]
x_wine_test<-wine_test[,1:11]
y_wine_test<-wine_test[,12]

#Don’t forget to scale each column before you create the model.??
lm_wine_train<-lm(y_wine~., data = x_wine)

#How well did the model predict the results for the last 199 observations?
wine_test_fit<-predict(lm_wine_train, newdata = x_wine_test)
dY<-y_wine_test - wine_test_fit
testErr <- sqrt(sum(dY*dY))/(length(y_wine_test))	

#What measure did you use to evaluate how well the model did this prediction?

#Next use the model to predict the results for the whole data set and measure how well your model worked. (hint: use the r function lm and the regression example from class)
wine_data_x<-wine_data[,1:11]
wine_data_y<-wine_data[,12]

wine_data_fit<-predict(lm_wine_train, newdata = wine_data_x)
dYData<-wine_data_y-wine_data_fit
dataErr <- sqrt(sum(dYData*dYData))/(length(wine_data_y))

