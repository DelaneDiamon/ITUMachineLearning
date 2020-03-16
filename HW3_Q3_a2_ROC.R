# TODO: Add comment
# 
# NAME: HW3_Q3 
rm(list=ls())	

#Question 3

setwd('/Users/atureci/Documents/ITU/MachineLearning/Week3/HW3-AlpTureci-95583')
library(MASS)
iris_ = iris[50:150,]
iris_train = iris_[1:70,]
iris_test = iris_[71:101,]
Err <- matrix(nrow = 21, ncol = 3)
I <- seq(1:nrow(iris_))

for(iLambda in seq(from = 0, to = 20)){
  #
  exp <- (+3 -4*(iLambda/20))
  xlambda <- 10^exp
  
  testErr <- 0.0
  trainErr <- 0.0

  for(ixval in seq(from = 1, to = 10)){
    Iout <- which(I%%10 == ixval - 1)
    iris_In <- iris_[-Iout,]
    iris_Out <- iris_[Iout,]
    
    Xin <- iris_In[,1:4]
    Xout <- iris_Out[,1:4]
    Yin <- iris_In[,5]
    Yout <- iris_Out[,5]
    mod <- lm.ridge(Species~.,data=iris_In,lambda=xlambda)
    C <- mod$coef/mod$scales
    XM <- Xin
    
    for(i in seq(from = 1, to = ncol(Xin))){
      XM[,i]<-Xin[,i]-mod$xm[i]
    } 
    X <- as.matrix(XM)
    A <- as.array(C)
    Yh <- X%*%A + mod$ym
    trainErr <- trainErr + sum(abs(Yin - Yh))/(nrow(as.matrix(Yin))*10)    
    
    XM <- Xout
    for(i in seq(from = 1, to = ncol(Xout))){
      XM[,i]<-Xout[,i]-mod$xm[i]
    } 
    X <- as.matrix(XM)
    A <- as.array(C)
    Yh <- X%*%A + mod$ym
    testErr <- testErr + sum(abs(Yout - Yh))/(nrow(as.matrix(Yout))*10)
  }
  Err[(iLambda+1),1] = trainErr
  Err[(iLambda+1),2] = testErr
  Err[(iLambda+1),3] = xlambda
}
  
plot(Err[,1], type='p', col='red', ylim=c(0,1),
     main = 'Error vs Log(Lambda)',
     ylab='Error',
     xlab='3 - Log(Lambda)')
points(Err[,1], pch=15, col='red')
#lines(out[,2], type='l', col='blue')
points(Err[,2], pch=16, col='blue')
legend(5, 0.2, c("TRAIN", "TEST"), cex = 1, col = c("red", "blue"),
       pch = c(15, 16), lty = 1:2)

# Use the best lambda and all the data
#     to create the production model
mindex <- which(min(Err[,2]) == Err[,2])
xlambda <- Err[mindex,3]

Xin <- iris_[,1:4]
Yin <- iris_[,5]
mod <- lm.ridge(Species~.,data=iris_,lambda=xlambda)
C <- mod$coef/mod$scales
XM <- Xin
for(i in seq(from = 1, to = ncol(Xin))){
  XM[,i]<-Xin[,i]-mod$xm[i]
} 
X <- as.matrix(XM)
A <- as.array(C)
Yh <- X%*%A + mod$ym
trainErr <- sum(abs(Yin - Yh))/nrow(as.matrix(Yin))    

# Use the production model to produce the
#          ROC curve

YinP <- Yin >0
YinN <- Yin<0
YhP <- Yh>0
YhN<- Yh<0

##calculate misclassification error with threshold 0.0
sum(YinP == YhP)/length(Yin)


# First Notice the Max and Min of Yh
max(Yh)
#[1] 1.439719
min(Yh)
#[1] -1.885619

#Generate ROC curve
p <- sum(YinP)
n <- sum(YinN)
tp <- c(rep(0.0,100))
fp <- c(rep(0.0,100))

# As the value of Yh is varying from 1.4 to -1.88, 
#     to generate the the ROC curve the threshold
#     needs to vary through these values.

for(i in 1:100){
  thresh <- 2 - i*0.04
  y <- Yh>=thresh
  tp[i] <- sum(y & YinP)/p
  fp[i] <- sum(y & YinN)/n
  
}

plot(fp,tp)
abline(0,1)

