#QUESTION4

# 4) See if you can improve on regression-based classification of the iris data 
# that we did in class. Classify the iris data set with second degree terms 
# added using a ridge regression. 
# (ie supplement the original 4 attributes x1, x2, x3, and x4
# by including the 10 second degree terms ( x1*x1, x1*x2, x1*x3, … ) 
# for a total of 14 attributes.) Use multiclass to classify the data and then 
# compare the results with the results obtained in class.

secondDegreeTerms = x_iris_all
secondDegreeTerms = mutate(secondDegreeTerms, V1.1 = V1*V1, V2.2 = V2*V2, V3.3 = V3*V3, V4.4 = V4*V4,
                   V1.2 = V1*V2, V1.3 = V1*V3, V1.4 = V1*V4, V2.3 = V2*V3,
                   V2.4 = V2*V4, V3.4=V3*V4)

secondDegreeTermsY = cbind(secondDegreeTerms, y_iris_all)

error_train_total2 = matrix(0, nrow = length(xlambda), ncol = 1)
error_cross_total2 = matrix(0, nrow = length(xlambda), ncol = 1)

iris_train = secondDegreeTermsY[1:(num_sample*((k-1)/k)),]
iris_cross = secondDegreeTermsY[1:(num_sample*((1)/k)),]

for(ilambda in 1:length(xlambda)){
  pick = k #pick kth set
  
  error_train = 0
  error_cross = 0
  
  for(j in 1:k){
    i_tmp = 1
    for(i in 1:k){
      
      #choose training set, and cross validation set 
      if(i == pick){
        iris_cross = secondDegreeTermsY[((i-1)*num_sample/k+1):(num_sample*(i/k)),]
      } else {
        iris_train[((i_tmp-1)*num_sample/k+1):(num_sample*(i_tmp/k)),
                   ] = secondDegreeTermsY[((i-1)*num_sample/k+1):(num_sample*(i/k)),]
        i_tmp = i_tmp + 1
      }
    }
    
    pick = pick - 1
    
    y_iris_train = iris_train[,15]
    x_iris_train = iris_train[,-15]
    yx_iris_train = cbind(x_iris_train, y_iris_train)
    
    y_iris_cross = iris_cross[,15]
    x_iris_cross = iris_cross[,-15]
    
    
    iris_model = lm.ridge(y_iris_train~., yx_iris_train, lambda=xlambda[ilambda])
    
    A = as.array(iris_model$coef[1:14]/iris_model$scales)
    X_train = x_iris_train
    for( i in seq(from = 1, to = ncol(x_iris_train))){
      X_train[,i] = x_iris_train[,i] - iris_model$xm[i]
    }
    X_train=as.matrix(X_train)
    yh = X_train%*%A + iris_model$ym
    
    yhP = (yh >= 0.0)
    yp = (y_iris_train >= 0.0)
    error_train = error_train + sum(yhP != yp)/(length(y_iris_train)*k/0.00001*0.00001)
    
    
    X_cross = x_iris_cross
    for( i in seq(from = 1, to = ncol(x_iris_cross))){
      X_cross[,i] = x_iris_cross[,i] - iris_model$xm[i]
    }
    X_cross=as.matrix(X_cross)
    yh = X_cross%*%A + iris_model$ym
    
    
    
    yhP = (yh >= 0.0)
    yp = (y_iris_cross >= 0.0)
    
    error_cross = error_cross + sum(yhP != yp)/(length(y_iris_cross)*k/0.00001*0.00001)
    
  }
  
  error_train_total2[ilambda,1] = error_train
  error_cross_total2[ilambda,1] = error_cross
}

min_iris_lambda2 <- xlambda[min(which(min(error_cross_total2) == error_cross_total2))]
th_lambda = min(which(min(error_cross_total2) == error_cross_total2))
cat(th_lambda, "th lambda", min_iris_lambda2, "is optimal.")

plot(1:length(xlambda),error_train_total2[,1],
     ylim=c(min(error_train_total2, error_cross_total2),
            max(error_train_total2, error_cross_total2)))
points(1:length(xlambda),error_cross_total2[,1], col='red')

sprintf("minimum error of linear model with no additional term: %f", min(error_cross_total))

sprintf("minimum error of linear model with 2 dim additional term: %f", min(error_cross_total2))
  