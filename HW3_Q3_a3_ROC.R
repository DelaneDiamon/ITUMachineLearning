rm(list=ls())	

library(MASS)


xlambda=rep(0, times = 30)
for(i in seq(from = 0, to = 29)){
  #
  exp <- (+3 -4*(i/20))
  xlambda[i+1] <- 10^exp
}

# we only need Versicolor and Virginica data.
iris_data = iris[51:150,]
target = rep(0,100)
target[iris_data[,5]=="Iris-versicolor"] = 1
target[iris_data[,5]!="Iris-versicolor"] = -1
row.names(iris_data)<-NULL
iris_data = cbind(iris_data, target)
set_seed <- function(i) {
  set.seed(i)
  if (exists(".Random.seed"))  oldseed <- get(".Random.seed", .GlobalEnv)
  if (exists(".Random.seed"))  assign(".Random.seed", oldseed, .GlobalEnv)
}

k = 10
# 10-fold cross valiation is used.
num_sample = nrow(iris_data)

set_seed(pi)
#set.seed(pi)
iris_data = iris_data[sample(num_sample, num_sample, replace=FALSE),]
iris_train = iris_data[1:(num_sample*((k-1)/k)),]
iris_cross = iris_data[1:(num_sample*(1/k)),]

error_train_total = matrix(0, nrow = length(xlambda), ncol = 1)
error_cross_total = matrix(0, nrow = length(xlambda), ncol = 1)
for(ilambda in 1:length(xlambda)){
  pick = k #pick kth set
  
  error_train = 0
  error_cross = 0
  
  for(j in 1:k){
    i_tmp = 1
    for(i in 1:k){
      
      #choose training set, and cross validation set 
      if(i == pick){
        iris_cross = iris_data[((i-1)*num_sample/k+1):(num_sample*(i/k)),]
      } else {
        iris_train[((i_tmp-1)*num_sample/k+1):(num_sample*(i_tmp/k)), 
                   ] = iris_data[((i-1)*num_sample/k+1):(num_sample*i/k),]
        i_tmp = i_tmp + 1
      }
    }
    pick = pick - 1
    y_iris_train = iris_train[,6]
    x_iris_train = iris_train[,1:4]
    yx_iris_train = cbind(x_iris_train, y_iris_train)
    
    y_iris_cross = iris_cross[,6]
    x_iris_cross = iris_cross[,1:4]
    
    iris_model = lm.ridge(y_iris_train~., yx_iris_train, lambda=xlambda[ilambda])
    A = as.array(iris_model$coef[1:4]/iris_model$scales)
    X_train = x_iris_train
    for( i in seq(from = 1, to = ncol(x_iris_train))){
      X_train[,i] = x_iris_train[,i] - iris_model$xm[i]
    }
    X_train=as.matrix(X_train)
    yh = X_train%*%A + iris_model$ym
    
    yhP = (yh >= 0.0)
    yp = (y_iris_train >= 0.0)
    error_train = error_train + sum(yhP != yp)/(length(y_iris_train)*k*0.00001/0.00001)
    X_cross = x_iris_cross
    for( i in seq(from = 1, to = ncol(x_iris_cross))){
      X_cross[,i] = x_iris_cross[,i] - iris_model$xm[i]
    }
    X_cross=as.matrix(X_cross)
    yh = X_cross%*%A + iris_model$ym
    
    
    yhP = (yh >= 0.0)
    yp = (y_iris_cross >= 0.0)
    
    error_cross = error_cross + sum(yhP != yp)/(length(y_iris_cross)*k*0.00001/0.00001)
    
  }
  
  error_train_total[ilambda,1] = error_train
  error_cross_total[ilambda,1] = error_cross
}

min_iris_lambda <- xlambda[min(which(min(error_cross_total) == error_cross_total))]
th_lambda = min(which(min(error_cross_total) == error_cross_total))
cat(th_lambda, "th lambda", min_iris_lambda, "is optimal.")

plot(1:length(xlambda),error_train_total[,1],
     ylim=c(min(error_train_total, error_cross_total),
            max(error_train_total, error_cross_total)))
points(1:length(xlambda),error_cross_total[,1], col='red')
