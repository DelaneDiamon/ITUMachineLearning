#QUESTION 5
# This is a multi-class problem. Consider the Glass Identification Data Set from the UC Irvine Data Repository. 
# This problem will only work with building and vehicle window glass (classes 1,2 and 3), 
# so it only uses the first 163 rows of data. (Ignore rows 164 through 214) 
# With this set up this is a three class problem. 
# Use ridge regression to classify this data into the three classes: 
# building windows float processed, building windows non float processed, 
# and vehicle windows float processed.

## Q5
k = 10
glass_data = read.table("glass.txt", header=FALSE, sep=',')
glass_data = glass_data[1:163,]

row.names(glass_data)<-NULL

num_sample = nrow(glass_data)
glass_data = glass_data[sample(num_sample, num_sample, replace=FALSE),]

class1 = rep(0,163)
class2 = rep(0,163)
class3 = rep(0,163)

class1[glass_data[,11]==1] = 1
class1[glass_data[,11]!=1] = -1

class2[glass_data[,11]==2] = 1
class2[glass_data[,11]!=2] = -1

class3[glass_data[,11]==3] = 1
class3[glass_data[,11]!=3] = -1


glass_data = cbind(glass_data, class1, class2, class3)


min_glass_lambda=rep(1000,3)
glass_train = glass_data[1:round((num_sample*((k-1)/k))),]

error_glass_train_total = matrix(0, nrow = length(xlambda), ncol = 3)
error_glass_cross_total = matrix(0, nrow = length(xlambda), ncol = 3)

th_lambda = c(0,0,0)
min_error = c(0,0,0)

for(iy in 1:3){
  
  for(ilambda in 1:length(xlambda)){
    pick = k #pick kth set
    
    error_train = 0
    error_cross = 0
    
    for(j in 1:k){
      i_tmp = 1
      for(i in 1:k){
        
        #choose training set, and cross validation set 
        if(i == pick){
          glass_cross = glass_data[(round((i-1)*num_sample/k)+1):(round(num_sample*(i/k))),]
        } else {
          glass_train[((i_tmp-1)*num_sample/k+1):(num_sample*(i_tmp/k)),
                      ] = glass_data[((i-1)*num_sample/k+1):(num_sample*(i/k)),]
          i_tmp = i_tmp + 1
        }
      }
      
      pick = pick - 1
      
      y_glass_train = glass_train[,11+iy]
      x_glass_train = glass_train[,1:10]
      yx_glass_train = cbind(x_glass_train, y_glass_train)
      
      y_glass_cross = glass_cross[,11+iy]
      x_glass_cross = glass_cross[,1:10]
      
      glass_model = lm.ridge(y_glass_train~., yx_glass_train, lambda=xlambda[ilambda])
      A = as.array(glass_model$coef[1:10]/glass_model$scales)      
      X_train = x_glass_train
      for( i in seq(from = 1, to = ncol(x_glass_train))){
        X_train[,i] = x_glass_train[,i] - glass_model$xm[i]
      }
      X_train=as.matrix(X_train)
      yh = X_train%*%A + glass_model$ym
      
      
      yhP = (yh >= 0.0)
      yp = (y_glass_train >= 0.0)
      error_train = error_train + sum(yhP != yp)/(length(y_glass_train)*k*0.00001/0.00001)
      
      
      X_cross = x_glass_cross
      for( i in seq(from = 1, to = ncol(x_glass_cross))){
        X_cross[,i] = x_glass_cross[,i] - glass_model$xm[i]
      }
      X_cross=as.matrix(X_cross)
      yh = X_cross%*%A + glass_model$ym
      
      
      yhP = (yh >= 0.0)
      yp = (y_glass_cross >= 0.0)
      
      error_cross = error_cross + sum(yhP != yp)/(length(y_glass_cross)*k*0.00001/0.00001)
      
    }
    
    error_glass_train_total[ilambda,iy] = error_train
    error_glass_cross_total[ilambda,iy] = error_cross
    
  }
  min_glass_lambda[iy] <- xlambda[min(which(min(error_glass_cross_total[,iy
                                                                        ]) == error_glass_cross_total[,iy]))]
  th_lambda[iy]=min(which(min(error_glass_cross_total[,iy]) == error_glass_cross_total[,iy]))
}

sprintf("minimum classification error are %f, %f, %f",
        error_glass_cross_total[5,1], 
        error_glass_cross_total[11,2],
        error_glass_cross_total[13,3])

## [1] "minimum classification error are 0.018382, 0.165441, 0.085662"
plot(1:length(xlambda),error_glass_train_total[,1],
     ylim=c(min(error_glass_train_total, error_glass_cross_total),
            max(error_glass_train_total, error_glass_cross_total)))
points(1:length(xlambda),error_glass_cross_total[,1], col='red')

plot(1:length(xlambda),error_glass_train_total[,2],
     ylim=c(min(error_glass_train_total, error_glass_cross_total),
            max(error_glass_train_total, error_glass_cross_total)))
points(1:length(xlambda),error_glass_cross_total[,2], col='red')

plot(1:length(xlambda),error_glass_train_total[,3],
     ylim=c(min(error_glass_train_total, error_glass_cross_total),
            max(error_glass_train_total, error_glass_cross_total)))
points(1:length(xlambda),error_glass_cross_total[,3], col='red')