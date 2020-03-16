
#QUESTION 3
# This problem uses the Iris Data Set. It only involves the Versicolor and
# Virginica species (rows 51 through 150). Use cross validatediris = ir ridge
# regression to classify these two species. Create and plot a ROC curve for
# this classification method.

iris_ = iris[50:150,]
#iris_train = iris_[1:70,]
#iris_test = iris_[71:101,]

#x=model.matrix(iris_$Species~.,iris_)[,-1]
#y=iris_$Species

observation_num<-nrow(iris_)
sample_iris<-iris_[sample(observation_num, observation_num, replace = FALSE),]
k =3
#iris_train = matrix(0, (k-1)*observation_num/k, ncol(iris_))
iris_train = sample_iris[1:(observation_num*(2/3)),]
max_depth=10
trainErrTot = rep(0, times = max_depth)
crossErrTot = rep(0, times = max_depth)

for(idepth in 1:max_depth){
  pick = k
  for(j in 1:k){
    i_tmp = 1
    for (i in 1:k){
      if(i == pick){
        iris_cross = sample_iris[((i-1)*observation_num/k+1):(observation_num*(i/k)),]
      } else {
        iris_train[((i_tmp-1)*observation_num/k+1):(observation_num*(i_tmp/k)),] = sample_iris[((i-1)*observation_num/k+1):(observation_num*(i/k)),]
        i_tmp = i_tmp +1
      }
    }
    pick = pick -1
    species = iris_train[,5]
    x_iris = iris_train[,1:4]
    
    #fit<-rpart(y_wine~., x_wine, control = rpart.control(maxdepth = idepth))
    ridge_iris<-glmnet(species~., x_iris, alpha = 0)
    #ridge.mod=glmnet(x,y,alpha=0,lambda=grid)
    
    errTrain = sqrt(sum((species - predict(ridge_iris, x_iris))^2)/length(species))
    trainErrTot[idepth] = trainErrTot[idepth] + errTrain
    
    y_iris_cross = iris_cross[,5]
    x_iris_cross = iris_[,1:4]
    
    crossError = sqrt(sum((y_iris_cross - predict(ridge_iris, x_iris_cross))^2)/length(y_iris_cross))
    crossErrTot[idepth] = crossErrTot[idepth] + crossError
  }
  
  trainErrTot[idepth] = trainErrTot[idepth]/k
  crossErrTot[idepth] = crossErrTot[idepth]/k
  
  if(idepth > 1 && crossErrTot[idepth] < crossErrTot[idepth -1]){
    fit_save = fit_save
    min_depth = idepth
  }
  if(idepth==5){
    opt_fit = fit
  }
}

plot(1:max_depth, trainErrTot)
points(1:max_depth, crossErrTot, col='red')

cat(crossErrTot, "\n", min_depth, "th cross validation error has the best value")
print(opt_fit)

plot(opt_fit)
text(opt_fit)

plot_frame = data.frame(sample_iris$alcohol, sample_iris$Species)
