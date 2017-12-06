#######################################
# Model functions
# Saaya Yasuda (sy2569)
#######################################

rm(list=ls())
setwd('~/Documents/Github/fall2017-project5-proj5-grp9')

packages.used=c("nnet","gbm","caret","randomForest","e1071","xgboost","dplyr")

packages.needed=setdiff(packages.used, intersect(installed.packages()[,1], packages.used))
if(length(packages.needed)>0){
  install.packages(packages.needed, dependencies = TRUE,repos='http://cran.us.r-project.org')
}
library(nnet) # nnet & multinom
library(caret) # test result etc
library(randomForest) #RF
library(e1071) #SVM etc
library(gbm) #gbm
library(xgboost)
library(dplyr)

RUNALL = FALSE # If this is true, it runs time consuming functions.

#######################################
# Load features
#######################################

#data_orig = read.csv("./output/all_features.csv",header=T)
data = read.csv("./output/all_features.csv",header=T)

# fixing original dataset's typo
data<-data[(data$age>0),]
data$sex[data$sex=="famale"] = "female"
data$sex = factor(data$sex,levels = c("female","male"))

rownames(data) = data[,1] # moving file names to rownames
data = data[,-1] # removing the file name col

### Remove uncommon countries.
# Those countries initially caused problems in predictions.

countries = sort(table(data$country),decreasing=T)
uncommon = countries[countries<=5] # less than 5 occurences
uncommon = names(uncommon)
common = countries[countries>5] # less than 5 occurences
common = names(common)

uncommon =data[data$country %in% uncommon,]
common =data[data$country %in% common,]
uncommon$country="other"
data = rbind(common,uncommon)

data$country = droplevels(data$country) # reduce levels


#######################################
# Divide into test and train for testing
#######################################
set.seed(123)
index = sample(1:nrow(data), size=0.7*nrow(data))
train = data[index,]
test = data[-index,]

train_age = data.frame(train["age"], train[,c(1:(ncol(data)-7))])
train_sex = data.frame(train["sex"], train[,c(1:(ncol(data)-7))])
train_country = data.frame(train["country"], train[,c(1:(ncol(data)-7))])
test_age = data.frame(test["age"], test[,c(1:(ncol(data)-7))])
test_sex = data.frame(test["sex"], test[,c(1:(ncol(data)-7))])
test_country = data.frame(test["country"], test[,c(1:(ncol(data)-7))])

#######################################
# SVM & tuned SVM
#######################################
# Age
set.seed(123)
svmfit_age = svm(age ~ ., train_age)
svmpred_age = predict(svmfit_age, test_age)
MSE_svm = mean( (svmpred_age - test_age$age)^2)
MSE_svm #185.0999


#tuning - takes REALLY LONG TIME to run. didn't finish.
if (RUNALL){
  svmtuned_age <- tune(svm, age ~ .,  data = train_age, 
                       ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9)))
  print(svmtuned_age)
  #plot(svmtuned_age)
}
#svmfit_age_better = svm(age ~ ., train_age)
#svmpred_age_better = predict(svmfit_age_better, test_age)
#MSE_svm_better = mean( (svmpred_age_better - test_age$age)^2)
#MSE_svm_better



# Sex
set.seed(123)
svmfit_sex = svm(sex ~ ., train_sex)
svmpred_sex = predict(svmfit_sex, test_sex)
table(svmpred_sex, test_sex$sex)
#svmpred_sex female male
#female    297   19
#male       20  304
postResample(svmpred_sex, test_sex$sex)
#Accuracy     Kappa 
#0.9390625 0.8781107


#tuning - takes a while to run
if (RUNALL){
  svmtuned_sex <- tune(svm, sex ~ .,  data = train_sex, 
                       ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9)))
  print(svmtuned_sex)
  #- best parameters:
  #  epsilon cost
  #0    8
  #png(filename="./output/svm_tuned_sex.png")
  #plot(svmtuned_sex)
  #dev.off()
  
}
svmfit_sex_better = svm(sex ~ ., train_sex, epsilon=0, cost=8)
svmpred_sex_better = predict(svmfit_sex_better, test_sex)
table(svmpred_sex_better, test_sex$sex)
postResample(svmpred_sex_better, test_sex$sex)
# didn't improve.
# Accuracy     Kappa 
#0.9328125 0.8656093 


# Country
set.seed(123)
svmfit_co = svm(country ~ ., train_country)
svmpred_co = predict(svmfit_co, test_country)
#table(svmpred_co,test_country$country)
postResample(svmpred_co, test_country$country)
#Accuracy      Kappa 
#0.21562500 0.07901526 

#tuning - takes REALLY LONG TIME to run. Didn't get to finish running.
if (RUNALL){
  svmtuned_country <- tune(svm, country ~ .,  data = train_country, 
                           ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9)))
  print(svmtuned_country)
  
  #plot(svmtuned_country)
}
#svmfit_country_better = svm(country ~ ., train_country)
#svmpred_country_better = predict(svmfit_country_better, test_country)
#table(svmpred_country_better, test_country$country)
#postResample(svmpred_country_better, test_country$country)


#######################################
# Random forest
#######################################

# Tested with and without tuneRF, and it was better with tuneRF.

#Age
set.seed(123)
MSE_vec = c()
for (ntree in 1:40){
  mtry_age = tuneRF(x=subset(train_age, select=-age), y = train_age$age, 
                    ntree=ntree,trace=FALSE,plot=FALSE)
  best_mtry_age = mtry_age[,1][which.min(mtry_age[,2])]
  rffit_age = randomForest(age ~ ., data = train_age, ntree=ntree, 
                           importance=T, mtry = best_mtry_age)
  rfpred_age = predict(rffit_age, test_age)
  MSE = mean( (rfpred_age - test_age$age)^2)
  MSE_vec = c(MSE_vec,MSE)
}
names(MSE_vec) = 1:40
#png(filename="./output/RF_age_MSE.png")
plot(MSE_vec,xlab="number of trees",ylab="MSE", 
     main="Random Forest Model: Age Prediction MSE")
#dev.off()
which.min(MSE_vec) 
#25 is the best with 177.6382

#> MSE_vec[25]
#25 
#177.6382 



#Sex
set.seed(123)
accuracy_vec_sex = c()
for (ntree in 1:50){
  mtry_sex = tuneRF(x=subset(train_sex, select=-sex), y = train_sex$sex, 
                    ntree=ntree,trace=FALSE,plot=FALSE)
  best_mtry_sex = mtry_sex[,1][which.min(mtry_sex[,2])]
  rffit_sex = randomForest(sex ~ ., data = train_sex, ntree=ntree, 
                           importance=T, mtry = best_mtry_sex)
  rfpred_sex = predict(rffit_sex, test_sex)
  #confmat = table(rfpred_sex, test_sex$sex)
  accuracy = postResample(rfpred_sex, test_sex$sex)
  accuracy_vec_sex = c(accuracy_vec_sex, accuracy[[1]])
}
names(accuracy_vec_sex) = 1:50

#png(filename="./output/RF_sex_Accuracy.png")
plot(accuracy_vec_sex,xlab="number of trees",ylab="Accuracy",
     main="Random Forest Model: Sex Prediction Accuracy")
#dev.off()

which.max(accuracy_vec_sex) 
#36 is the best with 0.940625
#> accuracy_vec_sex[36]
#36 
#0.940625



# Country
set.seed(123)
accuracy_vec_country = c()
for (ntree in seq(5,200,5)){
  mtry_country = tuneRF(x=subset(train_country, select=-country), y = train_country$country, 
                        ntree=ntree,trace=FALSE,plot=FALSE)
  best_mtry_country = mtry_country[,1][which.min(mtry_country[,2])]
  rffit_country = randomForest(country ~ ., data = train_country, ntree=ntree, 
                               importance=T, mtry = best_mtry_country)
  rfpred_country = predict(rffit_country, test_country)
  #confmat = table(rfpred_country, test_country$country)
  accuracy = postResample(rfpred_country, test_country$country)
  accuracy_vec_country = c(accuracy_vec_country, accuracy[[1]])
}
names(accuracy_vec_country) = seq(5,200,5)

#png(filename="./output/RF_country_Accuracy.png")
plot(accuracy_vec_country,xlab="number of trees",ylab="Accuracy",
     main="Random Forest Model: Country Prediction Accuracy")
#dev.off()

which.max(accuracy_vec_country) 
# 165 is the best with 0.2109375

#> accuracy_vec_country["165"]
#165 
#0.2109375


#######################################
# XGBoost
#######################################

set.seed(123)
params_df = expand.grid(nrounds=c(100,200),
                        eta = c(0.1,0.3,0.5),
                        gamma=1,
                        max_depth = c(3,5,7,10),
                        colsample_bytree=c(0.5,0.7,0.9),
                        min_child_weight=1:2,
                        subsample = c(0.5,0.75,1))

train_control = trainControl(method = "cv", number = 5,
                             verboseIter = T, returnData = F,
                             returnResamp = "all", allowParallel = T)



# Sex
labels_train = as.matrix(as.integer(train_sex$sex)-1)
xgb_sex_cv = xgb.cv(data =subset(data.matrix(train_sex), select=-sex),
                    test = data.matrix(test_sex),
                    label = labels_train, nrounds=10, nfold=5, num_class=2,nthread=2,
                    objective="multi:softmax",eval_metric="merror",prediction=T)

xgb_train_sex = train(x=subset(data.matrix(train_sex), select=-sex), 
                      y=as.factor(labels_train),
                      trControl = train_control,
                      tuneGrid = params_df,
                      method = "xgbTree")
#best param
head(xgb_train_sex$results[with(xgb_train_sex$results,order(Accuracy, decreasing=T)),],5)
xgb_train_sex$bestTune
#     nrounds max_depth eta gamma colsample_bytree min_child_weight subsample
#26     200         3 0.1     1              0.9                1       0.5

xgbfit_sex = xgboost(data =subset(data.matrix(train_sex), select=-sex),
                     label = labels_train, objective="multi:softmax",
                     eval_metric="merror",num_class=2, verbose=F,
                     params = xgb_train_sex$bestTune,
                     nrounds=xgb_train_sex$bestTune$nrounds)

labels_test = as.matrix(as.integer(test_sex$sex)-1)
test_sex2 = data.frame(labels_test, subset(data.matrix(test_sex), select=-sex))

xgpred_sex = predict(xgbfit_sex, subset(data.matrix(test_sex), select=-sex),reshape=T)
xgpred_sex = factor(xgpred_sex,labels = c("female","male"))

postResample(xgpred_sex, test_sex$sex)
#Accuracy     Kappa 
#0.9421875 0.8843682 
table(xgpred_sex, test_sex$sex)
#xgpred_sex female male
#female    299   19
#male       18  304



# Re-set up params for the two dfs that take a long time to run on.
params_df2 = expand.grid(nrounds=c(100),
                        eta = c(0.1,0.3,0.5),
                        gamma=1,
                        max_depth = c(3,5,7),
                        colsample_bytree=c(0.5,0.7,0.9),
                        min_child_weight=1:2,
                        subsample = c(0.5,1))

train_control2 = trainControl(method = "cv", number = 3,
                             verboseIter = T, returnData = F,
                             returnResamp = "all", allowParallel = T)



# Country
set.seed(123)
labels_train = as.matrix(as.integer(train_country$country)-1)

xgb_country_cv = xgb.cv(data =subset(data.matrix(train_country), select=-country),
                        test = data.matrix(test_country),
                        label = labels_train, nrounds=10, nfold=5, num_class=88,nthread=2,
                        objective="multi:softmax",eval_metric="merror",prediction=T)

xgb_train_country = train(x=subset(data.matrix(train_country), select=-country), 
                          y=as.factor(labels_train),
                          trControl = train_control2,
                          tuneGrid = params_df2,
                          method = "xgbTree")
#best param
head(xgb_train_country$results[with(xgb_train_country$results,order(Accuracy, decreasing=T)),],5)
xgb_train_country$bestTune
#     nrounds max_depth eta gamma colsample_bytree min_child_weight subsample
#28     200         3 0.1     1              0.9                1      0.75

xgbfit_country = xgboost(data =subset(data.matrix(train_country), select=-country),
                         label = labels_train, objective="multi:softprob",
                         eval_metric="merror",num_class=88, verbose=F,
                         params = xgb_train_country$bestTune,
                         nrounds=xgb_train_country$bestTune$nrounds)

labels_test = as.matrix(as.integer(test_country$country)-1)

xgpred_country = predict(xgbfit_country, 
                         subset(data.matrix(test_country), select=-country),reshape=T)
colnames(xgpred_country) = levels(test_country$country)
xgpred_country = cbind(xgpred_country, maxcol=apply(xgpred_country, 1, which.max))

which.max(xgpred_country[100,])

xgpred_country = data.frame(xgpred_country) %>%
  mutate(max_prob=which.max(.))
#mutate(max_prob=max.col(.,"last"))

xgpred_country = factor(xgpred_country,labels = levels(test_country$country))

postResample(xgpred_country, test_country$country)
table(xgpred_country, test_country$country)


# Age
set.seed(123)
labels_train = as.matrix(as.integer(train_age$age)-1)

xgb_age_cv = xgb.cv(data =subset(data.matrix(train_age), select=-age),
                    test = data.matrix(test_age),
                    label = labels_train, nrounds=10, nfold=5, num_class=88,nthread=2,
                    objective="multi:softmax",eval_metric="merror",prediction=T)

xgb_train_age = train(x=subset(data.matrix(train_age), select=-age), 
                      y=as.factor(labels_train),
                      trControl = train_control2,
                      tuneGrid = params_df2,
                      method = "xgbTree")
#best param
head(xgb_train_age$results[with(xgb_train_age$results,order(Accuracy, decreasing=T)),],5)
xgb_train_age$bestTune
#     nrounds max_depth eta gamma colsample_bytree min_child_weight subsample
#28     200         3 0.1     1              0.9                1      0.75

xgbfit_age = xgboost(data =subset(data.matrix(train_age), select=-age),
                     label = labels_train, objective="multi:softprob",
                     eval_metric="merror",num_class=88, verbose=F,
                     params = xgb_train_age$bestTune,
                     nrounds=xgb_train_age$bestTune$nrounds)

labels_test = as.matrix(as.integer(test_age$age)-1)

xgpred_age = predict(xgbfit_age, 
                     subset(data.matrix(test_age), select=-age),reshape=T)
colnames(xgpred_age) = levels(test_age$age)
xgpred_age = cbind(xgpred_age, maxcol=apply(xgpred_age, 1, which.max))








#######################################
# Logistic regression
#######################################

# Sex
set.seed(123)
lgfit_sex = glm(formula = sex ~ ., data=train_sex, family=binomial(link='logit'))
lgpred_sex = predict(lgfit_sex, test_sex)
table(lgpred_sex, test_sex$sex)
postResample(lgpred_sex, test_sex$sex)

# Age
set.seed(123)
lgfit_age = glm(formula = age ~ ., data=train_age, family=binomial(link='logit'))
lgpred_age = predict(lgfit_age, test_age)
MSE_lg = mean( (lgpred_age - test_age$age)^2)
MSE_lg #

#Country
set.seed(123)
lgfit_country = multinom(formula = country ~ .,
                        data=train, MaxNWts = 100000, maxit = 100)
lgpred_country = predict(lgfit_country, test_country)
#table(lgpred_country, test_country$country)
postResample(lgpred_country, test_country$country)





#######################################
# UNUSED nnet_train function - input must be [-1,1]
#######################################

# If it's scalable data
maxs <- apply(age_train, 2, max) 
mins <- apply(age_train, 2, min)
scaled <- as.data.frame(scale(age_train, center = mins, scale = maxs - mins))
train_nn <- scaled[index,]
test_nn <- scaled[-index,]

nnet_train = function(train_data, y, size){
  set.seed(123)
  nnet_fit = nnet(formula = as.factor(y) ~ .,
                  data=train_data, MaxNWts = 100000, maxit = 100,
                  size = size, na.action = "na.omit", trace=T)
  return(nnet_fit)
}

nnet_train_cv = function(train_data, test_data){
  accuracy_vec = c()
  for(i in 1:5){
    fit = nnet_train(train_data, train_data[,1], i)
    nnettest_result = test_result(test_data, fit)
    accuracy = nnettest_result[[1]]
    print(accuracy)
    accuracy_vec = c(accuracy_vec, accuracy)
  }
  return(accuracy_vec)
}

## Run it
nnet_train_cv(age_train, age_test)


#######################################
# UNUSED test_result function
#######################################

test_result = function(test_data, fit, is_factor){
  # INPUT: test data & fit obtained from train data
  # OUTPUT: accuracy and confusion matrix
  if(is_factor){
    pred = predict(fit, type="class", newdata=test_data)
  }
  else{
    pred = predict(fit, type="probs", newdata=test_data)
  }
  accuracy = postResample(pred, test_data[,1])
  matrix = confusionMatrix(pred, test_data[,1])$table
  return(list(accuracy,matrix))
}

# This prints out the result:
# test_result(age_test, age_multinom_fit)
