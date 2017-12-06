#######################################
# Model functions
# Saaya Yasuda (sy2569)
# All model functions assume the 1st column is the y/label.
#######################################

rm(list=ls())
setwd('~/Documents/Github/fall2017-project5-proj5-grp9')

packages.used=c("nnet","gbm","caret","randomForest","e1071")

packages.needed=setdiff(packages.used, intersect(installed.packages()[,1], packages.used))
if(length(packages.needed)>0){
  install.packages(packages.needed, dependencies = TRUE,repos='http://cran.us.r-project.org')
}
library(nnet) # nnet & multinom
library(gbm)
library(caret) # test result etc
library(randomForest) #RF
library(e1071) #SVM

#######################################
# Load features
#######################################

data = read.csv("./output/all_features.csv",header=T)
<<<<<<< HEAD
rownames(data) = data[,1] # moving file names to rownames
data = data[,-1] # removing the file name col

data = as.matrix(data)
=======
data<-data[(data$age>0),]
data$sex[data$sex=="famale"]<-"female"
rownames(data) = data[,1] # moving file names to rownames
data = data[,-1] # removing the file name col

#data = data[,-ncol(data)] #removing duration
x = data[,c(1:22)] # indep variables
age = data[,23] # age
gender = data[,27] # gender
accent = data[,26] # accent

age_df = data.frame(age, x)

gender_df = data.frame(gender, x)
accent_df = data.frame(accent, x)

>>>>>>> 122bb781a567cb204745cf79e8144b85047ae770
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

# y cols
#age = as.factor(unlist(train_age["age"]))
#sex = as.factor(unlist(train_sex["sex"]))
#country = as.factor(unlist(train_country["country"]))

#typeof(train_age)

#######################################
# test_result function
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


#######################################
# 
#######################################

svmfit_co = svm(country ~ ., train)
pred = predict(svmfit_co, test)

postResample(pred, test["country"])
confusionMatrix(pred,test["country"])

svmfit_sex = svm(sex ~ ., train)
preds = predict(svmfit_sex, test)
postResample(pred, test["sex"])





multinom_fit = multinom(formula = as.factor(country) ~ .,
                        data=train_country, MaxNWts = 100000, maxit = 100)
pred = predict(multinom_fit, type="class", newdata=test_country)
postResample(pred, test_country[,1])
confusionMatrix(pred, test_country[,1])$table

logistic_fit2 = glm(formula = sex ~ ., data=train_sex, family=binomial(link='logit'))
pred2 = predict(logistic_fit2, newdata=test_sex,type="response")
postResample(pred2, test_sex[,1])

logistic_fit2



test_result(test_country,multinom_fit$fitted.values,T)


multinom_train = function(train_data, y, is_factor){
  set.seed(123)
<<<<<<< HEAD
  if(is_factor){
    multinom_fit = multinom(formula = as.factor(y) ~ .,
                            data=train_data, MaxNWts = 100000, maxit = 500)
  }
  else{
    multinom_fit = multinom(formula = unlist(y) ~ .,
                            data=train_data, MaxNWts = 100000, maxit = 500)
  }
=======
  multinom_fit = multinom(formula = y ~ .,
                          data=train_data, MaxNWts = 100000, maxit = 500)
>>>>>>> 122bb781a567cb204745cf79e8144b85047ae770
  return(fit=multinom_fit)
}

### Run it:
# Training - age
<<<<<<< HEAD
age_multinom_fit = multinom_train(train_age, train_age["age"], is_factor=FALSE)

country_multinom_fit = multinom_train(train_country, train_country["country"], is_factor=T)
# Testing - age
test_result(test_age, age_multinom_fit, is_factor=TRUE)
=======
age_multinom_fit = multinom_train(age_train, age_train[,1])
 #Testing - age
test_result(age_test, age_multinom_fit)
>>>>>>> 122bb781a567cb204745cf79e8144b85047ae770

# the rest
RUN = FALSE
if (RUN){
  gender_multinom_fit = multinom_train(gender_train, gender_train[,1])
  accent_multinom_fit = multinom_train(accent_train, accent_train[,1])
  test_result(gender_test, gender_multinom_fit)
  test_result(accent_test, accent_multinom_fit)
}

# 161.653 secs
#system.time(multinom_train(age_train, age_train[,1]))

# VERY time consuming.
#stepwisefit = step(age_multinom_fit$fit, direction="both",scope=formula(age_multinom_fit$fit))


#######################################
# nnet_train function - input must be [-1,1]
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
# SVM function WIP
#######################################

svm_train = function(train_data, y){
  set.seed(123)
  svm_fit = svm(formula = as.factor(y) ~ .,
                data=train_data, na.action = "na.omit", trace=T)
  return(svm_fit)
}

svm_fit= svm_train(age_train, age_train[,1]) # error

tuneResult <- tune(svm, Y ~ X,  data = data,
                   ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9))
)
print(tuneResult)
# Draw the tuning graph
plot(tuneResult)

#######################################
# random forest function WIP
#######################################

random_forest_train = function(train_data, n_trees) {
  mtry = tuneRF(y = as.factor(train_data[,1]), 
                x= train_data[,-1], ntree=n_trees)
  mtry = mtry[,1][which.min(mtry[,2])]
  
  rf_fit = randomForest(as.factor(train_data[,1]) ~ ., 
                        data = train_data, ntree=n_trees, 
                        mtry=mtry, importance=T)
  return(rf_fit)
}

rf1 = random_forest_train(age_train,1)


