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
# Load summary
#######################################

data = read.csv("./output/all_features.csv",header=T)
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

#######################################
# Divide into test and train for testing
#######################################

divide_dataset = function(df){
  set.seed(123)
  index = sample(1:nrow(df), size=0.7*nrow(df))
  train_data = df[index,]
  test_data = df[-index,]
  return(list(train_data,test_data))
}

adf = divide_dataset(age_df)
age_train = adf[[1]]
age_test = adf[[2]]

gdf = divide_dataset(gender_df)
gender_train = gdf[[1]]
gender_test = gdf[[2]]

accdf = divide_dataset(accent_df)
accent_train = accdf[[1]]
accent_test = accdf[[2]]

#######################################
# test_result function
#######################################

test_result = function(test_data, fit){
  # input: test data & fit obtained from train data
  # output: accuracy and confusion matrix
  pred = predict(fit, type="class", newdata=test_data)
  accuracy = postResample(pred, test_data[,1])
  matrix = confusionMatrix(pred, test_data[,1])$table
  return(list(accuracy,matrix))
}

# This prints out the result:
# test_result(age_test, age_multinom_fit)


#######################################
# multinom_train function
#######################################
multinom_train = function(train_data, y){
  set.seed(123)
  multinom_fit = multinom(formula = y ~ .,
                          data=train_data, MaxNWts = 100000, maxit = 500)
  return(fit=multinom_fit)
}

### Run it:
# Training - age
age_multinom_fit = multinom_train(age_train, age_train[,1])
 #Testing - age
test_result(age_test, age_multinom_fit)

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


