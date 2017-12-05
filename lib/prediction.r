#######################################
# Model functions
# Saaya Yasuda (sy2569)
#######################################

rm(list=ls())
setwd('~/Documents/Github/fall2017-project5-proj5-grp9')

packages.used=c("nnet","gbm","caret")

packages.needed=setdiff(packages.used, intersect(installed.packages()[,1], packages.used))
if(length(packages.needed)>0){
  install.packages(packages.needed, dependencies = TRUE,repos='http://cran.us.r-project.org')
}
library(nnet)
library(gbm)
library(caret)

#######################################
# Load summary
#######################################

data = read.csv("./output/example_summary_stats.csv",header=T)
rownames(data) = data[,1] # moving file names to rownames
data = data[,-1] # removing the file name col

data = data[,-ncol(data)] #removing duration
x = data[,c(1:(ncol(data)-3))] # indep variables
age = data[,ncol(data)-2] # age
gender = data[,ncol(data)-1] # gender
accent = data[,ncol(data)] # accent

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

test_result <- function(test_data, fit){
  # input: test data & fit obtained from train data
  # output: accuracy and confusion matrix
  pred = predict(fit, type="class", newdata=test_data)
  accuracy = postResample(test_data[,1], pred)
  matrix = confusionMatrix(test_data[,1], pred)$table
  return(list(accuracy,matrix))
}

# This prints out the result:
# test_result(age_test, age_multinom_fit)


#######################################
# multinom_train function
#######################################
multinom_train <- function(train_data, y){
  multinom_fit <- multinom(formula = as.factor(y) ~ .,
                           data=train_data, MaxNWts = 100000, maxit = 500)
  return(fit=multinom_fit)
}

### Run it:
# Training
age_multinom_fit = multinom_train(age_train, age_train[,1])
gender_multinom_fit = multinom_train(gender_train, gender_train[,1])
accent_multinom_fit = multinom_train(accent_train, accent_train[,1])

# Testing
test_result(age_test, age_multinom_fit)
test_result(gender_test, gender_multinom_fit)
test_result(accent_test, accent_multinom_fit)


# 161.653 secs
#system.time(multinom_train(age_train, age_train[,1]))

# VERY time consuming.
#stepwisefit = step(age_multinom_fit$fit, direction="both",scope=formula(age_multinom_fit$fit))


#######################################
# nnet_train function WIP
#######################################
nnet_train <- function(train_data, y, size){
  nnet_fit <- nnet(formula = as.factor(y) ~ .,
                   data=train_data, MaxNWts = 100000, 
                   maxit = 1000, size = size, trace=T)
  return(nnet_fit)
}


nnet1 = nnet_train(age_train, age_train[,1], 1)
nnet2 = nnet_train(age_train, age_train[,1], 2)
#nnet3 = nnet_train(age_train, age_train[,1], 3)
accuracy_vec = c()

pred = predict(nnet1, type="class", newdata=age_test)
postResample(age_test[,1], pred)
confusionMatrix(age_test[,1], pred)$table

test_result(age_test, nnet1)

for(i in 1:5){
  fit = nnet_train(age_train, age_train[,1], i)
  nnettest_result = test_result(age_test, fit)
  accuracy = nnettest_result[[1]]
  accuracy_vec <- c(accuracy_vec, accuracy)
}
accuracy_vec

#######################################
# nnet_train function
#######################################

