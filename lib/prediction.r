#######################################
# Model functions
# Saaya Yasuda (sy2569)
#######################################

rm(list=ls())
setwd('~/Documents/Github/fall2017-project5-proj5-grp9')

packages.used=c("nnet","gbm")

packages.needed=setdiff(packages.used, intersect(installed.packages()[,1], packages.used))
if(length(packages.needed)>0){
  install.packages(packages.needed, dependencies = TRUE,repos='http://cran.us.r-project.org')
}
library(nnet)
library(gbm)

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
# multinom_train function
#######################################
multinom_train <- function(train_data, y){
  multinom_fit <- multinom(formula = y ~ .,
                           data=train_data, MaxNWts = 100000, maxit = 500)
  #top_models = varImp(multinom_fit)
  #top_models$variables = row.names(top_models)
  #top_models = top_models[order(-top_models$Overall),]
  return(fit=multinom_fit)
}

# run it:
age_multinom_fit = multinom_train(age_df, age)

#######################################
# multinom_test function
#######################################
multinom_test <- function(test_data, fit){
  multinom_pred = predict(fit, type="class", newdata=test_data)
  return(multinom_pred)
}

# run it:
multinomtest_result = multinom_test(test_data,multinomfit_train$fit)
postResample(test_data$label,multinomtest_result)


#system.time(multinom_train(train_data))

# VERY time consuming:
#stepwisefit = step(multinomfit_train$fit, direction="both",scope=formula(multinomfit_train$fit))


nnet_train = function(train data, other params){
  fit = 
  return fit
}


