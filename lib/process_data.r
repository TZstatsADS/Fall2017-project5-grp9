#######################################
# Process data
# Saaya Yasuda (sy2569)
#######################################

rm(list=ls())
setwd('~/Documents/Github/fall2017-project5-proj5-grp9')

#######################################
# install & load necessary packages
#######################################
packages.used=c("tuneR")

packages.needed=setdiff(packages.used, intersect(installed.packages()[,1], packages.used))
if(length(packages.needed)>0){
  install.packages(packages.needed, dependencies = TRUE,repos='http://cran.us.r-project.org')
}
library(tuneR)

#######################################
# load data
#######################################

other_dev = read.csv("./data/cv-other-dev.csv",header=T,na.strings=c("","NA"))
other_train = read.csv("./data/cv-other-train.csv",header=T,na.strings=c("","NA"))
valid_test = read.csv("./data/cv-valid-test.csv",header=T,na.strings=c("","NA"))

# remove NA rows (i.e. files without labels)
other_dev = other_dev[,-ncol(other_dev)]
other_dev = na.omit(other_dev)

other_train = other_train[,-ncol(other_train)]
other_train = na.omit(other_train)

valid_test = valid_test[,-ncol(valid_test)]
valid_test = na.omit(valid_test)

# Saving them as csv
write.csv(other_dev,file="./output/other_dev_clean.csv",row.names=FALSE)
write.csv(other_train,file="./output/other_train_clean.csv",row.names=FALSE)
write.csv(valid_test,file="./output/valid_test_clean.csv",row.names=FALSE)




