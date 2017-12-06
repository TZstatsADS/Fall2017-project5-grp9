#######################################
# Process data (UNUSED)
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
# load & clean csv files
#######################################

other_dev = read.csv("./data/cv-other-dev.csv",header=T,
                     na.strings=c("","NA"),
                     colClasses=c("filename"="character","text"="character"))
other_train = read.csv("./data/cv-other-train.csv",header=T,
                     na.strings=c("","NA"),
                     colClasses=c("filename"="character","text"="character"))
valid_test = read.csv("./data/cv-valid-test.csv",header=T,
                     na.strings=c("","NA"),
                     colClasses=c("filename"="character","text"="character"))

# remove NA rows (i.e. files without labels)
other_dev = other_dev[,-ncol(other_dev)]
other_dev = na.omit(other_dev)
row.names(other_dev) = 1:nrow(other_dev)

other_train = other_train[,-ncol(other_train)]
other_train = na.omit(other_train)
row.names(other_train) = 1:nrow(other_train)

valid_test = valid_test[,-ncol(valid_test)]
valid_test = na.omit(valid_test)
row.names(valid_test) = 1:nrow(valid_test)

# Saving them as csv
write.csv(other_dev,file="./output/other_dev_clean.csv",row.names=FALSE)
write.csv(other_train,file="./output/other_train_clean.csv",row.names=FALSE)
write.csv(valid_test,file="./output/valid_test_clean.csv",row.names=FALSE)

# summary(other_dev)

#######################################
# load data based on csv
#######################################

#test
filename = other_dev[1,1]
w = readMP3(paste0("./data/",filename))
str(w) # print

melw = melfcc(w)


####
#trying out codes from tutorials

length(w@left)/w@samp.rate # 2.88 secs of the data
s <- w@left / 2^(w@bit -1) # converting to the -1 to 1 scale

time_array <- (0:(length(w@left)-1)) / w@samp.rate * 1000 # ms
plot(time_array, s, type='l', col='black', xlab='Time (ms)', ylab='Amplitude')

n = length(s)
p = fft(s)

n_unique_pts <- ceiling((n+1)/2)
p <- p[1:n_unique_pts] #select just the first half since the second half 
   # is a mirror image of the first
p <- abs(p)
p <- p / n
p <- p^2

if (n %% 2 > 0){
  p[2:length(p)] <- p[2:length(p)]*2 # we've got odd number of points fft
} else {
  p[2: (length(p) -1)] <- p[2: (length(p) -1)]*2 # we've got even number of points fft
}

freq_array <- (0:(n_unique_pts-1)) * (w@samp.rate / n) #  create the frequency array 
plot(freq_array/1000, 10*log10(p), type='l', col='black', xlab='Frequency (kHz)', ylab='Power (dB)')

rms_val <- sqrt(mean(s^2))
rms_val
#[1] 0.09726062
sqrt(sum(p))
#[1] 0.09726062



my.ceps <- function(wave) {
  signal <- wave@left
  n <- length(signal)
  f <- wave@samp.rate
  N <- round(n/2)
  z1 <- Re(fft(log(abs(fft(signal))), inverse = TRUE))
  z <- z1[1:N]
  x <- seq(0, N/f, length.out = N)
  results <- cbind(x, z)
  return(results)
}

my.ceps.w = my.ceps(w)