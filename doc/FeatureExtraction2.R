library(warbleR)
library(readr)
library(tuneR)
library(dplyr)
library(e1071)

speakerInfo<-read_csv("../data/speech-accent-archive/speakers_all.csv")
speakerInfo<-speakerInfo[speakerInfo$`file_missing?`==FALSE,]
speakerInfo<-speakerInfo[,1:8]

##Now Converting to Wave Files
dir<-getwd()
dir<-gsub("doc","data/speech-accent-archive/recordings",dir)
##This function works in the folder it's given
#mp32wav(from=dir)
##Need to move them to their own directory
#waves<-list.files(dir)
#waves<-waves[grepl(".wav",waves)]
#dir.create("../data/WaveFiles")
#waves<-paste("../data/speech-accent-archive/recordings/",waves,sep="")
#file.copy(waves,"../data/WaveFiles")
#file.remove(waves)


files<-paste(speakerInfo$filename,".wav",sep="")
files<-files[files %in% list.files(path="../data/WaveFiles/")]
files<-paste("../data/WaveFiles/",files,sep="")

##Start with empty dataframe
stats<-data.frame(
  mean=NA,
  sd=NA,
  median=NA,
  sem=NA,
  mode=NA,
  Q25=NA,
  Q75=NA,
  IQR=NA,
  cent=NA,
  skewness=NA,
  kurtosis=NA,
  sfm=NA,
  sh=NA,
  prec=NA,
  meanfun=NA,
  minfun=NA,
  maxfun=NA,
  meandom=NA,
  mindom=NA,
  maxdom=NA,
  dfrange=NA,
  modindx=NA,
  file=NA
)
##Heavy Lifting---creates the summary stats for the files
for(i in 1:length(files)){
  r<-readWave(files[i])
  songspec <- seewave::spec(r, f = r@samp.rate, plot = FALSE)
  analysis <- seewave::specprop(songspec, f = r@samp.rate, 
                                flim = c(0, 280/1000), plot = FALSE)
  an<-as.data.frame(analysis)
  
  
  
  ff <- seewave::fund(r, f = r@samp.rate, ovlp = 50, threshold = 5, 
                      fmax = 280, ylim=c(0, 280/1000), plot = FALSE, wl = 2048)[, 2]
  an$meanfun<-mean(ff, na.rm = T)
  an$minfun<-min(ff, na.rm = T)
  an$maxfun<-max(ff, na.rm = T)
  
  
  y <- seewave::dfreq(r, f = r@samp.rate, wl = 2048, ylim=c(0, 280/1000), ovlp = 0, plot = F, threshold = 5, fftw = TRUE)[, 2]
  an$meandom <- mean(y, na.rm = TRUE)
  an$mindom <- min(y, na.rm = TRUE)
  an$maxdom <- max(y, na.rm = TRUE)
  an$dfrange <- (an$maxdom - an$mindom)
  
  
  #modulation index 
  changes <- vector()
  for(j in which(!is.na(y))){
    change <- abs(y[j] - y[j + 1])
    changes <- append(changes, change)
  }
  if(an$mindom==an$maxdom) an$modindx<-0 else an$modindx <- mean(changes, na.rm = T)/an$dfrange
  an$file<-files[i]
  stats[i,]<-an
  print(i/length(files))
}

stats$file<-gsub("../data/WaveFiles/","",stats$file)
stats$file<-gsub(".wav","",stats$file)
colnames(speakerInfo)[4]<-"file"
bigdata<- merge(stats,speakerInfo,by="file",all.x = T)

genderdf<-bigdata[,c(2:23,28)]
genderdf$sex<-as.factor(genderdf$sex)
train<-round(.75*nrow(genderdf))
train.ind<-sample(1:nrow(genderdf),train)
traindata<-genderdf[train.ind,]
testdata<-genderdf[-train.ind,]


genderSVM<-svm(sex~.,data=traindata)

#TrainTest
predictSvm <- predict(genderSVM, testdata)
table(predictSvm, testdata$sex)

SVM.tune<-tune(svm,sex~.,data=traindata,
               ranges = list(gamma = c(0,.01,.02,.03,.04), cost = 2^(-1:2)))
SVM.tune
