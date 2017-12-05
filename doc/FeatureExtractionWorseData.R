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
mp32wav(from=dir)
##Need to move them to their own directory
waves<-list.files(dir)
waves<-waves[grepl(".wav",waves)]
dir.create("../data/WaveFiles")

file.copy(paste("../data/recording",waves,sep=""),"VoiceWav")
file.remove(paste("VoiceMp3s/",waves,sep=""),"VoiceMp3s")
