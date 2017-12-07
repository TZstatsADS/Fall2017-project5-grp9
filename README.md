# ADS Project 5: 
## Speaker demographic prediction from mp3 voice clips

Term: Fall 2017

+ Team # 9
+ Projec title: "So where ya'll from?" -- Speaker demographic prediction from mp3 voice clips
+ Team members
	+ Wyatt Cole Thompson (wct2112)
	+ Saaya Yasuda (sy2569)
+ Project summary: For this project, we analyzed audio clips in R, extracted features from mp3 files, and predicted the demographics (i.e. age, country, and sex) of the speaker in the file.

**Contribution statement**: ([Please see the file here.](doc/a_note_on_contributions.md)) All team members contributed equally in all stages of this project. All team members approve our work presented in this GitHub repository including this contributions statement. 

Following [suggestions](http://nicercode.github.io/blog/2013-04-05-projects/) by [RICH FITZJOHN](http://nicercode.github.io/about/#Team) (@richfitz). This folder is orgarnized as follows.


### Note
Data is large, so they are not in the data folder.

The main dataset we used to extract features is: 
+ Speech Accent Archive - Parallel English speech samples from 177 countries
	+ https://www.kaggle.com/rtatman/speech-accent-archive/data

The initial dataset we looked into is:
+ Common Voice - 500 hours of speech recordings, with speaker demographics (by Mozilla)
	+ https://www.kaggle.com/mozillaorg/common-voice
	+ This data was large enough, but the file structure in Kaggle didn't match the csv file, and csv file had many missing labels.


```
proj/
├── lib/
├── data/
├── doc/
├── figs/
└── output/
```

Please see each subfolder for a README file.
