#######
library(ggplot2)
library(diptest)
library(plyr)


rm(list = ls(all.names = TRUE)) #Clear workspace
setwd("/Users/SBruce 1/repos/CovidAnalysis/") #Set working Directory
data_dir = '/Users/SBruce 1/Desktop/RESEARCH/COVID/Analysis/data/' #DIRECTORY WHERE DATA IS STORED
data_name = 'COVID19StudyClinical_DATA_2020-04-21_1557' #GIVE THE PROPER NAME

#Load plotting functions
source('plot_functions.R')


data_frame = read.csv(paste0(data_dir,data_name,'_POST.csv'),stringsAsFactors=F)

treatment_frame = ddply(data_frame,.(ed_date),summarize,N=length(ed_date),hqc=sum(plaq,na.rm=T))

#hqc = data_frame[which(data_frame$plaq==1),]
#hqc = hqc[as.Date(hqc$plaq_date)>=as.Date()]
plaq_plot = ggplot(treatment_frame,aes(x=as.Date(ed_date),y=hqc/N))+geom_bar()+xlim(as.Date("2020-03-01"),NA)