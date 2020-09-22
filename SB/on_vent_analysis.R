#Bimodal analysis
library(ggplot2)
library(diptest)
library(plyr)


#rm(list = ls(all.names = TRUE)) #Clear workspace
setwd("/Users/SBruce 1/repos/CovidAnalysis/") #Set working Directory
data_dir = '/Users/SBruce 1/Desktop/RESEARCH/COVID/Analysis/data/' #DIRECTORY WHERE DATA IS STORED
data_name = 'COVID19StudyClinical_DATA_2020-04-21_1557' #GIVE THE PROPER NAME
#data_name = 'COVID19StudyClinical_DATA_2020-04-15_2235' #Paper 1 data
save_dir = "on_vent/"


data_frame = read.csv(paste0(data_dir,data_name,'_POST.csv'),stringsAsFactors=F)

int_df = data_frame[data_frame$intubation1==1,]

#Clean up the non-required variables, make the NA's 0's
int_df$extubation1[is.na(int_df$extubation1)]=0
int_df$intubation2[is.na(int_df$intubation2)]=0
int_df$extubation2[is.na(int_df$extubation2)]=0

int_df$category = ""
int_df$int1_days = NA
int_df$int2_days = NA
#1 dead while intubated 1
death1_i = which(int_df$death==1 & int_df$extubation1==0)
int_df$category[death1_i] = "Died while intubated (first intubation)"
int_df$int1_days[death1_i]=as.numeric(as.Date(int_df$death_date[death1_i])-as.Date(int_df$intubation1_date[death1_i]))
int_df$int2_days[death1_i] = 0
#2 Extubated, not-reintubated, and alive
int_df$category[ext1_alive_i] = "Extubated and not-reintubated"
ext1_alive_i = which(int_df$extubation1==1  & int_df$intubation2==0)
int_df$int1_days[ext1_alive_i]=as.numeric(as.Date(int_df$extubation1_date[ext1_alive_i])-as.Date(int_df$intubation1_date[ext1_alive_i]))
int_df$int2_days[ext1_alive_i] = 0
#3 Extubated, re-intubated and died while intubated second time
int_df$category[death2_i] = "Died while intubated (second intubation)"
death2_i = which(int_df$intubation2==1 & int_df$death == 1 & int_df$extubation2 == 0)
int_df$int1_days[death2_i]=as.numeric(as.Date(int_df$extubation1_date[death2_i])-as.Date(int_df$intubation1_date[death2_i]))
int_df$int2_days[death2_i]=as.numeric(as.Date(int_df$death_date[death2_i])-as.Date(int_df$intubation2_date[death2_i]))
#4 extubated a second time
ext2_i = which(int_df$extubation2==1)
int_df$category[ext2_i] = "Extubated twice"
int_df$int1_days[ext2_i]=as.numeric(as.Date(int_df$extubation1_date[ext2_i])-as.Date(int_df$intubation1_date[ext2_i]))
int_df$int2_days[ext2_i]=as.numeric(as.Date(int_df$extubation2_date[ext2_i])-as.Date(int_df$intubation2_date[ext2_i]))

#OK now for the currently intubated folks at last review
#5 Still INtubated first time
still_int1_i = which(int_df$extubation1==0 & int_df$death==0)
int_df$category[still_int1_i] = "Intubated at last review (first intubation)"
int_df$int1_days[still_int1_i]=as.numeric(as.Date(int_df$last_reviewed[still_int1_i])-as.Date(int_df$intubation1_date[still_int1_i]))
int_df$int2_days[still_int1_i] = 0
#6 Still intubated second time
still_int2_i = which(int_df$intubation2==1 & int_df$extubation2==0 & int_df$death==0)
int_df$category[still_int2_i] = "Intubated at last review (second intubation)"
int_df$int1_days[still_int2_i]=as.numeric(as.Date(int_df$extubation1_date[still_int2_i])-as.Date(int_df$intubation1_date[still_int2_i]))
int_df$int2_days[still_int2_i]=as.numeric(as.Date(int_df$last_reviewed[still_int2_i])-as.Date(int_df$intubation2_date[still_int2_i]))

int_df$int_total = int_df$int1_days+int_df$int2_days

#QC
print(paste0("Go check on these records, they have a negative intubation 1 time: ",int_df$record_id[which(int_df$int1_days<0)]))
print(paste0("Go check on these records, they have a negative intubation 2 time: ",int_df$record_id[which(int_df$int2_days<0)]))

int_df=int_df[int_df$int1_days>=0 & int_df$int2_days>=0,]


#Table for ray
table = ddply(int_df,.(category),summarize,n=length(category),
              "Median Time on Invasive Mechanical Ventilator (days)"= median(int_total),
              "IQR Time on Invasive Mechanical Ventilator (days)" = IQR(int_total))

table[7,] = c("Overall",dim(int_df)[1],median(int_df$int_total),IQR(int_df$int_total))              
              # ,
              # "Second Intubation Time (median ± IQR days)"= paste0(median(int2_days)," ± ",IQR(int_2_days)),
              # "Total Intubation Time (median ± IQR days)"= paste0(median(int_total)," ± ",IQR(int_total)))

#write.csv(table,paste0(data_dir,save_dir,"on_vent_medians_4_21_data.csv"))

#Hack of some join
joined = join(exact_intubators,int_df,by="record_id")
ggplot(joined[,c("firstsx2intubation_days","int_total")],aes(x=firstsx2intubation_days,y=int_total))+geom_point()

