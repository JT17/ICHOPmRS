#PreProc Script
#Last updated 4/4/20
#SB MESSAGE!
library(stringr)
library(plyr)
library(dplyr)
library(openxlsx)

#Read in the data
WHO_AM_I = "SB"
if(!exists("MASTER_WRAPPER")){
  rm(list = setdiff(ls(), c("WHO_AM_I")))
  if(WHO_AM_I=="SB"){
    setwd("/Users/SBruce 1/repos/ICHOPmRS/") #Set working Directory
    data_dir="/Users/SBruce 1/Desktop/RESEARCH/iCHOP Stroke/data/"
    save_dir="/Users/SBruce 1/Desktop/RESEARCH/iCHOP Stroke/tables/" 
  }else if(WHO_AM_I=="JT"){
    setwd("/Users/JonathanTiao/source/ICHOPmRS/")
    data_dir = '/Users/JonathanTiao/source/ICHOPmRS/data/'
    save_dir = '/Users/JonathanTiao/source/ICHOPmRS/tables/'
  }
  else if(WHO_AM_I == 'MGA'){
    
  }
  data_name = 'ichop_data' #GIVE THE PROPER NAME
}

df_orig = read.xlsx(paste0(data_dir,data_name, ".xlsx"))
############ FILTER EMPTIES / Incompletes  ####################

#omit anything without premrs, mrs at discharge, mrs at 3months, ICH score
df = df_orig[complete.cases(df_orig[,c(5:7,10)]),]

#white, black, hispanic only
df = df[df$`Race/ethnicity` == '0' | df$`Race/ethnicity`== '1' | df$`Race/ethnicity` == '3',]

#add string for race / ethnicity
df$race_string = ""
df$race_string[df$`Race/ethnicity`==0]="White"
df$race_string[df$`Race/ethnicity`==1]="Black"
df$race_string[df$`Race/ethnicity`==3]="Hispanic"

#add sex string
#data$Anti.platelets_string = ""
df$white = ifelse(df$`Race/ethnicity`==0,1,0) 
df$black = ifelse(df$`Race/ethnicity`==1,1,0) 
df$hispanic = ifelse(df$`Race/ethnicity`==3,1,0) 

#Rename sex
df$sex[df$sex==1]=0
df$sex[df$sex==2]=1

#categories
df$pre_mrs_binary = ifelse(df$`pre-mRS.(admission)`>2,1,0)
df$mrs_discharge_binary = ifelse(df$mRS.at.discharge>2,1,0)
df$mrs_3months_binary = ifelse(df$mRS.3.months>2,1,0)

df$mrs_discharge_delta = as.numeric(df$mRS.at.discharge) - as.numeric(df$`pre-mRS.(admission)`)
df$mrs_3months_delta = as.numeric(df$mRS.3.months)- as.numeric(df$`pre-mRS.(admission)`)

df$mrs_3months_minus_discharge = as.numeric(df$mRS.3.months)- as.numeric(df$mRS.at.discharge)

#More pre processing
df$sex_category<-factor(df$sex,labels=c("Male","Female"))
df$smoking_binary = ifelse(df$smoking=="Y",T,F) 
df$Anti.coagulants_binary =  ifelse(df$`Anti-coagulants`=="y",T,F) 
df$Anti.platelets_category = factor(df$`Anti-platelets`,labels=c("None","ASA","Dual","Dual","Clopidogrel","Other"))
df$Anti.platelets_binary = ifelse(df$Anti.platelets_category=="None",0,1)

#For now rename multiple locations

df$cortical = ifelse(grepl("1",df$Location),1,0)
df$cerebellum = ifelse(grepl("4",df$Location),1,0) 
df$basal_ganglia = ifelse(grepl("2",df$Location),1,0) 
df$Brainstem= ifelse(grepl("5",df$Location),1,0) 
df$Thalamus= ifelse(grepl("3",df$Location),1,0) 
df$subcortical_white_matter = ifelse(grepl("6",df$Location),1,0) 

df$Location[grep(",",df$Location)]="800"
df$Location_category = factor(df$Location,
                              labels = c("Cortical","Basal Ganglia","Thalamus","Cerebellum","Brainstem","Subcortical white matter","Multiple Locations"))

df$ICH.score=as.numeric(df$ICH.score)

#Rename multiple etiologies
df$etiology[grep(";",df$etiology)]="90000"
df$etiology[df$etiology==0]="90000"
df$etiology[df$etiology==11]="90000"
df$etiology_category = factor(df$etiology,
                                     labels = c("hypertension", "cerebral amyloid angiopathy","AVM","coagulopathy", "anticoagulation", "multiple or unknown"))


######## OUTPUT #################
write.csv(x = df,file = paste0(data_dir,data_name,"_POST.csv"),row.names=F)