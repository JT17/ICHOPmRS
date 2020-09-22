#Table 1
source("prepare_data.R")
source("table_helpers.R")
library(grid)
library("DescTools")
library(openxlsx)
library(table1)
library(boot)

<<<<<<< HEAD
WHO_AM_I = "SB"
=======
WHO_AM_I = "JT"
>>>>>>> 8530db8c84cbd7a947c11ece279cf905cc5e4a05
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
    data_dir="~/Documents/R/ICHOPmRS/MGA/Data/"
  }
  data_name = 'ichop_data' #GIVE THE PROPER NAME
}

char_data = read.csv(paste0(data_dir,"ichop_data_POST.csv"),stringsAsFactors = F)
field_dict = read.csv(paste0(data_dir,"field_dict.csv"),stringsAsFactors = F)
for(i in 1:nrow(char_data)) {
if(char_data$race_string[i]=="White") {
  char_data$race_string[i]<-"Non-Hispanic White"} else if(char_data$race_string[i]=="Black") {
    char_data$race_string[i]<-"Non-Hispanic Black"} else {}
}
# Create Table for Demographics in Patients Presenting to the ED - this is the problem table
library(table1)
library(boot)

version="V2"
# binaries<-char_data$dm
binary_render <- function(binaries, ci_interval = .95, ci_method = "wilson") {
  count<-table(binaries)
  if(length(count>2)) {
    percent <- count/sum(count)
    binomial_dist = as.data.frame(BinomCI(x = count, n =rep(sum(count),length(count)), conf.level = ci_interval, method=ci_method))
  } else {percent <- count[2]/sum(count)
  binomial_dist = as.data.frame(BinomCI(x = count[2], n =sum(count), conf.level = ci_interval, method=ci_method))
  }
  if(version=="V2") {
    vect <- t(as.matrix(c("",paste0(100*round(binomial_dist$est, 3), 
                                    "% (",c(count),")"))))[1,]
    names(vect)<-c("",levels(as.factor(binaries)))  
    
  } else {
  vect <- t(as.matrix(c("",paste0(c(count),  
                                  " ", 100*round(binomial_dist$est, 3), 
                                  "% [", 100*round(binomial_dist$lwr.ci,3) , " - ", 
                                  100*round(binomial_dist$upr.ci,3) , "]"))))[1,]
  names(vect)<-c("",levels(as.factor(binaries)))
  }
  
  if(sum(names(vect)%in%c("TRUE","FALSE"))==2){
    vect = vect[names(vect)=="TRUE"]
  }
  return(vect)
}


rndr <- function(x, name, ...) {
  if (!is.numeric(x)) return(binary_render(x))
  what <- switch(name,
                 Age = c("Median [Q1-Q3]"),#c("Mean (SD)","Median [Q1-Q3]"),
                 BMI  = c("Median [Q1-Q3]","NMISS"),#c("Mean (SD)","Median [Q1-Q3]","NMISS"),
                 ICH.score = c("Median [Q1-Q3]","NMISS")
                 )#c("Mean (SD)","Median [Q1-Q3]"))
  parse.abbrev.render.code(c("", what))(x)
}

char_data$sex_category<-factor(char_data$sex,labels=c("Male","Female"))
char_data$smoking_binary = ifelse(char_data$smoking=="Y",T,F)
char_data$Anti.coagulants_binary =  ifelse(char_data$Anti.coagulants=="y",T,F)
char_data$Anti.platelets_category = factor(char_data$Anti.platelets,labels=c("None","ASA","Dual","Dual","Clopidogrel","Other"))

#For now rename multiple locations
char_data$Location[grep(",",char_data$Location)]="800"
char_data$Location[is.na(char_data$Location)]<-"Missing"
char_data$Location_category = factor(char_data$Location,
                                     labels = c("Cortical","Cerebellum","Basal Ganglia","Brainstem","Thalamus","Subcortical white matter","Multiple Locations", "Missing"))
char_data$ICH.score=as.numeric(char_data$ICH.score)

#Rename multiple etiologies
char_data$etiology[grep(";",char_data$etiology)]="90000"
char_data$etiology[char_data$etiology==0]="90000"
char_data$etiology[char_data$etiology==11]="90000"
char_data$etiology_category = factor(char_data$etiology,
                                     labels = c("hypertension", "cerebral amyloid angiopathy","AVM","coagulopathy", "anticoagulation", "multiple or unknown"))

strata <- c(split(char_data, char_data$race_string), list(Overall=char_data))

SPLIT = FALSE

if(SPLIT){
  labels_1<-list(variables=list(Age="Age",sex_category="Sex",BMI="BMI",smoking_binary="smoker",
                              Afib="Afib",CHF="CHF", HLD="HLD",HTN="HTN",DM="DM",Kidney.disease="Kidney disease",prior.stroke="Prior stroke",Dementia="Dementia",
                              Anti.platelets_category="Anti-platelets",Anti.coagulants_binary="Anti-coagulation"))
  labels_2<-list(variables=list(ICH.score = "ICH score",Location_category="ICH Location",etiology_category="Etiology"))
  
  table1::table1(strata,labels_1,render=rndr ,groupspan=c(3,1), topclass="Rtable1-zebra")
  table1::table1(strata,labels_2,render=rndr ,groupspan=c(3,1), topclass="Rtable1-zebra")
  
  
  
  
}else{
  labels<-list(variables=list(Age="Age",sex_category="Sex",BMI="BMI",smoking_binary="smoker",
                              Afib="Afib",CHF="CHF", HLD="HLD",HTN="HTN",DM="DM",Kidney.disease="Kidney disease",prior.stroke="Prior stroke",Dementia="Dementia",
                              Anti.platelets_category="Anti-platelets",Anti.coagulants_binary="Anti-coagulation",
                              ICH.score = "ICH score",Location_category="ICH Location",etiology_category="Etiology")) 
  
  table1::table1(strata,labels,render=rndr ,groupspan=c(3,1), topclass="Rtable1-zebra")
  
}



