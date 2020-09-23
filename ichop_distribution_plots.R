##Histogram##
#setwd("~/Documents/R/ICHOPmRS/MGA")

#Table 1

library(grid)

#data_dir="Data/"

WHO_AM_I = "JT"
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
source("prepare_data.R")
source("table_helpers.R")

char_data = read.csv(paste0(data_dir,"ichop_data_POST.csv"),stringsAsFactors = F)
for(i in 1:nrow(char_data)) {
  if(char_data$race_string[i]=="White") {
    char_data$race_string[i]<-"Non-Hispanic White"} else if(char_data$race_string[i]=="Black") {
      char_data$race_string[i]<-"Non-Hispanic Black"} else {}
}
field_dict = read.csv(paste0(data_dir,"field_dict.csv"),stringsAsFactors = F)

viol_data<-char_data[,c("pre.mRS..admission.","mRS.at.discharge","mRS.3.months", "race_string")]

viol_data[,1]<-as.numeric(viol_data[,1])
viol_data[,2]<-as.numeric(viol_data[,2])
viol_data[,3]<-as.numeric(viol_data[,3])
viol_data[,4]<-as.factor(viol_data[,4])
viol_data$discharge_norm<-viol_data$mRS.at.discharge-viol_data$pre.mRS..admission.
viol_data$three_month_norm<-viol_data$mRS.3.months-viol_data$pre.mRS..admission.
viol_data$discharge_net<-viol_data$three_month_norm-viol_data$discharge_norm

viol_data<-viol_data[complete.cases(viol_data),]

# library(ggplot2)
# p <- ggplot(viol_data, aes(x=race_string, y=pre.mRS..admission., color=race_string)) + 
#   geom_violin()
# p <- p + stat_summary(fun.y=median, geom="point", size=2, color="red")
# p <- p + geom_dotplot(binaxis='y', stackdir='center', dotsize=.1)
# p <- p + scale_color_brewer(palette="Dark2")
# p <- p + ylab("mRS at Admission")
# p <- p + xlab("Race") 
# p <- p + ggtitle("Distribution of mRS at Admission by Race")
# p <- p + labs(color = "Race")
# p

library(ggplot2)
p <- ggplot(viol_data, aes(x=race_string, y=viol_data$mRS.at.discharge, color=race_string)) + 
  geom_violin()
p <- p + stat_summary(fun.y=median, geom="point", size=2, color="red")
p <- p + geom_dotplot(binaxis='y', stackdir='center', dotsize=.1)
p <- p + scale_color_brewer(palette="Dark2")
p <- p + ylab("mRS at Discharge")
p <- p + xlab("Race") 
p <- p + ggtitle("Distribution of mRS at Discharge by Race")
p <- p + labs(color = "Race")
p


library(ggplot2)
p <- ggplot(viol_data, aes(x=race_string, y=viol_data$mRS.3.months, color=race_string)) + 
  geom_violin()
p <- p + stat_summary(fun.y=median, geom="point", size=2, color="red")
p <- p + geom_dotplot(binaxis='y', stackdir='center', dotsize=.1)
p <- p + scale_color_brewer(palette="Dark2")
p <- p + ylab("mRS at 3 Months post Discharge")
p <- p + xlab("Race") 
p <- p + ggtitle("Distribution of mRS at 3 Months post Discharge by Race")
p <- p + labs(color = "Race")
p

# library(ggplot2)
# p <- ggplot(viol_data, aes(x=race_string, y=discharge_net, color=race_string)) + 
#   geom_violin()
# p <- p + stat_summary(fun.y=median, geom="point", size=2, color="red")
# p <- p + geom_dotplot(binaxis='y', stackdir='center', dotsize=.1)
# p <- p + scale_color_brewer(palette="Dark2")
# p <- p + ylab("change in mRS after three months post-discharge")
# p <- p + xlab("Race") 
# p <- p + ggtitle("Distribution of change in mRS after three months post-discharge")
# p <- p + labs(color = "Race")
# p

