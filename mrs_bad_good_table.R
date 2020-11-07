#Initialize data
library(ordinal)
library(ggplot2)
WHO_AM_I = "SB"
if(!exists("MASTER_WRAPPER")){
  rm(list = setdiff(ls(), c("WHO_AM_I")))
  if(WHO_AM_I=="SB"){
    setwd("/Users/SBruce 1/repos/research/ICHOPmRS/") #Set working Directory
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
source('table_helpers.R')

set.seed(123)
data_frame_raw = read.csv(paste0(data_dir,data_name,'_POST.csv'),stringsAsFactors=F)
mapping <- read.csv(paste0(data_dir,"/field_dict.csv"),row.names=2)
mapping$NewFields <- rownames(mapping)
mapping$Local <- F

#data_frame_raw$mrs_delta_3mo_discharge_binary = data_frame_raw$mrs_3months_binary - data_frame_raw$mrs_discharge_binary
#0 = good -> good, 1 = good -> bad, 2 = bad -> good, 3 = bad -> bad
data_frame_raw$mrs_good_bad = 0
data_frame_raw$mrs_good_bad[data_frame_raw$mrs_discharge_binary==0 & data_frame_raw$mrs_3months_binary==0]=0
data_frame_raw$mrs_good_bad[data_frame_raw$mrs_discharge_binary==0 & data_frame_raw$mrs_3months_binary==1]=1
data_frame_raw$mrs_good_bad[data_frame_raw$mrs_discharge_binary==1 & data_frame_raw$mrs_3months_binary==0]=2
data_frame_raw$mrs_good_bad[data_frame_raw$mrs_discharge_binary==1 & data_frame_raw$mrs_3months_binary==1]=3

#data_frame_raw$mrs_got_worse = data_frame_raw$mrs_delta_3mo_discharge_binary == 1

df = table(data_frame_raw$mrs_good_bad,data_frame_raw$race_string)

# table_df<-table_function(data_table=df,
#                          strata_name="Race",
#                          strata_labels=c("ED","Floor","ICU"),
#                          variables=status,
#                          var_labels=status.labels)
