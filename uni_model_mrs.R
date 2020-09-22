##Initialize data
library(ordinal)
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

source('risk_model/model_functions.R')
set.seed(123)
data_frame_raw = read.csv(paste0(data_dir,data_name,'_POST.csv'),stringsAsFactors=F)



#model_1_vars = c("black", "hispanic", "Age", "sex")
model_1_vars = c("black_hispanic", "Age", "sex")
outcome = "mrs_3_months_factor"

data_frame_raw$mrs_3_months_factor = as.factor(data_frame_raw$mRS.3.months)
data_frame_raw$black_hispanic = as.numeric(data_frame_raw$black | data_frame_raw$hispanic)

data_frame_raw$mrs_3months_delta_factor = as.factor(data_frame_raw$mrs_3months_delta)
data_frame_raw$mrs_3months_minus_discharge_factor = as.factor(data_frame_raw$mrs_3months_minus_discharge)


univariate_clm_variables = c("black_hispanic")

univariate_clm_equation = paste("mrs_3months_minus_discharge_factor",univariate_clm_variables,sep=" ~ ")
univariate_model = clm(univariate_clm_equation, data = data_frame_raw, Hess=T)


pdf(paste(save_dir,"uni_odds_ratio.pdf"))
plot_logit(univariate_model, "Univariate Black/Hispanic", "Race")+theme_bw()
dev.off()
