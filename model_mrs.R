#Initialize data
library(ordinal)
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

source('risk_model/model_functions.R')
library(ordinal)
set.seed(123)
data_frame_raw = read.csv(paste0(data_dir,data_name,'_POST.csv'),stringsAsFactors=F)
mapping <- read.csv(paste0(data_dir,"/field_dict.csv"),row.names=2)
mapping$NewFields <- rownames(mapping)
mapping$Local <- F



#model_1_vars = c("black", "hispanic", "Age", "sex")
model_1_vars = c("black_hispanic", "Age", "sex")
#outcome = "mrs_3_months_factor"
outcome = "mrs_3months_minus_discharge_factor"


data_frame_raw$mrs_3_months_factor = as.factor(data_frame_raw$mRS.3.months)
data_frame_raw$black_hispanic = as.numeric(data_frame_raw$black | data_frame_raw$hispanic)
data_frame_raw$mrs_3months_delta_factor = as.factor(data_frame_raw$mrs_3months_delta)
data_frame_raw$mrs_3months_minus_discharge_factor = as.factor(data_frame_raw$mrs_3months_minus_discharge)

univariate_clm_variables = c("black_hispanic")
univariate_clm_equation = paste(outcome,paste(univariate_clm_variables,collapse = " + "),sep=" ~ ")
univariate_model = clm(univariate_clm_equation, data = data_frame_raw, Hess=T)
plot_logit(univariate_model, "Univariate Black/Hispanic", "Race", vars=univariate_clm_variables)

demographic_vars = c( "Age", "black_hispanic", "sex")
demographic_names = c("Age", "Black or Hispanic", "Sex")
demographic_equation = paste(outcome,paste(demographic_vars,collapse = " + "),sep=" ~ ")
demographic_model = clm(demographic_equation, data = data_frame_raw, Hess=T)
plot_logit(demographic_model,
           "Multivariate Demographics", 
           "", 
           vars = demographic_vars)

comorbs = as.character(mapping$Fields[which(mapping$Tag == "Comorbidity")])
meds = as.character(mapping$Fields[which(mapping$Tag == "Medication")])
location = as.character(mapping$Fields[which(mapping$Tag == "Location")])
data_frame_raw[,comorbs] = ifelse(data_frame_raw[,comorbs]==TRUE,1,0)
data_frame_raw[,"Anti.coagulants_binary"] = ifelse(data_frame_raw[,"Anti.coagulants_binary"] == TRUE, 1,0)
data_frame_raw[,"smoking_binary"] = ifelse(data_frame_raw[,"smoking_binary"] == TRUE, 1,0)

health_vars = c(demographic_vars, comorbs, meds, location)
health_names = c(demographic_names,
                 as.character(mapping$English[which(mapping$Tag == "Comorbidity")]), 
                 as.character(mapping$English[which(mapping$Tag == "Medication")]), 
                 as.character(mapping$English[which(mapping$Tag == "Location")]))
health_equation = paste(outcome, paste(health_vars, collapse=' + '), sep=" ~")
health_model = clm(health_equation, data = data_frame_raw, Hess=T)
plot_logit(health_model,
           "Multivariate Health",
           "",
           vars = health_vars,
           num_vals = length(health_vars))

behaviors = as.character(mapping$Fields[which(mapping$Tag == "Behavior")])
behavioral_vars = c(health_vars, behaviors )
behavior_names = c(health_names, 
                   as.character(mapping$English[which(mapping$Tag == "Behavior")]))

behavior_data = data_frame_raw[complete.cases(data_frame_raw[,behaviors]),]
behavior_equation = paste(outcome, paste(behavioral_vars, collapse=' + '), sep=" ~")
behavior_model = clm(behavior_equation, data=behavior_data, Hess=T)
plot_logit(behavior_model,
           "Multivariate Behaviors",
           "",
           vars = behavioral_vars,
           num_vals = length(behavioral_vars))
