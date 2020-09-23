#Initialize data
library(ordinal)
library(ggplot2)
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

set.seed(123)
data_frame_raw = read.csv(paste0(data_dir,data_name,'_POST.csv'),stringsAsFactors=F)
mapping <- read.csv(paste0(data_dir,"/field_dict.csv"),row.names=2)
mapping$NewFields <- rownames(mapping)
mapping$Local <- F

#Pre-processing
comorbs = as.character(mapping$Fields[which(mapping$Tag == "Comorbidity")])
meds = as.character(mapping$Fields[which(mapping$Tag == "Medication")])
location = as.character(mapping$Fields[which(mapping$Tag == "Location")])
data_frame_raw[,comorbs] = ifelse(data_frame_raw[,comorbs]==TRUE,1,0)
data_frame_raw[,"Anti.coagulants_binary"] = ifelse(data_frame_raw[,"Anti.coagulants_binary"] == TRUE, 1,0)
data_frame_raw[,"smoking_binary"] = ifelse(data_frame_raw[,"smoking_binary"] == TRUE, 1,0)
behaviors = as.character(mapping$Fields[which(mapping$Tag == "Behavior")])
data_frame_raw$black_hispanic = as.numeric(data_frame_raw$black | data_frame_raw$hispanic)

#Names
names(data_frame_raw)[names(data_frame_raw)=="black"]="non_hispanic_black"

#Setting up the iterative models
iter_names = c("race",
               "race + baseline mrs",
               "race + baseline mrs + discharge mrs",
               "race + baseline mrs + discharge mrs + age + sex", 
               "race + baseline mrs + discharge mrs + age + sex + health comorbidities",
               "race + baseline mrs + discharge mrs + age + sex + health comorbidities + health habits")

outcome = "mrs_3months_binary"
model_vars =  list(c("non_hispanic_black", "hispanic"))
#model_vars = list(c("black_hispanic"))
model_vars[[2]] = c(model_vars[[1]], "pre_mrs_binary")
model_vars[[3]] = c(model_vars[[2]], "pre_mrs_binary","mrs_discharge_binary")
model_vars[[4]] = c(model_vars[[3]], "Age", "sex")
model_vars[[5]] = c(model_vars[[4]], comorbs, meds, location)
#Commenting out behaviors for now because we'll need to figure out how we want to filter
model_vars[[6]] = c(model_vars[[5]], "BMI","smoking")
models=list()
lm = FALSE
for(i in 1:length(model_vars)){
  model_equation = paste(outcome,paste(model_vars[[i]],collapse = " + "),sep=" ~ ")
  models[[i]]=glm(formula = model_equation, data=data_frame_raw,family = "binomial")
  if (lm == TRUE){
    models[[i]]=lm(formula = model_equation, data=data_frame_raw)
  }
  if(i == 6){
    no_missing_df = na.omit(data_frame_raw[,c(model_vars[[i]], outcome)])
    models[[i]] = glm(formula = model_equation, data = no_missing_df, family="binomial")
  }
}

#ORDINAL
# outcome_ordinal = "mRS.3.months_factor"
# data_frame_raw$mRS.3.months_factor=as.factor(data_frame_raw$mRS.3.months)
# models_ordinal=list()
# for(i in 1:length(model_vars)){
#   model_equation = paste(outcome_ordinal,paste(model_vars[[i]],collapse = " + "),sep=" ~ ")
#   models_ordinal[[i]]=clm(model_equation,data = data_frame_raw, Hess=T)
# }

#model_list: list of models 
#var_names: variables to select form each model to plot
#iter_names: names of each iteration
plot_iterative_models <- function(model_list, var_names, iter_names, 
                                  upper_bound=10,print_p=FALSE,lm=FALSE){
  n_models = length(model_list)
  model_summary = data.frame()
  for(i in 1:n_models){
    model_summary_i = as.data.frame(coef(summary(model_list[[i]])))[var_names,]
    colnames(model_summary_i) = c("est", "se", "z", "p")
    #ranked_model_summary = head(model_summary[order(model_summary$p),],num_vals)
    ci = exp(confint(model_list[[i]]))
    merged_df = merge(model_summary_i, ci, by=0)
    colnames(merged_df) = c("name", "est", "se", "z", "p", "ll", "ul")
    merged_df = merged_df[c("name", "est", "p", "ll", "ul")]
    merged_df$est = exp(merged_df$est)
    merged_df$i_name = iter_names[i]
    
    model_summary = rbind(model_summary,merged_df)
  }

  plot_data = model_summary[,c("est", "ll", "ul", "name","i_name","p")]
  if(print_p){
    plot_data$name = as.factor(paste0(plot_data$name,", p = ",signif(plot_data$p,digits=3)))
  }else{plot_data$name = as.factor(plot_data$name)}
  print (plot_data)
  #plot_data$name = factor(plot_data$name,levels = plot_data$name[order(-plot_data$p)])
  
  plot_data$i_name = factor(plot_data$i_name,levels = rev(iter_names))
  plot_data$plot_name = factor(paste(plot_data$i_name," (",plot_data$name,")"),levels = rev(paste(plot_data$i_name," (",plot_data$name,")")) ) 
  
  p = ggplot(data=plot_data,
             aes(x = plot_name#rev(paste(i_name," (",name,")"))
                 ,y = est, ymin = ll, ymax = ul, colour=name ))+
    geom_pointrange()+
    geom_hline(yintercept =1, linetype=2)+
    xlab("Model") + ylab("Risk Ratio (95% Confidence Interval)") +
    #scale_x_discrete(labels=y_names) +
    theme_bw()+
    coord_flip()+ggtitle("Logistical Regression Model of mRS at 3 Months for ICHOP Patients")
  return(p)
}

plot = plot_iterative_models(model_list = models, 
                      var_names = c("non_hispanic_black","hispanic"), 
                      iter_names = iter_names)

# pdf(paste(save_dir,"ichop_binary_iter_plot.pdf"))
# plot(plot)
# dev.off()