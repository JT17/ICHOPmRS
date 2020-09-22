#Bimodal analysis
library(ggplot2)
library(diptest)
library(plyr)


rm(list = ls(all.names = TRUE)) #Clear workspace
setwd("/Users/SBruce 1/repos/CovidAnalysis/") #Set working Directory
data_dir = '/Users/SBruce 1/Desktop/RESEARCH/COVID/Analysis/data/' #DIRECTORY WHERE DATA IS STORED
data_name = 'COVID19StudyClinical_DATA_2020-04-22_1928' #GIVE THE PROPER NAME
#data_name = 'COVID19StudyClinical_DATA_2020-04-15_2235' #Paper 1 data
save_dir = "bimodal/4.22.20 data/"


#Load plotting functions
source('plot_functions.R')


data_frame = read.csv(paste0(data_dir,data_name,'_POST.csv'),stringsAsFactors=F)

#####Exact Intubators######
#Time from first symptom in daysx
exact_intubators = data_frame[data_frame$first_sx_date==1&data_frame$intubation1==1,]

exact_intubators$firstsx2intubation_days = as.numeric(difftime(as.Date(exact_intubators$intubation1_date),as.Date(exact_intubators$first_sx_date_exact), 
                                                               units = "days"))
exact_intubators$firstsx2intubation_days[which(exact_intubators$firstsx2intubation_days==0)]=0.01

#Clean up NA's for not required fields 

##### QC #####

print(paste0("Go check on these records, they have a negative sx to intubation time: ",exact_intubators$record_id[which(exact_intubators$firstsx2intubation_days<0)]))
exact_intubators = exact_intubators[which(exact_intubators$firstsx2intubation_days>=0),]

##### Plot from Paper 1 #####
sx2intubation_density_plot = plot_histogram(exact_intubators,var="firstsx2intubation_days",type="simple",
                                            x_lab="# Days from First Symptom to First Intubation",bw=1,units="days",
                                            hide.title = T,
                                            printing = F)


# ggsave(paste0(data_dir,"sx2int_plot",".pdf"),
#        sx2intubation_density_plot, width=16, height=8, units="in", scale=3,dpi=50000)
dip_symptoms = dip.test(exact_intubators$firstsx2intubation_days, simulate.p.value = FALSE, B = 2000)

##### Let's explore the distribution of the three groups #####
# death_group = exact_intubators[which(exact_intubators$death==1),]
# discharge_group = exact_intubators[which(exact_intubators$discharge==1 & exact_intubators$death!=1),]
# #hospital_group = exact_intubators[which(exact_intubators$death!=1 & exact_intubators$discharge!=1),]
# hospital_group = exact_intubators[which(!exact_intubators$record_id %in% c(death_group$record_id,discharge_group$record_id)),]
#Add endpoint
exact_intubators$endpoint = paste0("in hospital at last review \n n = ",length(which(exact_intubators$death==0 & (exact_intubators$discharge==0 | is.na(exact_intubators$discharge)))))#+length(exact_intubators$death==0))
exact_intubators$endpoint[which(exact_intubators$death==1)]=paste0("died in hospital \n n = ",length(which(exact_intubators$death==1)))
exact_intubators$endpoint[which(exact_intubators$death==0 & exact_intubators$discharge==1)]=paste0("discharged from hospital\n n = ",length(which(exact_intubators$death==0 & exact_intubators$discharge==1)))

#labels(exact_intubators$endpoint)=paste0

density_by_endpoint = ggplot(exact_intubators,aes(x=firstsx2intubation_days,fill=endpoint))+geom_density(alpha=0.2,aes(y=..count..))
density_by_endpoint = density_by_endpoint + xlab("# Days from First Symptom to First Intubation") + ggtitle("Density Plot of Days from Symptom to Intubation by Endpoint")

pdf(paste0(data_dir,save_dir,"density_by_endpoint.pdf"),width=10)
print(density_by_endpoint)
dev.off()
# ggplot()+geom_density(data = death_group,aes(x=firstsx2intubation_days),fill="red",alpha=0.2)+
#   geom_density(data = discharge_group,aes(x=firstsx2intubation_days),fill="blue",alpha=0.2)+
#   geom_density(data = hospital_group,aes(x=firstsx2intubation_days),fill="green",alpha=0.2)

#### Second set of endpoints ####
exact_intubators$endpoint_2 = "still intubated"
exact_intubators$endpoint_2[which(exact_intubators$death==1)]= paste0("died in hospital\n n = ",length(which(exact_intubators$death==1)))
exact_intubators$endpoint_2[which(exact_intubators$intubation2==1 & exact_intubators$death==0)]=paste0("Extubated and Re-Intubated (not dead)\n n = ",length(which(exact_intubators$intubation2==1 & exact_intubators$death==0)))
exact_intubators$endpoint_2[which(exact_intubators$extubation1==1 &(exact_intubators$intubation2!=1 | is.na(exact_intubators$intubation2)) & exact_intubators$death==0)] = paste0("Extubated and Alive (not re-intubated)\n n = ",length(which(exact_intubators$extubation1==1 &(exact_intubators$intubation2!=1 | is.na(exact_intubators$intubation2)) & exact_intubators$death==0)))

density_by_endpoint2 = ggplot(exact_intubators,aes(x=firstsx2intubation_days,fill=endpoint_2))+geom_density(alpha=0.2,aes(y=..count..))
density_by_endpoint2 = density_by_endpoint2 + xlab("# Days from First Symptom to First Intubation") + ggtitle("Density Plot of Days from Symptom to First Intubation by Endpoint 2")

pdf(paste0(data_dir,save_dir,"density_by_endpoint_2.pdf"))
print(density_by_endpoint2)
dev.off()

#filter out the still intubated patients
exact_intubators_with_endpoint2 = exact_intubators[exact_intubators$endpoint_2!="still intubated",]

density_by_endpoint2_truncated = ggplot(exact_intubators_with_endpoint2,aes(x=firstsx2intubation_days,fill=endpoint_2))+geom_density(alpha=0.2,aes(y=..count..))
density_by_endpoint2_truncated = density_by_endpoint2_truncated + xlab("# Days from First Symptom to First Intubation") + ggtitle("Density Plot of Days from Symptom to First Intubation by Endpoint 2\n(excluding those currently intubated for the first time)")

pdf(paste0(data_dir,save_dir,"density_by_endpoint_2_without_currentlyintubated.pdf"))
print(density_by_endpoint2_truncated)
dev.off()

###### date of intubation vs time to intubation###3#

scatter_by_date = ggplot(exact_intubators,aes(x=as.Date(ed_date),y=firstsx2intubation_days))+geom_point()+xlab("ED Arrival Date")+ylab("# Days from First Symptom to First Intubation")

scatter_by_meds = ggplot(exact_intubators,aes(x=med_count,y=firstsx2intubation_days,colour=endpoint))+geom_point()



#### hypoxia ed ########

exact_intubators$hypoxia_in_ed = paste0("no hypoxia in ED\n n = ",length(which(exact_intubators$hypoxia_ed==0)))
exact_intubators$hypoxia_in_ed[exact_intubators$complication_aki==1]=paste0("hypoxia in ed\n n = ",length(which(exact_intubators$hypoxia_ed==1)))
density_by_AKI = ggplot(exact_intubators,aes(x=firstsx2intubation_days,fill=hypoxia_in_ed))+geom_density(alpha=0.2,aes(y=..count..))
density_by_AKI = density_by_AKI + xlab("# Days from First Symptom to First Intubation") + ggtitle("Density Plot of Days from Symptom to First Intubation: Hypoxia in ED")


###### AKI ########
exact_intubators$aki = paste0("no AKI\n n = ",length(which(exact_intubators$complication_aki==0)))
exact_intubators$aki[exact_intubators$complication_aki==1]=paste0("AKI\n n = ",length(which(exact_intubators$complication_aki==1)))
density_by_AKI = ggplot(exact_intubators,aes(x=firstsx2intubation_days,fill=aki))+geom_density(alpha=0.2,aes(y=..count..))
density_by_AKI = density_by_AKI + xlab("# Days from First Symptom to First Intubation") + ggtitle("Density Plot of Days from Symptom to First Intubation: AKI vs. no AKI")


exact_intubators$hd = paste0("no in-patient dialysis\n n = ",length(which(exact_intubators$renal_replacement==0)))
exact_intubators$hd[exact_intubators$renal_replacement==1]=paste0("in-patient dialysis\n n = ",length(which(exact_intubators$renal_replacement==1)))
density_by_hd = ggplot(exact_intubators,aes(x=firstsx2intubation_days,fill=hd))+geom_density(alpha=0.2,aes(y=..count..))
density_by_hd = density_by_hd + xlab("# Days from First Symptom to First Intubation") + ggtitle("Density Plot of Days from Symptom to First Intubation: dialysis vs. no dialysis")


exact_intubators$age_group = ""
exact_intubators$age_group[exact_intubators$age>=60] = paste0("60 or older\n n = ",length(which(exact_intubators$age>=60)))
exact_intubators$age_group[exact_intubators$age<60] = paste0("younger than 60\n n = ",length(which(exact_intubators$age<60)))
density_by_age = ggplot(exact_intubators,aes(x=firstsx2intubation_days,fill=age_group))+geom_density(alpha=0.2,aes(y=..count..))
density_by_age = density_by_age + xlab("# Days from First Symptom to First Intubation") + ggtitle("Density Plot of Days from Symptom to First Intubation by age")

pulm_i = exact_intubators$copd | exact_intubators$asthma | exact_intubators$ild |exact_intubators$osa  | exact_intubators$ild|exact_intubators$other_pulm_dz
exact_intubators$pulm = ""
exact_intubators$pulm[which(pulm_i)] =  paste0("pulmonary co-morbidity\n n = ",sum(pulm_i))
exact_intubators$pulm[which(!pulm_i)] =  paste0("no pulmonary co-morbidity\n n = ",sum(!pulm_i))
density_by_pulm = ggplot(exact_intubators,aes(x=firstsx2intubation_days,fill=pulm))+geom_density(alpha=0.2,aes(y=..count..))
density_by_pulm = density_by_pulm + xlab("# Days from First Symptom to First Intubation") + ggtitle("Density Plot of Days from Symptom to First Intubation by pulmonary co-morbidity")

exact_intubators$bmi_group = ""
exact_intubators$bmi_group[exact_intubators$bmi>=40] = paste0("BMI >= 40 \n n = ",length(which(exact_intubators$bmi>=40)))
exact_intubators$bmi_group[exact_intubators$bmi>=30 & exact_intubators$bmi<40] = paste0("30 <= BMI < 40 \n n = ",length(which(exact_intubators$bmi>=30 & exact_intubators$bmi<40)))
exact_intubators$bmi_group[exact_intubators$bmi<30] = paste0("BMI < 30 \n n = ",length(which(exact_intubators$bmi<30)))
density_by_bmi = ggplot(exact_intubators,aes(x=firstsx2intubation_days,fill=bmi_group))+geom_density(alpha=0.2,aes(y=..count..))
density_by_bmi = density_by_bmi + xlab("# Days from First Symptom to First Intubation") + ggtitle("Density Plot of Days from Symptom to First Intubation by obesity")




#### Examine the tail ########
#switch it back because no more plotting down here
exact_intubators$firstsx2intubation_days[exact_intubators$firstsx2intubation_days==0.01]=0

sx2int_table = as.data.frame(table(exact_intubators$firstsx2intubation_days))
names(sx2int_table)=c("firstsx2intubation_days","N")

#sx2int_table = ddply(sx2int_table,.(firstsx2intubation_days),summarize,N=N,mean_age=mean(exact_intubators$age[exact_intubators$firstsx2intubation_days==firstsx2intubation_days]))

tail = exact_intubators[which(exact_intubators$firstsx2intubation_days>14),]

dim(tail)[1]/dim(exact_intubators)[1]
table(tail$endpoint) 
table(tail$firstsx2intubation_days,tail$record_id)
#### dip test the death ####

##### Linear regression ######

#Printing out a figure for ray#
pdf(paste0(data_dir,save_dir,"sx_to_intubation_density_plots.pdf"),width=11)
print(density_by_endpoint)
print(density_by_endpoint2_truncated)
print(density_by_age)
dev.off()
