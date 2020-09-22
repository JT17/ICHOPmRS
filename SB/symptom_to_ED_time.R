#Median Time
#For cards
rm(list = ls(all.names = TRUE)) #Clear workspace
setwd("/Users/SBruce 1/repos/CovidAnalysis/") #Set working Directory
data_dir = '/Users/SBruce 1/Desktop/RESEARCH/COVID/Analysis/data/' #DIRECTORY WHER
data_name = 'COVID19StudyClinical_DATA_2020-04-29_2013' #GIVE THE PROPER NAME
save_dir = "model_plots/distributions/4_29_data/"
if(!dir.exists(paste0(data_dir,save_dir))){dir.create(paste0(data_dir,save_dir))}

source('plot_functions.R')

data_frame = read.csv(paste0(data_dir,data_name,'_POST.csv'),stringsAsFactors=F)

#exact_symptom = data_frame[which(data_frame$first_sx_date==1),]

#exact_symptom$sx2ed_days = as.numeric(as.Date(exact_symptom$ed_date)-as.Date(exact_symptom$first_sx_date_exact))
exact_symptom = data_frame[which(!is.na(data_frame$sx2ed_days)),]

#QC
qc = exact_symptom[exact_symptom$sx2ed_days<0,][,c("record_id","sx2ed_days","review_notes")]
positives = exact_symptom[exact_symptom$sx2ed_days>=0,]
#times = data.frame()
times = data.frame(all_data = c(dim(exact_symptom)[1],median(exact_symptom$sx2ed_days),IQR(exact_symptom$sx2ed_days),mean(exact_symptom$sx2ed_days),sd(exact_symptom$sx2ed_days),min(exact_symptom$sx2ed_days),max(exact_symptom$sx2ed_days)),
                   no_negatives = c(dim(positives)[1],median(positives$sx2ed_days),IQR(positives$sx2ed_days),mean(positives$sx2ed_days),sd(positives$sx2ed_days),min(positives$sx2ed_days),max(positives$sx2ed_days)))

row.names(times)=c("N","Median","IQR","Mean","SD","Min","Max")

#Write the table
write.csv(times,paste0(data_dir,save_dir,"first_symptomt_to_ED.csv"))

#Plot the histogram
sx2ed_plot = plot_histogram(exact_symptom,var="sx2ed_days",type="simple",
                                            x_lab="# Days from First Symptom to ED\n(negative times indicate symptom onset after ED arrival)",bw=1,units="days",
                            hide.title = F,printing = F, filter_negative = F,
                            title = paste0("N = ",dim(exact_symptom)[1]))



pdf(paste0(data_dir,save_dir,"sx2ed_plot",".pdf"))
print(sx2ed_plot)
dev.off()
# ggsave(paste0(data_dir,save_dir,"sx2ed_plot",".pdf"),
#        sx2ed_plot, width=16, height=8, units="in", scale=3,dpi=50000)

exact_symptom$sx2ed_days[exact_symptom$sx2ed_days==0]=0.01
sx2ed_plot_nn = plot_histogram(exact_symptom,var="sx2ed_days",type="simple",
                            x_lab="# Days from First Symptom to ED",bw=1,units="days",
                            title = paste0("N = ",dim(positives)[1],"* excluding patients symptomatic after arrival"),
                            hide.title = F,printing = F, filter_negative = T)



pdf(paste0(data_dir,save_dir,"sx2ed_no_neg_plot",".pdf"))
print(sx2ed_plot_nn)
dev.off()

exact_symptom = exact_symptom[exact_symptom$sx2ed_days>=0,]

exact_symptom$hypoxia_in_ed = paste0("no hypoxia in ED\n n = ",length(which(exact_symptom$hypoxia_ed==0)))
exact_symptom$hypoxia_in_ed[exact_symptom$hypoxia_ed==1]=paste0("hypoxia in ED\n n = ",length(which(exact_symptom$hypoxia_ed==1)))
exact_symptom_plot = ggplot(exact_symptom,aes(x=sx2ed_days,fill=hypoxia_in_ed))+geom_density(alpha=0.2,aes(y=..count..))
exact_symptom_plot = exact_symptom_plot + xlab("# Days from First Symptom to ED") + ggtitle("Density Plot of Days from Symptom to ED: By Hypoxia in ED\n* excluding patients symptomatic after arrival")

pdf(paste0(data_dir,save_dir,"sx2ed_by_hypoxia",".pdf"),width=11)
print(exact_symptom_plot)
dev.off()



