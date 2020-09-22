#Zip code analysis
library(sf)
library(tigris)
library(ggplot2)

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

#zip_table = read.csv(paste0(data_dir,'zip_code_count.csv'),stringsAsFactors=F)
#names(zip_table)[c(2,3)]=c("fraction_under_poverty_line","patient_count")
df = read.csv(paste0(data_dir,"ichop_data_POST.csv"),stringsAsFactors = F)
zip_table = table(df$zip)
names(zip_table)=c("zip","patient_count")

z_sf_ny <- zctas(cb = TRUE, starts_with = c("10", "11"), class = "sf")

z_sub = z_sf_ny[z_sf_ny$ZCTA5CE10 %in% zip_table$zip,]
z_merge = merge(z_sub,zip_table,by.x="ZCTA5CE10",by.y="zip",type="left")

all_zip_plot = ggplot(z_merge) + geom_sf(aes(fill=patient_count))+scale_fill_distiller(palette = "Reds", direction = 1)

z_sub = z_sf_ny[z_sf_ny$ZCTA5CE10 %in% zip_table$zip[zip_table$patient_count>5],]
z_merge = merge(z_sub,zip_table,by.x="ZCTA5CE10",by.y="zip",type="left")

over5_zip_plot_count = ggplot(z_merge) + geom_sf(aes(fill=patient_count))+scale_fill_distiller(palette = "Reds", direction = 1)
over5_zip_plot_poverty = ggplot(z_merge) + geom_sf(aes(fill=fraction_under_poverty_line))+scale_fill_distiller(palette = "Reds", direction = 1)

pdf(paste0(save_dir,"Over_5_maps.pdf"))
print(over5_zip_plot_count+ggtitle("Patient Count Heatmap: New York Area Zip Codes with > 5 Patients")+theme(axis.text=element_blank(),axis.ticks=element_blank()))
print(over5_zip_plot_poverty+ggtitle("Poverty Heatmap: New York Area Zip Codes with > 5 Patients")+theme(axis.text=element_blank(),axis.ticks=element_blank()))
print(over5_zip_plot_poverty+ggtitle("Poverty Heatmap with Superimposed Patient Counts:\nNew York Area Zip Codes with > 5 Patients")+geom_sf_label(aes(label = patient_count))+theme(axis.text=element_blank(),axis.ticks=element_blank()))
dev.off()
