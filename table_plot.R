library(grid)

# data_table<-table1_data
# strata_name<-"max.visit.level"
# strata_labels<-c("ED","Floor/SDU","ICU")
# variables<-comorbidities #column names of df that you want as rows of table
# var_labels<-c("COPD","Asthma","ILD","OSA","CKD","Hepatitis B or C","History of Cancer","Prior Transplant","Rheumatological Disease","Prior Healthcare","Coronary Artery Disease","Congestive Heart Failure","Stroke","Diabetes","Hypertension", "Baseline Mechanical Ventilation","Cirrhosis","HIV","Pregnancy","IBD","Immunosuppressed")


table_function<-function(data_table,strata_name,strata_labels,variables, var_labels, plot_label) {
strata<-data_table[,strata_name]
binary_funct<-function(binary_var) {
  count<-table(data_table[,binary_var],strata)["1",]
  percent<-c(count,sum(count))/c(table(strata),length(strata))
  mat<-t(as.matrix(c(paste0(c(count,sum(count))," (",100*round(percent,3), "%)"))))
  return(mat)
}
header<-paste0(c(strata_labels,"Overall")," (N=", c(table(strata),length(strata)),")")

df<-data.frame(matrix(ncol=4,nrow=length(variables)))
rownames(df)<-variables
colnames(df)<-header
for(i in 1:length(variables)){
  if(sum(data_table[,variables[i]])>0) {
  df[i,]<-binary_funct(variables[i])
  }
}
rownames(df)<-var_labels
return(df)


}


table_df<-table_function(data_table=data_table,
                    strata_name="max.visit.level",
                    strata_labels=c("ED","Floor/SDU","ICU"),
                    variables=comorbidities,
                    var_labels=c("COPD","Asthma","ILD","OSA","CKD","Hepatitis B or C","History of Cancer","Prior Transplant","Rheumatological Disease","Prior Healthcare","Coronary Artery Disease","Congestive Heart Failure","Stroke","Diabetes","Hypertension", "Baseline Mechanical Ventilation","Cirrhosis","HIV","Pregnancy","IBD","Immunosuppressed"))

table_plot <- function(x,theme_fun= gridExtra::ttheme_minimal,base_size = 12,base_colour = "black",base_family = "",parse = FALSE,padding = unit(c(4, 4), "mm"),col = base_colour,lwd=1,lty=1) {
  g <- gridExtra::tableGrob(x,
                            theme = theme_fun(base_size, base_colour, base_family, parse, padding))
  separators <- replicate(ncol(g) - 2,
                          grid::segmentsGrob(x1 = unit(0, "npc"), gp=gpar(col=col,lwd=lwd,lty=lty)),
                          simplify=FALSE)
  g <- gtable::gtable_add_grob(g, grobs = separators,
                               t = 2, b = nrow(g), l = seq_len(ncol(g)-2)+2)
  ggplot2::ggplot() + ggplot2::annotation_custom(g) + ggplot2::theme_void()
}

pdf(paste0(plot_label,"_plot.pdf"))
table_plot(table_df, base_family = "serif")
dev.off()


