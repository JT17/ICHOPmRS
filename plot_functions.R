#Plot Functions
library(ggplot2)
####### Plots ######################
# Histogram function
plot_histogram <- function(data_frame,var,
                           bw=1/2,units="hours",
                           strata="max.visit.name",
                           type="color",
                           x_lab="",title="Histogram",
                           printing=TRUE,
                           hide.title = TRUE,
                           filter_negative = TRUE){
  if(x_lab==""){x_lab=var}#if label is blank name it variable name
  
  if(units=="hours"){f=24}#divide by 24 from original hours
  else{f=1}
  
  v_t = data.frame(t=data_frame[,var],strata=data_frame[,strata]) #vector of the relvent column for variable
  v_t = v_t[!is.na(v_t$t),] #filter out na's
  
  
  if(filter_negative){
    v_t = v_t[v_t$t>0,] #filter out negative times
    
  }
  
  
  if(type=="color"){
    
    
    plot = ggplot(v_t,aes(x=t/f)) + #,fill=strata)) + 
      geom_histogram(binwidth = bw, color = "grey30", fill="white")+theme(legend.position = "top") #xlim(0,NA)
    
  }else if(type=="density"){
    
    plot= ggplot(v_t,aes(x=t/f)) + 
      geom_histogram(aes(y=..density..),binwidth = bw, color = "grey30", fill = "white")+
      geom_density()+ 
      xlab(x_lab)
    
  }else if(type =="color bar"){
    
    plot = ggplot(v_t,aes(x=t/f,fill=strata)) + 
      geom_bar(position="stack", stat="identity")+theme(legend.position = "top")
  }else if(type == "simple"){
    plot = ggplot(v_t,aes(x=t/f)) + 
      geom_histogram(binwidth = bw, color = "grey30", fill = "white")#+theme(legend.position = "top") #xlim(0,NA)
  }
  
  
  if(printing){
    plot = plot +theme(axis.text=element_text(size=50, margin=margin(20,20,20,20)),
                       axis.title=element_text(size=50),
                       plot.title = element_text(size=100,face="bold",margin=margin(0,0,30,0)),
                       plot.margin = margin(2, 2, 2, 2, "cm"),
                       legend.key.size = unit(3, "cm"),
                       legend.text = element_text(size=40),
                       legend.title = element_text(size=40),
                       strip.text.x = element_text(size = 50,face="bold",margin = margin(20,20,20,20))) 
  }
  
  if(!hide.title){plot=plot + ggtitle(title) }
  
  return(  plot + xlab(x_lab)  )
  
}


graph_timeline <- function(graph_df,
                           title="",
                           x_lab="Individual Intubated Patient",
                           y_lab="",
                           legend_top=F,
                           units="minutes",
                           facets=TRUE,
                           printing=FALSE,
                           hide.title=TRUE){
  if(units=="minutes"){f=24}
  else{f=1}
  
  g = ggplot(graph_df, aes(fill=legend,y=value/f, x=id)) + 
    geom_bar(aes(),position="stack", stat="identity")+#geom_bar(aes(fill=endpoint),alpha=0.2, stat="identity")+
    coord_flip()+xlab(x_lab)+ylab(y_lab)+ 
    scale_fill_discrete(guide=guide_legend(reverse=T))
  
  if(legend_top){g=g+theme(legend.position = "top")}
  
  #https://community.rstudio.com/t/facet-specific-ordering-for-stacked-bar-chart/40491
  if(facets){g=g+facet_grid( ~ endpoint,  space  = "free", scales = "free", drop = TRUE, shrink = TRUE)}
  
  if(printing){
    g = g +theme(axis.text=element_text(size=36),
                 axis.title=element_text(size=50),
                 plot.title = element_text(size=100,face="bold",margin=margin(0,0,30,0)),
                 plot.margin = margin(2, 2, 2, 2, "cm"),
                 legend.key.size = unit(3, "cm"),
                 legend.text = element_text(size=40),
                 legend.title = element_text(size=40),
                 strip.text.x = element_text(size = 50,face="bold",margin = margin(20,20,20,20))) 
  }
  
  if(!hide.title){g=g+ggtitle(title)}
  
  
  return(g)
  
}

#Function if you are using ggsve
printing_to_ggsave <- function(plot,bw=F){
  
  if(bw){
    
    return(plot+theme_bw()+theme(axis.text=element_text(size=36),
                      axis.title=element_text(size=75,margin = margin(20,20,20,20)),
                      #plot.title = element_text(size=100,face="bold",margin=margin(0,0,30,0)),
                      plot.margin = margin(2, 2, 2, 2, "cm"),
                      legend.key.size = unit(5, "cm"),
                      legend.text = element_text(size=60),
                      legend.title = element_text(size=60),
                      #axis.title.x = element_text(size = 40),
                      #axis.title.y = element_text(size = 40),
                      strip.text.x = element_text(size = 50,face="bold",margin = margin(20,20,20,20)))) 
    
  }else{
    
    return(plot+theme(axis.text=element_text(size=36),
                      axis.title=element_text(size=75,margin = margin(20,20,20,20)),
                      #plot.title = element_text(size=100,face="bold",margin=margin(0,0,30,0)),
                      plot.margin = margin(2, 2, 2, 2, "cm"),
                      legend.key.size = unit(5, "cm"),
                      legend.text = element_text(size=60),
                      legend.title = element_text(size=60),
                      #axis.title.x = element_text(size = 40),
                      #axis.title.y = element_text(size = 40),
                      strip.text.x = element_text(size = 50,face="bold",margin = margin(20,20,20,20)))) 
    
  }

}

#Works for binary strata
# density_by_strata <- function(data,strata,strata_name){
#   
#   data[,strata_name]= paste0("no hypoxia in ED\n n = ",length(which(exact_symptom$hypoxia_ed==0)))
#   exact_symptom$hypoxia_in_ed[exact_symptom$hypoxia_ed==1]=paste0("hypoxia in ED\n n = ",length(which(exact_symptom$hypoxia_ed==1)))
#   exact_symptom_plot = ggplot(exact_symptom,aes(x=sx2ed_days,fill=hypoxia_in_ed))+geom_density(alpha=0.2,aes(y=..count..))
#   exact_symptom_plot = exact_symptom_plot + xlab("# Days from First Symptom to ED") + ggtitle("Density Plot of Days from Symptom to ED: By Hypoxia in ED\n* excluding patients symptomatic after arrival")
#   
# }


library(dplyr)
library(ggraph)
library(igraph)
#https://shiring.github.io/machine_learning/2017/03/16/rf_plot_ggraph
tree_func <- function(final_model, 
                      tree_num) {
  
  # get tree by index
  tree <- randomForest::getTree(final_model, 
                                k = tree_num, 
                                labelVar = TRUE) %>%
    tibble::rownames_to_column() %>%
    # make leaf split points to NA, so the 0s won't get plotted
    mutate(`split point` = ifelse(is.na(prediction), `split point`, NA))
  
  # prepare data frame for graph
  graph_frame <- data.frame(from = rep(tree$rowname, 2),
                            to = c(tree$`left daughter`, tree$`right daughter`))
  
  # convert to graph and delete the last node that we don't want to plot
  graph <- graph_from_data_frame(graph_frame) %>%
    delete_vertices("0")
  
  # set node labels
  V(graph)$node_label <- gsub("_", " ", as.character(tree$`split var`))
  V(graph)$leaf_label <- as.character(tree$prediction)
  V(graph)$split <- as.character(round(tree$`split point`, digits = 2))
  
  # plot
  plot <- ggraph(graph, 'dendrogram') + 
    theme_bw() +
    geom_edge_link() +
    geom_node_point() +
    geom_node_text(aes(label = node_label), na.rm = TRUE, repel = TRUE) +
    geom_node_label(aes(label = split), vjust = 2.5, na.rm = TRUE, fill = "white") +
    geom_node_label(aes(label = leaf_label, fill = leaf_label), na.rm = TRUE, 
                    repel = TRUE, colour = "white", fontface = "bold", show.legend = FALSE) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_blank(),
          plot.background = element_rect(fill = "white"),
          panel.border = element_blank(),
          axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(size = 18))
  
  print(plot)
}

#Readjust dates to zero them all out
dates_relative_to <- function(DF,base="2020-03-01"){
  #DF=data_frame
  DF$delta_base = as.numeric(as.Date(DF$ed_date)-as.Date(base))
  DF$ed_date = as.character(as.Date(DF$ed_date) - DF$delta_base)
  DF$intubation1_date = as.character(as.Date(DF$intubation1_date) - DF$delta_base)
  DF$intubation2_date[DF$intubation2_date!=""] = as.character(as.Date(DF$intubation2_date[DF$intubation2_date!=""]) - DF$delta_base[DF$intubation2_date!=""])
  #DF$extubation1_date  = as.Date(DF$extubation1_date) - DF$delta_base
  DF$extubation1_date[DF$extubation1_date!=""]  = as.character(as.Date(DF$extubation1_date[DF$extubation1_date!=""]) - DF$delta_base[DF$extubation1_date!=""])
  DF$extubation2_date[DF$extubation2_date!=""] = as.character(as.Date(DF$extubation2_date[DF$extubation2_date!=""]) - DF$delta_base[DF$extubation2_date!=""])
  DF$death_date[DF$death_date!=""] = as.character(as.Date(DF$death_date[DF$death_date!=""]) - DF$delta_base[DF$death_date!=""])
  DF$discharge_date[DF$discharge_date!=""] = as.character(as.Date(DF$discharge_date[DF$discharge_date!=""]) - DF$delta_base[DF$discharge_date!=""])
  DF$last_reviewed = as.character(as.Date(DF$last_reviewed) - DF$delta_base)
  return(DF)
}

