library(ggplot2)
library(DescTools)
# library(htmlTable)
library(grid)
# remotes::install_github("emwozniak/Table1")
# library(Table1)
table_function <- function(data_table,strata_name,strata_labels,variables, var_labels, plot_label) {
  strata <- data_table[,strata_name]
  binary_funct <- function(binary_var, with_ci = TRUE, ci_interval = .95, ci_method = "wilson") {
    count<-table(data_table[,binary_var],strata)["1",]
    percent <- c(count,sum(count))/c(table(strata),length(strata))
    if(with_ci == TRUE){
      binomial_dist = as.data.frame(BinomCI(c(count,sum(count)), c(table(strata), length(strata)) , conf.level = ci_interval, method=ci_method))
      overall = sum(count)/length(strata)
      mat <- t(as.matrix(paste0(c(count, sum(count)),  
                                    "\n", 100*round(binomial_dist$est, 3),"%", 
                                  " [", 100*round(binomial_dist$lwr.ci,3) , " - ", 
                                  100*round(binomial_dist$upr.ci,3) , "]")))

      return (mat)
    }
    mat <- t(as.matrix(c(paste0(c(count,sum(count))," (",100*round(percent,3), "%)"))))
    return(mat)
  }
  header <- paste0(c(strata_labels,"Overall"),"\n (n=", c(table(strata),length(strata)),")")
  df <- data.frame(matrix(ncol=(length(unique(strata[!is.na(strata)] ))+1),nrow=length(variables)))
  rownames(df)<-var_labels
  colnames(df)<-header
  for(i in 1:length(variables)) {
    if(length(table(data_table[,variables[i]]))>1) {
      df[i,]<-binary_funct(variables[i])
    } else {
      df[i,]<-NA
    }
  }
  return(df)
}
              
table_plot <- function(x,title_text = "", 
                       title_font_size=20,
                       theme_fun= gridExtra::ttheme_minimal,
                       base_size = 12,
                       base_colour = "black",
                       base_family = "",
                       parse = FALSE,
                       padding = unit(c(4, 4), "mm"),
                       height_padding=unit(5,"mm"), 
                       col = base_colour,
                       lwd=1,
                       lty=1,
                       print=T) {
  g <- gridExtra::tableGrob(x,
                            theme = theme_fun(base_size, base_colour, base_family, parse, padding
                                              #, core=list(bg_params=list(fill=c("gray", "gray"))) #this looks awful
                                                        ))
  separators <- replicate(ncol(g) - 2,
                          grid::segmentsGrob(x1 = unit(0, "npc"), gp=gpar(col=col,lwd=lwd,lty=lty)),
                          simplify=FALSE)
  title_grob = textGrob(title_text,gp=gpar(fontsize=title_font_size, fontfamily=base_family))
  g <- gtable::gtable_add_rows(g,heights=grobHeight(title_grob) + height_padding, pos=0)
  g = gtable::gtable_add_grob(g, title_grob, 1, 1,1,ncol(g))
  g = gtable::gtable_add_grob(g, grobs = separators,
                              t = 2, b = nrow(g), l = seq_len(ncol(g)-2)+2)
  if(print){print(ggplot2::ggplot() + ggplot2::annotation_custom(g) + ggplot2::theme_void())
    }else{return(ggplot2::ggplot() + ggplot2::annotation_custom(g) + ggplot2::theme_void())}
}

#given df_in, generate outcomes for each outcome_list with outcome_label, specificy column indices of interest per outcome
get_multiple_outcomes <- function(df_in, outcomes_list, outcomes_labels, indices, rows_in, row_labels) {
  
  output_df <- data.frame(matrix(ncol = length(outcomes_list), nrow = length(rows_in)))
  header <- character(length = length(outcomes_list))
  for(i in 1:length(outcomes_list)){
    table_df <- table_function(data_table=df_in,
                               strata_name=outcomes_list[i],
                               strata_labels=c(paste("No", outcomes_labels[i]), outcomes_labels[i]),
                               variables=rows_in,
                               var_labels=row_labels)
    output_df[,i] = table_df[,indices[i]]
    header[i] = colnames(table_df)[indices[i]]
  }
  colnames(output_df) = header
  rownames(output_df) = row_labels
  return (output_df)
}



table_function_numeric <- function(data_table,strata_name,strata_labels,variables, var_labels, plot_label) {
  strata <- data_table[,strata_name]
  binary_funct <- function(binary_var) {
    count<-table(data_table[,binary_var],strata)["1",]
    mat <- t(as.matrix(c(count,sum(count))))
    return(mat)
  }
  header <- paste0(c(strata_labels,"Overall"),"\n (N=", c(table(strata),length(strata)),")")
  df <- data.frame(matrix(ncol=(length(unique(strata[!is.na(strata)] ))+1),nrow=length(variables)))
  rownames(df)<-var_labels
  colnames(df)<-header
  for(i in 1:length(variables)) {
    if(length(table(data_table[,variables[i]]))>1) {
      df[i,]<-binary_funct(variables[i])
    } else {
      df[i,]<-NA
    }
  }
  return(df)
}