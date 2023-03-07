#Import modules-------------------------------------------------------------

library(ggplot2)
library(knitr)
library(scales)
library(tidyverse)

#Create visualization functions----------------------------------------------

# DOMAIN REPRESENTATION IN EACH COMMUNITY

barplot_by_domain <- function(path, cm_algorithm, battery_name){
  #import datasets
  df_CN <- read.csv(paste(path,cm_algorithm,"_CN.csv",sep=""))
  df_CN['Diagnostic'] <- "CN"
  df_MCI <- read.csv(paste(path,cm_algorithm,"_MCI.csv",sep=""))
  df_MCI['Diagnostic'] <- "MCI"
  df_AD <- read.csv(paste(path,cm_algorithm,"_AD.csv",sep=""))
  df_AD['Diagnostic'] <- "AD"
  
  df <- rbind(df_CN, df_MCI, df_AD)
  df['Group']<-as.character(df$Index) #transform into discrete variable
  
  #barplot
  p <- ggplot(df, aes(x=Domain,y=Percentage)) +
    geom_bar(aes(fill=Group), position="stack", stat="identity") +
    scale_fill_viridis_d() +
    facet_grid(~factor(Diagnostic, levels=c('CN', 'MCI', 'AD'))) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  +
    ggtitle(paste("% of representation of each domain by domain (", battery_name,
                  ", ", cm_algorithm, ")", sep=""))
  
  return(p)
}


barplot_by_community <- function(path, cm_algorithm, battery_name){
  #import datasets
  df_CN <- read.csv(paste(path,cm_algorithm,"_CN.csv",sep=""))
  df_CN['Diagnostic'] <- "CN"
  df_MCI <- read.csv(paste(path,cm_algorithm,"_MCI.csv",sep=""))
  df_MCI['Diagnostic'] <- "MCI"
  df_AD <- read.csv(paste(path,cm_algorithm,"_AD.csv",sep=""))
  df_AD['Diagnostic'] <- "AD"
  
  df <- rbind(df_CN, df_MCI, df_AD)
  df['Group']<-as.character(df$Index) #transform into discrete variable
  
  #barplot
  p <- ggplot(df, aes(x=Group,y=Percentage)) +
    geom_bar(aes(fill=Domain), position="stack", stat="identity") +
    scale_fill_viridis_d() +
    facet_grid(~factor(Diagnostic, levels=c('CN', 'MCI', 'AD'))) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle(paste("% of representation of each domain by community (", battery_name,
                  ", ", cm_algorithm, ")", sep=""))
  
  return(p)
}


## ADAS

### Louvain algorithm communities

ADAS_Lv_dm <- barplot_by_domain("./ADAS/", "Louvain", "ADAS")
ADAS_Lv_cm <- barplot_by_community("./ADAS/", "Louvain", "ADAS")

### Greedy community algorithm
#barplot by domain
ADAS_Gd_dm <- barplot_by_domain("./ADAS/", "Greedy", "ADAS")
#barplot by community
ADAS_Gd_cm <- barplot_by_community("./ADAS/", "Greedy", "ADAS")

### Kernighan-Lin bisection algorithm
ADAS_Bs_dm <- barplot_by_domain("./ADAS/", "Bisection", "ADAS")
ADAS_Bs_cm <- barplot_by_community("./ADAS/", "Bisection", "ADAS")

### Asynchronous fluid communities algorithm
ADAS_As_dm <- barplot_by_domain("./ADAS/", "Asyn", "ADAS")
ADAS_As_cm <- barplot_by_community("./ADAS/", "Asyn", "ADAS")
