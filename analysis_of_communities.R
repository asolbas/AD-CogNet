#Import modules-------------------------------------------------------------

library(ggplot2)
library(knitr)
library(scales)
library(tidyverse)

#Create visualization functions----------------------------------------------

# DOMAIN REPRESENTATION IN EACH COMMUNITY


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
  
  #create palette
  cbPalette <- c("#9999FF", "#CC99FF", "#F8766D", "#00BFC4", "#66CC99", "#FFCC99")
  
  #barplot
  p <- ggplot(df, aes(x=Group,y=Percentage)) +
    geom_bar(aes(fill=Domain), position="stack", stat="identity") +
    scale_fill_manual(values=cbPalette) +
    facet_grid(~factor(Diagnostic, levels=c('CN', 'MCI', 'AD'))) +
    theme_bw() +
    ggtitle(paste("% of representation of each domain by community (", battery_name,
                  ", ", cm_algorithm, ")", sep=""))
  
  result_name=paste(battery_name, "_", cm_algorithm, ".png", sep="")
  
  ggsave(filename=result_name, plot=p, device='png', path="./Figures/Community detection algorithm/", width = 8, height = 4, )
  
  return(p)
}


## ADAS
ADAS_Lv_cm <- barplot_by_community("./ADAS/", "Louvain", "ADAS")
ADAS_Gd_cm <- barplot_by_community("./ADAS/", "Greedy", "ADAS")
ADAS_Bs_cm <- barplot_by_community("./ADAS/", "Bisection", "ADAS")
ADAS_As_cm <- barplot_by_community("./ADAS/", "Asyn", "ADAS")

## MMSE
MMSE_Lv_cm <- barplot_by_community("./MMSE/", "Louvain", "MMSE")
MMSE_Gd_cm <- barplot_by_community("./MMSE/", "Greedy", "MMSE")
MMSE_Bs_cm <- barplot_by_community("./MMSE/", "Bisection", "MMSE")
MMSE_As_cm <- barplot_by_community("./MMSE/", "Asyn", "MMSE")

## MOCA
MOCA_Lv_cm <- barplot_by_community("./MOCA/", "Louvain", "MOCA")
MOCA_Gd_cm <- barplot_by_community("./MOCA/", "Greedy", "MOCA")
MOCA_Bs_cm <- barplot_by_community("./MOCA/", "Bisection", "MOCA")
MOCA_As_cm <- barplot_by_community("./MOCA/", "Asyn", "MOCA")

## Merged
merged_Lv_cm <- barplot_by_community("./merged/", "Louvain", "merged")
merged_Gd_cm <- barplot_by_community("./merged/", "Greedy", "merged")
merged_Bs_cm <- barplot_by_community("./merged/", "Bisection", "merged")
merged_As_cm <- barplot_by_community("./merged/", "Asyn", "merged")

## Merged (ADAS and MoCA)
ADASMOCA_Lv_cm <- barplot_by_community("./ADASMOCA/", "Louvain", "ADASMOCA")
ADASMOCA_Gd_cm <- barplot_by_community("./ADASMOCA/", "Greedy", "ADASMOCA")
ADASMOCA_Bs_cm <- barplot_by_community("./ADASMOCA/", "Bisection", "ADASMOCA")
ADASMOCA_As_cm <- barplot_by_community("./ADASMOCA/", "Asyn", "ADASMOCA")m