#IMPORT LIBRARIES -------------------------------------------------------------
library(stringr)
library(readr)
library(ggplot2)
library("ggsignif")
library(tidyr)
library('dplyr')
library(tidytext)
#analysis of communities
library(tidyverse)
library(knitr)
library(scales)

#GLOBAL METRIC RESULTS (BAR PLOT)-----------------------------------------------

#get list of directories inside Bootstrap_results folder
main_dir <- paste(getwd(), "/Bootstrap_gm_results", sep="")
dir_ls <- list.dirs(path = main_dir, full.names = TRUE, recursive = FALSE)

#empty dataframe to store results
gm_df <- data.frame(battery=character(), dx=character(), measure=character(), 
                    mean=double(), sd=double(), sd_min=double(), sd_max=double()) 

#iterate directories (batteries)
for (dir in dir_ls){
  i <- 0
  file_path_ls <- list.files(path=dir, full.names=TRUE) #get metric files for each dx group
  battery <- tail(unlist(str_split(dir, "/")), n=1) 
  
  #iterate each diagnostic group
  for (file in file_path_ls){
    name <- tail(unlist(str_split(file, "/")), n=1) #remove path
    dx <- unlist(str_split(name, ".csv"))[1] #remove file extension
    
    df_temp <- read.csv(file, header=TRUE, sep=",")
    
    for (measure in colnames(df_temp)){
      mean <- mean(df_temp[,measure])
      sd <- sd(df_temp[,measure])
      sd_min <- mean - sd
      sd_max <- mean + sd
      gm_df[nrow(gm_df) + 1,] <- c(battery, dx, measure, mean, sd, sd_min, sd_max)
    }
    
  }
}

gm_df$dx = factor(gm_df$dx, levels=c('CN', 'MCI', 'AD'))
gm_df$battery = factor(gm_df$battery, levels=c('ADAS', 'MOCA', 'NEUROBAT','ADASMOCA'))
gm_df$measure = factor(gm_df$measure, levels = c('Diameter', 'Density', 'AvDegree',
                       'AvCC', 'GE'))

cbPalette <- c("#1f77b4", "#ff7f0e", "#2ca02c")


#plot

gm <- ggplot(gm_df, aes(x=dx, y=as.numeric(mean), fill=dx)) +
  #geom_boxplot() +
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           linewidth=.3) +      # Thinner 
  geom_errorbar(aes(ymin = as.numeric(mean), ymax = as.numeric(sd_max)), position = position_dodge(), width = 0.2) +
  #geom_signif(comparisons = list(c("CN", "MCI"), c("CN", "AD")), map_signif_level = TRUE) +
  scale_fill_manual(values=cbPalette, name="diagnostic") +
  scale_y_continuous(n.breaks=3, expand = expansion(mult = c(0, 0.3))) +
  facet_grid(rows = vars(measure), cols = vars(battery), scales = "free") +
  xlab("Diagnostic group") +
  ylab("Mean value (n=250)") +
  ggtitle("GT global measures") +
  theme_bw() +
  theme(
    panel.grid.major.x = element_blank(),
    plot.title = element_text(hjust = 0.5, size=16, face="bold"),
    axis.text=element_text(size=10),
    axis.title=element_text(size=14,face="bold"),
    strip.text.x = element_text(size = 12, face="bold"),
    strip.text.y = element_text(size = 12, face="bold"),
    strip.background = element_rect(color = "white", fill="white"),
    legend.title=element_blank(),
    legend.position="bottom"
  )

ggsave(file="./Figures/Bootstrap_metrics/gm.png", device="png", plot=gm, width=10, height=8)

#GLOBAL METRIC RESULTS (BOXPLOT)-----------------------------------------------

#get list of directories inside Bootstrap_results folder
main_dir <- paste(getwd(), "/Bootstrap_gm_results", sep="")
dir_ls <- list.dirs(path = main_dir, full.names = TRUE, recursive = FALSE)

#empty dataframe to store results
gm_df <- data.frame(measure=character(), value=double(), dx=character(),battery=character()) 

#iterate directories (batteries)
for (dir in dir_ls){
  i <- 0
  file_path_ls <- list.files(path=dir, full.names=TRUE) #get metric files for each dx group
  battery <- tail(unlist(str_split(dir, "/")), n=1) 
  
  #iterate each diagnostic group
  for (file in file_path_ls){
    name <- tail(unlist(str_split(file, "/")), n=1) #remove path
    dx <- unlist(str_split(name, ".csv"))[1] #remove file extension
    
    df_temp <- read.csv(file, header=TRUE, sep=",")
    
    # Convert dataframe to long format
    df_temp <- df_temp %>% 
      pivot_longer(everything(), names_to = "measure", values_to = "value")
    df_temp$dx <- dx
    df_temp$battery <- battery
    
    gm_df <- rbind(gm_df, df_temp)
    
    }
  }


gm_df$dx = factor(gm_df$dx, levels=c('CN', 'MCI', 'AD'))
gm_df$battery = factor(gm_df$battery, levels=c('ADAS', 'MOCA', 'NEUROBAT','ADASMOCA'))
gm_df$measure = factor(gm_df$measure, levels = c('Diameter', 'Density', 'AvDegree',
                                                 'AvCC', 'GE'))

cbPalette <- c("#1f77b4", "#ff7f0e", "#2ca02c")


#plot

sigFunc = function(x){
  if(x < 0.001){"***"} 
  else if(x < 0.01){"**"}
  else if(x < 0.05){"*"}
  else{NA}}

gm_bx <- ggplot(gm_df, aes(x=dx, y=as.numeric(value), fill=dx)) +
  stat_boxplot(geom = "errorbar", width = 0.3) +
  geom_boxplot(colour="black", # Use black outlines,
  linewidth=.3, width=0.5, position = position_dodge(width=0.1),
  outlier.size=1.3, outlier.shape=18) +
  geom_signif(comparisons = list(c("CN", "MCI")), map_signif_level = sigFunc, 
              margin_top = 0.05, tip_length = 0) +
  geom_signif(comparisons = list(c("MCI", "AD")), map_signif_level = sigFunc, 
              margin_top = 0.20, tip_length = 0) +
  geom_signif(comparisons = list(c("CN", "AD")), map_signif_level = sigFunc, 
              margin_top = 0.35, tip_length = 0) +
  stat_summary(fun = "mean", geom="point", size=0.7, colour="white", fill="white") +
  scale_fill_manual(values=cbPalette, name="diagnostic") +
  scale_y_continuous(n.breaks=5, expand = expansion(mult = c(0, 0.1))) +
  facet_grid(rows = vars(measure), cols = vars(battery), scales = "free") +
  xlab("Diagnostic group") +
  ylab("Mean value (n=250)") +
  ggtitle("GT global measures") +
  theme_bw() +
  theme(
    panel.grid.major.x = element_blank(),
    plot.title = element_text(hjust = 0.5, size=16, face="bold"),
    axis.text=element_text(size=10),
    axis.title=element_text(size=14,face="bold"),
    strip.text.x = element_text(size = 12, face="bold"),
    strip.text.y = element_text(size = 12, face="bold"),
    strip.background = element_rect(color = "white", fill="white"),
    legend.title=element_blank(),
    legend.position="bottom"
  )

ggsave(file="./Figures/Bootstrap_metrics/gm_boxplot.png", device="png", plot=gm_bx, width=10, height=9)


#DEGREE CENTRALITY RESULTS --------------------------------------------------------

#get list of directories inside Bootstrap_results folder
main_dir <- paste(getwd(), "/Bootstrap_dc_results", sep="")
dir_ls <- list.dirs(path = main_dir, full.names = TRUE, recursive = FALSE)

for (dir in dir_ls){
  i <- 0
  file_path_ls <- list.files(path=dir, full.names=TRUE) #get metric files for each dx group
  battery <- tail(unlist(str_split(dir, "/")), n=1) 
  
  if (battery == "ADAS"){
    tests = c("Q1_WR", "Q2_Comm", "Q3_CP", "Q4_DWR", "Q5_NT", "Q6_IP", "Q7_O",
                 "Q8_WRT", "Q9_RTI", "Q10_Comp", "Q11_WFD", "Q12_SL", "Q13_NC")
  }
  
  else if (battery == "MOCA") {
    tests = c("Mo1_TMT", "Mo2_CC", "Mo3_CDT", "Mo4_NT", "Mo5_WRT1", "Mo6_WRT2",
                 "Mo7_DSF", "Mo8_DSB", "Mo9_SA", "Mo10_SST", "Mo11_Rep", "Mo12_VF",
                 "Mo13_Abs", "Mo14_DWR", "Mo15_OT", "Mo16_OP")
  }
  
  else{
    tests = c("NB1_RAVLTD", "NB2_RAVLTR", "NB3_RAVLTI", "NB4_CAT_FLU", "NB5_BNT",
              "NB6_CDT", "NB7_CCT", "NB8_DSF", "NB9_DSB", "NB10_TMTA", "NB11_TMTB",
              "NB12_ANART")
  }
  
  #results df
  dc_stats <- data.frame(Test=character(), mean=double(), sd=double(), 
                         max=double(), min=double(), dx=character())
  
  #iterate each diagnostic group
  for (file in file_path_ls){
    name <- tail(unlist(str_split(file, "/")), n=1) #remove path
    dx <- unlist(str_split(name, ".csv"))[1] #remove file extension
    
    df_temp <- read.csv(file, header=TRUE, sep=",")
    colnames(df_temp) <- tests
    
    data_long <- gather(df_temp, factor_key=TRUE, key="Test")
    
    #Compute bootstrap statistics
    stats_temp_df <- data_long%>% group_by(Test)%>%
      summarise(mean= mean(value), sd= sd(value), max = mean(value) + sd(value),
                min = mean(value) - sd(value))
    
    stats_temp_df$dx <- dx
    
    dc_stats <- rbind(dc_stats, stats_temp_df)
    
  }
  
  cbPalette <- c("#1f77b4", "#ff7f0e", "#2ca02c")
  dc_stats$dx = factor(dc_stats$dx, levels=c('CN', 'MCI', 'AD'))
  dc_stats$mean <- as.numeric(as.character(dc_stats$mean))
  
  threshold_df <- dc_stats %>%
    group_by(dx) %>%
    summarise(threshold = mean(mean) + 1.5*sd(mean))
  
  
  #plot
  p <- ggplot(dc_stats, aes(x=reorder_within(Test, mean, dx) , y=as.numeric(mean), fill=dx)) + 
      geom_bar(position=position_dodge(), stat="identity",
               colour="black", # Use black outlines,
               linewidth=.3) +      # Thinner 
      geom_errorbar(aes(ymin = as.numeric(mean), ymax = as.numeric(max)), position = position_dodge(), width = 0.2) +
      geom_hline(data=threshold_df, aes(yintercept=threshold), colour="black", linewidth=0.8) + 
      coord_flip() + 
      scale_fill_manual(values=cbPalette, name="diagnostic") +
      scale_y_continuous(n.breaks=5) +
      facet_grid(rows = vars(dx), scales = "free") +
      scale_x_reordered() +
      xlab("Degree centrality (DC)") +
      ylab("Test") +
      ggtitle(paste("Degree centrality (", battery, ")", sep="")) +
      theme_bw() +
    theme(
      panel.grid.major.y = element_blank(),
      plot.title = element_text(hjust = 0.5, size=16, face="bold"),
      axis.text=element_text(size=10),
      axis.title=element_text(size=14,face="bold"),
      strip.text.x = element_text(size = 12, face="bold"),
      strip.text.y = element_text(size = 12, face="bold"),
      strip.background = element_rect(color = "white", fill="white"),
      legend.title=element_blank(),
      legend.position="bottom"
    )
  
  filename = paste("./Figures/Bootstrap_metrics/DC_", battery, ".png", sep="")
  ggsave(file=filename, device="png", plot=p, width=7, height=10)
}

#BETWEENNESS CENTRALITY RESULTS --------------------------------------------------------

#get list of directories inside Bootstrap_results folder
main_dir <- paste(getwd(), "/Bootstrap_bc_results", sep="")
dir_ls <- list.dirs(path = main_dir, full.names = TRUE, recursive = FALSE)

for (dir in dir_ls){
  i <- 0
  file_path_ls <- list.files(path=dir, full.names=TRUE) #get metric files for each dx group
  battery <- tail(unlist(str_split(dir, "/")), n=1) 
  
  if (battery == "ADAS"){
    tests = c("Q1_WR", "Q2_Comm", "Q3_CP", "Q4_DWR", "Q5_NT", "Q6_IP", "Q7_O",
              "Q8_WRT", "Q9_RTI", "Q10_Comp", "Q11_WFD", "Q12_SL", "Q13_NC")
  }
  
  else if (battery == "MOCA") {
    tests = c("Mo1_TMT", "Mo2_CC", "Mo3_CDT", "Mo4_NT", "Mo5_WRT1", "Mo6_WRT2",
              "Mo7_DSF", "Mo8_DSB", "Mo9_SA", "Mo10_SST", "Mo11_Rep", "Mo12_VF",
              "Mo13_Abs", "Mo14_DWR", "Mo15_OT", "Mo16_OP")
  }
  
  else{
    tests = c("NB1_RAVLTD", "NB2_RAVLTR", "NB3_RAVLTI", "NB4_CAT_FLU", "NB5_BNT",
              "NB6_CDT", "NB7_CCT", "NB8_DSF", "NB9_DSB", "NB10_TMTA", "NB11_TMTB",
              "NB12_ANART")
  }
  
  #results df
  dc_stats <- data.frame(Test=character(), mean=double(), sd=double(), 
                         max=double(), min=double(), dx=character())
  
  #iterate each diagnostic group
  for (file in file_path_ls){
    name <- tail(unlist(str_split(file, "/")), n=1) #remove path
    dx <- unlist(str_split(name, ".csv"))[1] #remove file extension
    
    df_temp <- read.csv(file, header=TRUE, sep=",")
    colnames(df_temp) <- tests
    
    data_long <- gather(df_temp, factor_key=TRUE, key="Test")
    
    #Compute bootstrap statistics
    stats_temp_df <- data_long%>% group_by(Test)%>%
      summarise(mean= mean(value), sd= sd(value), max = mean(value) + sd(value),
                min = mean(value) - sd(value))
    
    stats_temp_df$dx <- dx
    
    dc_stats <- rbind(dc_stats, stats_temp_df)
    
  }
  
  cbPalette <- c("#1f77b4", "#ff7f0e", "#2ca02c")
  dc_stats$dx = factor(dc_stats$dx, levels=c('CN', 'MCI', 'AD'))
  dc_stats$mean <- as.numeric(as.character(dc_stats$mean))
  
  threshold_df <- dc_stats %>%
    group_by(dx) %>%
    summarise(threshold = mean(mean) + 1.5*sd(mean))
  
  
  #plot
  p <- ggplot(dc_stats, aes(x=reorder_within(Test, mean, dx) , y=as.numeric(mean), fill=dx)) + 
    geom_bar(position=position_dodge(), stat="identity",
             colour="black", # Use black outlines,
             linewidth=.3) +      # Thinner 
    geom_errorbar(aes(ymin = as.numeric(mean), ymax = as.numeric(max)), position = position_dodge(), width = 0.2) +
    geom_hline(data=threshold_df, aes(yintercept=threshold), colour="black", linewidth=0.8) + 
    coord_flip() + 
    scale_fill_manual(values=cbPalette, name="diagnostic") +
    scale_y_continuous(n.breaks=5) +
    facet_grid(rows = vars(dx), scales = "free") +
    scale_x_reordered() +
    xlab("Betweenness centrality (BC)") +
    ylab("Test") +
    ggtitle(paste("Betweenness centrality (", battery, ")", sep="")) +
    theme_bw() +
    theme(
      panel.grid.major.x = element_blank(),
      plot.title = element_text(hjust = 0.5, size=16, face="bold"),
      axis.text=element_text(size=10),
      axis.title=element_text(size=14,face="bold"),
      strip.text.x = element_text(size = 12, face="bold"),
      strip.text.y = element_text(size = 12, face="bold"),
      strip.background = element_rect(color = "white", fill="white"),
      legend.title=element_blank(),
      legend.position="bottom"
    )
  
  filename = paste("./Figures/Bootstrap_metrics/BC_", battery, ".png", sep="")
  ggsave(file=filename, device="png", plot=p, width=7, height=10)
}

#ANALYSIS OF COMMUNITIES

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
                  ", ", cm_algorithm, ")", sep="")) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_text(hjust = 0.5, size=16, face="bold"),
      axis.text=element_text(size=10),
      axis.title=element_text(size=14,face="bold"),
      strip.text.x = element_text(size = 12, face="bold"),
      strip.text.y = element_text(size = 12, face="bold"),
      strip.background = element_rect(color = "white", fill="white"),
    )
  
  result_name=paste(battery_name, "_", cm_algorithm, ".png", sep="")
  
  ggsave(filename=result_name, plot=p, device='png', path="./Figures/Community detection algorithm/", width = 8, height = 4, )
  
  return(p)
}


## ADAS
ADAS_Lv_cm <- barplot_by_community("./ADAS/", "Louvain", "ADAS")
ADAS_Gd_cm <- barplot_by_community("./ADAS/", "Greedy", "ADAS")
ADAS_Bs_cm <- barplot_by_community("./ADAS/", "Bisection", "ADAS")
ADAS_As_cm <- barplot_by_community("./ADAS/", "Asyn", "ADAS")

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
ADASMOCA_As_cm <- barplot_by_community("./ADASMOCA/", "Asyn", "ADASMOCA")

## NEUROBAT
NEUROBAT_Lv_cm <- barplot_by_community("./NEUROBAT/", "Louvain", "NEUROBAT")
NEUROBAT_Gd_cm <- barplot_by_community("./NEUROBAT/", "Greedy", "NEUROBAT")
NEUROBAT_Bs_cm <- barplot_by_community("./NEUROBAT/", "Bisection", "NEUROBAT")
NEUROBAT_As_cm <- barplot_by_community("./NEUROBAT/", "Asyn", "NEUROBAT")

