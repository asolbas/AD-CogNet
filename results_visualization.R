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
library(rstatix)
library(ggpubr)

#GLOBAL METRIC RESULTS (BAR PLOT)-----------------------------------------------

#get list of directories inside Bootstrap_results folder
main_dir <- paste(getwd(), "/Results/Bootstrap_gm_results", sep="")
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
      if (measure != "GE"){
        mean <- mean(df_temp[,measure])
        sd <- sd(df_temp[,measure])
        sd_min <- mean - sd
        sd_max <- mean + sd
        gm_df[nrow(gm_df) + 1,] <- c(battery, dx, measure, mean, sd, sd_min, sd_max)
      }
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
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           linewidth=.3) +      # Thinner 
  geom_errorbar(aes(ymin = as.numeric(mean), ymax = as.numeric(sd_max)), position = position_dodge(), width = 0.2) +
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

ggsave(file="./Results/Figures/Bootstrap_metrics/gm.png", device="png", plot=gm, width=10, height=8)

#GLOBAL METRIC RESULTS without ADASMOCA (BOXPLOT)-----------------------------------------------

#get list of directories inside Bootstrap_results folder
main_dir <- paste(getwd(), "/Results/Bootstrap_gm_results", sep="")
dir_ls <- list.dirs(path = main_dir, full.names = TRUE, recursive = FALSE)

#empty dataframe to store results
gm_df <- data.frame(measure=character(), value=double(), dx=character(),battery=character()) 

#iterate directories (batteries)
for (dir in dir_ls){
  i <- 0
  file_path_ls <- list.files(path=dir, full.names=TRUE) #get metric files for each dx group
  battery <- tail(unlist(str_split(dir, "/")), n=1) 
  
  if (battery != "ADASMOCA"){
  
    #iterate each diagnostic group
    for (file in file_path_ls){
      name <- tail(unlist(str_split(file, "/")), n=1) #remove path
      dx <- unlist(str_split(name, ".csv"))[1] #remove file extension
      
      df_temp <- read.csv(file, header=TRUE, sep=",")
      
      # Convert dataframe to long format
      df_temp <- df_temp %>% 
        pivot_longer(everything(), names_to = "measure", values_to = "value") %>%
        filter(measure!="GE")
      df_temp$dx <- dx
      df_temp$battery <- battery
      
      gm_df <- rbind(gm_df, df_temp)
    }
    
  }
}


#Student's T test
st_gm <- gm_df[, c("dx", "battery", "measure", "value")] %>% group_by(battery, measure ) %>%
  t_test(value ~ dx, p.adjust.method = "BH",) %>%
  add_significance("p.adj", cutpoints = c(0, 0.001, 0.01, 0.05, 1),
                   symbols = c("***", "**", "*", "ns")) %>% 
  add_xy_position(x = "dx", 
                  scales = "free", 
                  step.increase=0)


gm_df$dx = factor(gm_df$dx, levels=c('CN', 'MCI', 'AD'))
gm_df$battery = factor(gm_df$battery, levels=c('ADAS', 'MOCA', 'NEUROBAT','ADASMOCA'))
gm_df$measure = factor(gm_df$measure, levels = c('Diameter', 'Density', 'AvDegree',
                                                 'AvCC', 'GE'))

cbPalette <- c("#1f77b4", "#ff7f0e", "#2ca02c")


#plot

#adjust sig labels position
bracket.nudge.y <- c(0.01,0.04,0.07,
                     0.005, 0.17, 0.34,
                     0.005,0.02,0.035,
                     0.02,0.04,0.06,
                     0.01,0.04,
                     0, 0.17, 0.34,
                     0.005,0.02,0.035,
                     0.02,0.04,0.06,
                     
                     0.01,0.04,0.07,
                     0, 0.17, 0.34,
                     0.005,0.02,0.035,
                     0.02,0.04,0.06
)


gm_bx <- ggplot(gm_df) +
  stat_boxplot(aes(x=dx, y=as.numeric(value)), geom = "errorbar", width = 0.3) +
  geom_boxplot(aes(x=dx, y=as.numeric(value), fill=dx), colour="black", # Use black outlines,
               linewidth=.3, width=0.5, position = position_dodge(width=0.1),
               outlier.size=1.3, outlier.shape=18) +
  stat_summary(aes(x=dx, y=as.numeric(value)),
               fun = "mean", geom="point", size=0.7, colour="white", fill="white") +
  scale_fill_manual(values=cbPalette, name="diagnostic") +
  facet_grid(rows = vars(measure), cols = vars(battery), scales = "free") +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.15))) +
  xlab("Diagnostic group") +
  ylab("Value (n=250)") +
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
  ) +   
  stat_pvalue_manual(st_gm, label = "p.adj.signif",
                     tip.length = 0, step.increase = 0,
                     hide.ns = TRUE, bracket.nudge.y=bracket.nudge.y,
                     size = 5, bracket.size = 0.5) 


ggsave(file="./Results/Figures/Bootstrap_metrics/gm_boxplot_noADASMOCA.png", device="png", plot=gm_bx, width=7, height=7)


#DEGREE CENTRALITY RESULTS --------------------------------------------------------

#get list of directories inside Bootstrap_results folder
main_dir <- paste(getwd(), "/Results/Bootstrap_dc_results", sep="")
dir_ls <- list.dirs(path = main_dir, full.names = TRUE, recursive = FALSE)

for (dir in dir_ls){
  i <- 0
  file_path_ls <- list.files(path=dir, full.names=TRUE) #get metric files for each dx group
  battery <- tail(unlist(str_split(dir, "/")), n=1) 
  
  if(battery=="ADASMOCA"){
    next
  }
  
  if (battery == "ADAS"){
    tests = c("Q1_WR", "Q2_Comm", "Q3_CP", "Q4_DWR", "Q5_NT", "Q6_IP", "Q7_O",
                 "Q8_WRT", "Q9_RTI", "Q10_Comp", "Q11_WFD", "Q12_SL", "Q13_NC")
  }
  
  else if (battery == "MOCA") {
    tests = c("Mo1_TMT", "Mo2_CC", "Mo3_CDT", "Mo4_NT", "Mo5_WRT1", "Mo6_WRT2",
                 "Mo7_DSF", "Mo8_DSB", "Mo9_SelA", "Mo10_SST", "Mo11_Rep", "Mo12_VF",
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
      #xlab("Tests") +
      xlab("") +
      ylab("DC") +
      #ggtitle(paste("Degree centrality (", battery, ")", sep="")) +
      theme_bw() +
    theme(
      panel.grid.major.y = element_blank(),
      plot.title = element_text(hjust = 0.5, size=16, face="bold"),
      axis.text=element_text(size=10),
      axis.title=element_text(size=12,face="bold"),
      strip.text.x = element_text(size = 12, face="bold"),
      strip.text.y = element_text(size = 12, face="bold"),
      strip.background = element_rect(color = "white", fill="white"),
      legend.title=element_blank(),
      legend.position="bottom"
    )
  
  filename = paste("./Results/Figures/Bootstrap_metrics/DC_", battery, ".png", sep="")
  ggsave(file=filename, device="png", plot=p, width=3, height=7)
}

#BETWEENNESS CENTRALITY RESULTS --------------------------------------------------------

#get list of directories inside Bootstrap_results folder
main_dir <- paste(getwd(), "/Results/Bootstrap_bc_results", sep="")
dir_ls <- list.dirs(path = main_dir, full.names = TRUE, recursive = FALSE)

for (dir in dir_ls){
  i <- 0
  file_path_ls <- list.files(path=dir, full.names=TRUE) #get metric files for each dx group
  battery <- tail(unlist(str_split(dir, "/")), n=1) 
  
  if(battery=="ADASMOCA"){
    next
  }
  
  if (battery == "ADAS"){
    tests = c("Q1_WR", "Q2_Comm", "Q3_CP", "Q4_DWR", "Q5_NT", "Q6_IP", "Q7_O",
              "Q8_WRT", "Q9_RTI", "Q10_Comp", "Q11_WFD", "Q12_SL", "Q13_NC")
  }
  
  else if (battery == "MOCA") {
    tests = c("Mo1_TMT", "Mo2_CC", "Mo3_CDT", "Mo4_NT", "Mo5_WRT1", "Mo6_WRT2",
              "Mo7_DSF", "Mo8_DSB", "Mo9_SelA", "Mo10_SST", "Mo11_Rep", "Mo12_VF",
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
    #xlab("Tests") +
    xlab("") +
    ylab("BC") +
    #ggtitle(paste("Betweenness centrality (", battery, ")", sep="")) +
    theme_bw() +
    theme(
      panel.grid.major.y = element_blank(),
      plot.title = element_text(hjust = 0.5, size=16, face="bold"),
      axis.text=element_text(size=10),
      axis.title=element_text(size=12,face="bold"),
      strip.text.x = element_text(size = 12, face="bold"),
      strip.text.y = element_text(size = 12, face="bold"),
      strip.background = element_rect(color = "white", fill="white"),
      legend.title=element_blank(),
      legend.position="bottom"
    )
  
  filename = paste("./Results/Figures/Bootstrap_metrics/BC_", battery, ".png", sep="")
  ggsave(file=filename, device="png", plot=p, width=3, height=7)
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
  
  #Convert to percentages
  percent_labels <- function(x) {
    scales::number(x * 100)
  }
  
  #barplot
  p <- ggplot(df, aes(x=Group,y=Percentage)) +
    geom_bar(aes(fill=Domain), position="stack", stat="identity") +
    scale_fill_manual(values = c("Attention" = "#9999FF", "Executive"="#CC99FF",
                                "Language"="#F8766D","Memory"= "#00BFC4",
                                "Orientation"="#66CC99", "Visuospatial"="#FFCC99")) +
    facet_grid(~factor(Diagnostic, levels=c('CN', 'MCI', 'AD'))) +
    scale_y_continuous(labels=percent_labels) +
    theme_bw() + 
    xlab("Community") +
    ylab("Percentage (%)") +
    #ggtitle(paste("% of representation of each domain by community (", battery_name,
    #              ", ", cm_algorithm, ")", sep="")) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_text(hjust = 0.5, size=16, face="bold"),
      axis.text=element_text(size=10),
      axis.title=element_text(size=12,face="bold"),
      strip.text.x = element_text(size = 12, face="bold"),
      strip.text.y = element_text(size = 12, face="bold"),
      strip.background = element_rect(color = "white", fill="white"),
    )
  
  result_name=paste(battery_name, "_", cm_algorithm, ".png", sep="")
  
  ggsave(filename=result_name, plot=p, device='png', path="./Results/Figures/Community detection algorithm/",
         width = 7, height = 2)
  
  return(p)
}


## ADAS
ADAS_Lv_cm <- barplot_by_community("./Results/ADAS/", "Louvain", "ADAS")
ADAS_Gd_cm <- barplot_by_community("./Results/ADAS/", "Greedy", "ADAS")
ADAS_Bs_cm <- barplot_by_community("./Results/ADAS/", "Bisection", "ADAS")
ADAS_As_cm <- barplot_by_community("./Results/ADAS/", "Asyn", "ADAS")

## MOCA
MOCA_Lv_cm <- barplot_by_community("./Results/MOCA/", "Louvain", "MOCA")
MOCA_Gd_cm <- barplot_by_community("./Results/MOCA/", "Greedy", "MOCA")
MOCA_Bs_cm <- barplot_by_community("./Results/MOCA/", "Bisection", "MOCA")
MOCA_As_cm <- barplot_by_community("./Results/MOCA/", "Asyn", "MOCA")

## Merged
merged_Lv_cm <- barplot_by_community("./Results/merged/", "Louvain", "merged")
merged_Gd_cm <- barplot_by_community("./Results/merged/", "Greedy", "merged")
merged_Bs_cm <- barplot_by_community("./Results/merged/", "Bisection", "merged")
merged_As_cm <- barplot_by_community("./Results/merged/", "Asyn", "merged")

## Merged (ADAS and MoCA)
ADASMOCA_Lv_cm <- barplot_by_community("./Results/ADASMOCA/", "Louvain", "ADASMOCA")
ADASMOCA_Gd_cm <- barplot_by_community("./Results/ADASMOCA/", "Greedy", "ADASMOCA")
ADASMOCA_Bs_cm <- barplot_by_community("./Results/ADASMOCA/", "Bisection", "ADASMOCA")
ADASMOCA_As_cm <- barplot_by_community("./Results/ADASMOCA/", "Asyn", "ADASMOCA")

## NEUROBAT
NEUROBAT_Lv_cm <- barplot_by_community("./Results/NEUROBAT/", "Louvain", "NEUROBAT")
NEUROBAT_Gd_cm <- barplot_by_community("./Results/NEUROBAT/", "Greedy", "NEUROBAT")
NEUROBAT_Bs_cm <- barplot_by_community("./Results/NEUROBAT/", "Bisection", "NEUROBAT")
NEUROBAT_As_cm <- barplot_by_community("./Results/NEUROBAT/", "Asyn", "NEUROBAT")

#-----------------------------------------------------------------------------
#GENETIC CLUSTERS ANALYSIS
#-----------------------------------------------------------------------------
#GLOBAL METRIC RESULTS (BAR PLOT)-----------------------------------------------

#get list of directories inside Bootstrap_results folder
main_dir <- paste(getwd(), "/Genetic-Clusters/Results", sep="")
dir_ls <- list.dirs(path = main_dir, full.names = TRUE, recursive = FALSE)

#empty dataframe to store results
gm_df <- data.frame(Cluster=character(), battery=character(), Measure=character(), 
                    mean=double(), sd=double(), sd_min=double(), sd_max=double()) 

#iterate directories (batteries)
for (dir in dir_ls){
  battery <- tail(unlist(str_split(dir, "/")), n=1) 
  file <- paste(dir, "/cluster/globalMetrics.csv", sep="")
  
  df_temp <- read.csv(file, header=TRUE, sep=";") %>% rename(GE=AvGE)
  metrics <- c("Diameter", "Density", "AvDegree", "AvCC", "GE")
  df_temp$battery <- battery
  
  df_temp <- df_temp %>% pivot_longer(cols=metrics, names_to="Measure", values_to="Value") %>%
    filter(Measure!="GE")%>%
    group_by(Cluster, battery, Measure) %>%
    summarise(mean=mean(Value, na.rm = TRUE), sd=sd(Value, na.rm = TRUE), 
              sd_min=mean(Value, na.rm = TRUE)-sd(Value, na.rm = TRUE), 
              sd_max=sd(Value, na.rm = TRUE)+mean(Value, na.rm = TRUE))
  
  gm_df <- rbind(gm_df,df_temp)
}

gm_df$Cluster = factor(gm_df$Cluster, levels=c('0', '1', '2'))
gm_df$battery = factor(gm_df$battery, levels=c('ADAS', 'MOCA', 'NEUROBAT','ADASMOCA'))
gm_df$Measure = factor(gm_df$Measure, levels = c('Diameter', 'Density', 'AvDegree',
                                                 'AvCC', 'GE'))

cbPalette <- c("#6A4C93", "#00B5AD", "#FFC300")


#plot

gm <- ggplot(gm_df, aes(x=Cluster, y=as.numeric(mean), fill=Cluster)) +
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           linewidth=.3) +      # Thinner 
  geom_errorbar(aes(ymin = as.numeric(mean), ymax = as.numeric(sd_max)), position = position_dodge(), width = 0.2) +
  #geom_signif(comparisons = list(c("CN", "MCI"), c("CN", "AD")), map_signif_level = TRUE) +
  scale_fill_manual(values=cbPalette, name="Cluster") +
  scale_y_continuous(n.breaks=3, expand = expansion(mult = c(0, 0.3))) +
  facet_grid(rows = vars(Measure), cols = vars(battery), scales = "free") +
  xlab("Genetic Cluster") +
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

ggsave(file="./Genetic-Clusters/Results/gm.png", device="png", plot=gm, width=10, height=8)

#GLOBAL METRIC RESULTS (BOXPLOT)-----------------------------------------------

#get list of directories inside Bootstrap_results folder
main_dir <- paste(getwd(), "/Genetic-Clusters/Results", sep="")
dir_ls <- list.dirs(path = main_dir, full.names = TRUE, recursive = FALSE)

#empty dataframe to store results
gm_df <- data.frame(Cluster=character(), battery=character(), Measure=character(), 
                    Value=double()) 

#iterate directories (batteries)
for (dir in dir_ls){
  battery <- tail(unlist(str_split(dir, "/")), n=1) 
  file <- paste(dir, "/cluster/globalMetrics.csv", sep="")
  
  if (battery != "ADASMOCA") {
    
    df_temp <- read.csv(file, header=TRUE, sep=";") %>% rename(GE=AvGE)
    metrics <- c("Diameter", "Density", "AvDegree", "AvCC", "GE")
    df_temp$battery <- battery
    
    df_temp <- df_temp %>% pivot_longer(cols=metrics, names_to="Measure", values_to="Value") %>%
      filter(Measure!="GE")
    gm_df <- rbind(gm_df,df_temp)
  }
}

# Compare groups

#Normality test
#gm_df[, c("Cluster", "battery", "Measure", "Value")] %>% group_by(battery, Measure, Cluster) %>%
#  shapiro_test(Value) %>%
#  add_significance()

#Wilcoxon-Mann-Whitney U test 
stat.test <- gm_df[, c("Cluster", "battery", "Measure", "Value")] %>% group_by(battery, Measure ) %>%
  wilcox_test(Value ~ Cluster, p.adjust.method = "BH",) %>%
  add_significance("p.adj",cutpoints = c(0, 0.001, 0.01, 0.05, 1),
                   symbols = c("***", "**", "*", "ns"))

#Export results 
write.csv(stat.test, "./Results/GlobalMetrics_sig.csv", row.names=FALSE)

stat.test <- stat.test %>% 
  add_xy_position(x = "Cluster", 
                  scales = "free", 
                  step.increase=0.1)


#plot 
gm_df$Cluster = factor(gm_df$Cluster, levels=c('0', '1', '2'))
gm_df$battery = factor(gm_df$battery, levels=c('ADAS', 'MOCA', 'NEUROBAT','ADASMOCA'))
gm_df$Measure = factor(gm_df$Measure, levels = c('Diameter', 'Density', 'AvDegree',
                                                 'AvCC', 'GE'))

cbPalette <- c("#6A4C93", "#00B5AD", "#FFC300")



#plot


gm_bx <- ggplot(gm_df) +
  stat_boxplot(aes(x=Cluster, y=as.numeric(Value)),
               geom = "errorbar", width = 0.3) +
  geom_boxplot(aes(x=Cluster, y=as.numeric(Value), fill=Cluster), 
               colour="black", # Use black outlines,
               linewidth=.3, width=0.5, position = position_dodge(width=0.1),
               outlier.size=1.3, outlier.shape=18) +
  stat_summary(aes(x=Cluster, y=as.numeric(Value)),
               fun = "mean", geom="point", size=0.7, colour="white", fill="white") +
  scale_fill_manual(values=cbPalette, name="Cluster") +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.10))) +
  facet_grid(rows = vars(Measure), cols = vars(battery), scales = "free") +
  xlab("Genetic Clusters") +
  ylab("Value (n=250)") +
  #ggtitle("GT global measures") +
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

bracket.nudge.y <- c(0.1,0.15, 0.2,
                     0.1,0.25, 0.4,
                     0.05,0.08,0.11,
                     0,0.2,
                     
                     0.05,0.15,
                     
                     
                     0.1,0.35,
                     #0,100,
                     
                     
                     0
                     
                     #0,0.075,0.125,
                     #0.05,0.15,0.25,
                     #1,2,3,
                     #0.1,0.2,
                     #600,2000
)



gm_bx <- gm_bx +   stat_pvalue_manual(stat.test, label = "p.adj.signif", 
                                      tip.length = 0, step.increase = 0,
                                      bracket.nudge.y=bracket.nudge.y,
                                      hide.ns = TRUE) 

ggsave(file="./Genetic-Clusters/Results/gm_boxplot_noADASMOCA.png", device="png", plot=gm_bx, width=7, height=7)

#DEGREE CENTRALITY RESULTS --------------------------------------------------------
main_dir <- paste(getwd(), "/Genetic-Clusters/Results", sep="")
dir_ls <- list.dirs(path = main_dir, full.names = TRUE, recursive = FALSE)



#iterate directories (batteries)
for (dir in dir_ls){
  battery <- tail(unlist(str_split(dir, "/")), n=1) 
  file <- paste(dir, "/cluster/DC.csv", sep="")
  
  df_temp <- read.csv(file, header=TRUE, sep=";")
  nodes <- colnames(df_temp)[-c(1,2)]
  df_temp$battery <- battery
  
  dc_stats <- df_temp %>% pivot_longer(cols=nodes, names_to="Test", values_to="Value") %>%
    group_by(cluster, battery, Test) %>%
    summarise(mean=mean(Value, na.rm = TRUE), sd=sd(Value, na.rm = TRUE), 
              sd_min=mean(Value, na.rm = TRUE)-sd(Value, na.rm = TRUE), 
              sd_max=sd(Value, na.rm = TRUE)+mean(Value, na.rm = TRUE))
  
  
  cbPalette <- c("#6A4C93", "#00B5AD", "#FFC300")
  dc_stats$cluster = factor(dc_stats$cluster, levels=c('0', '1', '2'))
  dc_stats$mean <- as.numeric(as.character(dc_stats$mean))
  
  threshold_df <- dc_stats %>%
    group_by(cluster) %>%
    summarise(threshold = mean(mean) + 1.5*sd(mean))
  
  
  #plot
  p <- ggplot(dc_stats, aes(x=reorder_within(Test, mean, cluster) , y=as.numeric(mean), fill=cluster)) + 
    geom_bar(position=position_dodge(), stat="identity",
             colour="black", # Use black outlines,
             linewidth=.3) +      # Thinner 
    geom_errorbar(aes(ymin = mean, ymax = sd_max), position = position_dodge(), width = 0.2) +
    geom_hline(data=threshold_df, aes(yintercept=threshold), colour="black", linewidth=0.8) + 
    coord_flip() + 
    scale_fill_manual(values=cbPalette, name="Cluster") +
    scale_y_continuous(n.breaks=5) +
    facet_grid(rows = vars(cluster), scales = "free") +
    scale_x_reordered() +
    ylab("Degree centrality (DC)") +
    xlab("Test") +
    #ggtitle(paste("Degree centrality (", battery, ")", sep="")) +
    theme_bw() +
    theme(
      panel.grid.major.y = element_blank(),
      plot.title = element_text(hjust = 0.5, size=16, face="bold"),
      axis.text=element_text(size=12),
      axis.title=element_text(size=12,face="bold"),
      strip.text.x = element_text(size = 12, face="bold"),
      strip.text.y = element_text(size = 12, face="bold"),
      strip.background = element_rect(color = "white", fill="white"),
      #legend.title=element_blank(),
      legend.position="bottom"
    )
  
  filename = paste("./Genetic-Clusters/Results/DC_", battery, ".png", sep="")
  ggsave(file=filename, device="png", plot=p, width=5, height=8)
}

#BETWEENNESS CENTRALITY RESULTS ------------------------------------------------
main_dir <- paste(getwd(), "/Genetic-Clusters/Results", sep="")
dir_ls <- list.dirs(path = main_dir, full.names = TRUE, recursive = FALSE)

#iterate directories (batteries)
for (dir in dir_ls){
  battery <- tail(unlist(str_split(dir, "/")), n=1) 
  file <- paste(dir, "/cluster/BC.csv", sep="")
  
  df_temp <- read.csv(file, header=TRUE, sep=";")
  nodes <- colnames(df_temp)[-c(1,2)]
  df_temp$battery <- battery
  
  bc_stats <- df_temp %>% pivot_longer(cols=nodes, names_to="Test", values_to="Value") %>%
    group_by(cluster, battery, Test) %>%
    summarise(mean=mean(Value, na.rm = TRUE), sd=sd(Value, na.rm = TRUE), 
              sd_min=mean(Value, na.rm = TRUE)-sd(Value, na.rm = TRUE), 
              sd_max=sd(Value, na.rm = TRUE)+mean(Value, na.rm = TRUE))
  
  
  cbPalette <- c("#6A4C93", "#00B5AD", "#FFC300")
  bc_stats$cluster = factor(bc_stats$cluster, levels=c('0', '1', '2'))
  bc_stats$mean <- as.numeric(as.character(bc_stats$mean))
  
  threshold_df <- bc_stats %>%
    group_by(cluster) %>%
    summarise(threshold = mean(mean) + 1.5*sd(mean))
  
  
  #plot
  p <- ggplot(bc_stats, aes(x=reorder_within(Test, mean, cluster) , y=as.numeric(mean), fill=cluster)) + 
    geom_bar(position=position_dodge(), stat="identity",
             colour="black", # Use black outlines,
             linewidth=.3) +      # Thinner 
    geom_errorbar(aes(ymin = mean, ymax = sd_max), position = position_dodge(), width = 0.2) +
    geom_hline(data=threshold_df, aes(yintercept=threshold), colour="black", linewidth=0.8) + 
    coord_flip() + 
    scale_fill_manual(values=cbPalette, name="Cluster") +
    scale_y_continuous(n.breaks=5) +
    facet_grid(rows = vars(cluster), scales = "free") +
    scale_x_reordered() +
    ylab("Degree centrality (BC)") +
    xlab("Test") +
    theme_bw() +
    theme(
      panel.grid.major.y = element_blank(),
      plot.title = element_text(hjust = 0.5, size=16, face="bold"),
      axis.text=element_text(size=12),
      axis.title=element_text(size=12,face="bold"),
      strip.text.x = element_text(size = 12, face="bold"),
      strip.text.y = element_text(size = 12, face="bold"),
      strip.background = element_rect(color = "white", fill="white"),
      #legend.title=element_blank(),
      legend.position="bottom"
    )
  
  filename = paste("./Genetic-Clusters/Results/BC_", battery, ".png", sep="")
  ggsave(file=filename, device="png", plot=p, width=5, height=8)
}