
#### Pf TES 2021 manuscript , Figure 2a_d

#### Loading library
library(ggplot2)
library(survival)
library(survminer)
library(ggplot2)
library(gridExtra)
library(grid)
library(openxlsx)   
library(scales)
library(ggpmisc)
library(labelled)   
library(rstatix)   
library(ggpubr)     
library(ggprism)
library(reshape2)
library(ggpubr)
library(tidyverse)  
library(extrafont)
library(showtext)
font_add("Times New Roman", regular = "times.ttf")
showtext_auto()


### loading data 

par_pcr_clear= read.xlsx("data/pcr_clearance.xlsx")
# Filter the IDs with cleared = 1 and keep them as they are
cleared_pcr1_unique_asex <- par_pcr_clear %>%
  filter(cleared == 1) %>%
  distinct(`Study.ID`, .keep_all = TRUE)

# Filter the IDs with cleared = 0 and keep the last day where cleared == 0 for each ID
cleared_pc0_unique_asex <- par_pcr_clear %>%
  filter(cleared == 0) %>%
  filter(!(`Study.ID` %in% cleared_pcr1_unique_asex$`Study.ID`)) %>%
  group_by(`Study.ID`) %>%
  slice_tail(n = 1) %>%
  ungroup()
# Convert Day to character type before using it in case_when
cleared_pc0_unique_asex <- cleared_pc0_unique_asex %>%
  mutate(
    time = case_when(
      Day == "42" ~ "1",
      
    )
  )%>%
  arrange(as.numeric(time)) %>% 
  mutate(Day = factor(time))    
# Convert the Day column to character type in cleared_pcr1_unique_asex
cleared_pcr1_unique_asex <- cleared_pcr1_unique_asex %>%
  mutate(Day = as.character(Day))

# Convert the Day column to character type in cleared_pc0_unique_asex
cleared_pc0_unique_asex <- cleared_pc0_unique_asex %>%
  mutate(Day = as.character(Day))
unique_ids <- bind_rows(cleared_pcr1_unique_asex, cleared_pc0_unique_asex)
unique_ids $ Day = as.numeric(unique_ids$Day)

### i want to do for each site
unique_ids$day_To_transformed1 <- unique_ids$Day  %>% 
  recode("1" = "1","2"="2", "3" = "3", "7" = "4")
unique_ids$day_To_transformed1 = as.numeric(unique_ids$day_To_transformed1)
########################
# Create a survival object  
surv_obj <- Surv(unique_ids$day_To_transformed1, unique_ids$cleared)  

# Perform the log-rank test  
logrank_result <- survdiff(surv_obj ~ unique_ids$arm)  

# Fit the survival model and create the ggsurvplot
km6a <- survfit(Surv(day_To_transformed1, cleared) ~ arm, data = unique_ids)
survout6a <- survminer::ggsurvplot(km6a, conf.int = FALSE, break.time.by = 1, risk.table = TRUE, risk.table.y.text.col = FALSE,
                                   xlim = c(0, 4), ncensor.plot = TRUE,censor.size = 1, size = 0.3, risk.table.fontsize = 0.2, pch=0.1, ris.table.height = 0.2,
                                   pval = "Log-rank p=0.66",pval.size=3,
                                   surv.median.line = "hv",pval.method.coord=T,
                                   pval.coord = c(0, 0.2),linetype = c( "solid","dashed"))
dt6a <- survout6a$data.survplot

# Define custom labels for x-axis
custom_labels <- c(0, 1, 2, 3, 7)
c("tomato", "skyblue")
# Modify the plot appearance
plot_only_par<- survout6a$plot +
  xlab("Days") +
  #scale_y_continuous(breaks=c(0.0,0.2,0.4,0.6,0.8,1.0),labels = function(x) paste0(x , "")) +
  scale_y_continuous(breaks=c(0.0,0.2,0.4,0.6,0.8,1.0),labels = scales::number_format(accuracy = 0.1, scale = 1)) +
  ylab("Proportion of parasite positive") +
  #ylab("") +
  scale_x_continuous(limits = c(0, 4), breaks = c(0, 1, 2, 3, 4), labels = custom_labels) +
  coord_cartesian(clip = "off") +
  theme_pubr() +
  labs(title = "a")+
  theme(legend.position = "none",
        axis.line = element_line(size = 0.2),  # Adjust axis line size
        axis.ticks = element_line(size = 0.15),  # Adjust the length of the ticks here
        axis.ticks.length = unit(0.05, "cm"),  # Adjust the length of the ticks here
        strip.background = element_blank(),
        axis.title =element_text(size = 8),
        axis.text.x = element_text(size = 8),  # Adjust x-axis label size
        axis.text.y = element_text(size = 8)  # Adjust y-axis label size
  ) + 
  scale_color_manual(values = c("red", "darkblue"), labels = c( "", "")) +
  scale_fill_manual(values = c("red", "darkblue"),, labels = c( "", "")) 
  

###### risk table only
km6a <- survfit(Surv(day_To_transformed1, cleared) ~ arm, data = unique_ids)
survout6a <- survminer::ggsurvplot(km6a, conf.int = FALSE, break.time.by = 1, risk.table = TRUE, risk.table.y.text.col = FALSE,
                                   xlim = c(0, 4), ncensor.plot = TRUE,censor.size = 1, size = 0.3, risk.table.fontsize = 0.25, pch=0.1, ris.table.height = 0.2)
dt6a <- survout6a$data.survplot

# Define custom labels for x-axis
custom_labels <- c(0, 1, 2, 3, 7)
risk_table=  survout6a$data.survtable [survout6a$data.survtable$time %in% c(0:4),] %>%
  ggplot(aes(x = time, y = strata, label = n.risk)) +
  #geom_text(size = 3, color = "black") +  # Adjust the size here to control the text size
  geom_text(data = survout6a$data.survtable, aes(x = time, y = strata, label = paste0(n.risk, "(", n.censor , ")")), vjust = 0.5, size =3) +
  scale_y_discrete(labels = c( "AL+PQ", "PA+PQ")) +
  theme_pubr() +
  theme(axis.title.y = element_blank()) +
  theme(
    axis.title.y = element_blank(),
    plot.title = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line = element_blank(),
    axis.text.y = element_text(colour = c("tomato", "skyblue"), size = 8)) +
  #axis.text.y = element_text(colour = c("#FBB4AEFF", "#B3CDE3FF", "#CCEBC5FF", "#DECBE4FF", "#FED9A6FF"), size = 10)) +
  xlab("Time (days)") +
  scale_x_continuous(limits = c(0, 4), breaks = c(0, 1, 2, 3, 4), labels = custom_labels) 
########
km6a <- survfit(Surv(day_To_transformed1, cleared) ~ arm, data = unique_ids)
survout6a <- survminer::ggsurvplot(km6a, conf.int = FALSE, break.time.by = 1, risk.table = TRUE, risk.table.y.text.col = FALSE,
                                   xlim = c(0, 4), ncensor.plot = TRUE,censor.size = 1, size = 0.2, risk.table.fontsize = 0.2, pch=0.1, ris.table.height = 0.2)
dt6a <- survout6a$data.survplot

# Define custom labels for x-axis
custom_labels <- c(0, 1, 2, 3, 7)
censor=  survout6a$ncensor.plot +
  labs(x = "", y = "censor") +
  #labs(x = "", y = "") +
  scale_x_continuous(limits = c(0, 4), breaks = c(0:4), labels = custom_labels) +
  ##scale_x_continuous(limits = c(0, 28), breaks = c(0:3, 7,14,21,28)) +
  # scale_x_continuous(limits = c(0, 7), breaks = c(0, 1, 2, 3, 4, 5, 6, 7), labels = custom_labels) 
  theme_pubr() +
  theme(legend.position = "top",
        legend.text = element_text(size = 6),
        plot.title = element_blank(),
        axis.line = element_line(size = 0.1),
        legend.box = "none") +
  theme(legend.position = "none") +  # Remove the legend
  guides(fill = guide_legend(title = NULL)) +  # Exclude legend title for fill scale
  scale_color_manual(values = c("tomato", "skyblue")) +
  scale_fill_manual(values = c("tomato", "skyblue"))


### qPCR survival 
pcr_surv=read.csv("data/Figure_2b.csv",header=T) # data for figure 2b

#################################
pcr_surv$day_To_transformed1 <- pcr_surv$day  %>% 
  recode("1" = "1","2"="2", "3" = "3", "7" = "4","14"="5")
pcr_surv$day_To_transformed1 = as.numeric(pcr_surv$day_To_transformed1)

########################################
# Create a survival object  
surv_obj <- Surv(pcr_surv$day_To_transformed1, pcr_surv$status)  

# Perform the log-rank test  
logrank_result <- survdiff(surv_obj ~ pcr_surv$arm)  

#############################

km6b <- survfit(Surv(day_To_transformed1, status) ~ arm, data = pcr_surv)
survout6b <- survminer::ggsurvplot(km6b, conf.int = FALSE, break.time.by = 1, risk.table = TRUE, risk.table.y.text.col = FALSE,
                                   xlim = c(0, 5), ncensor.plot = TRUE,censor.size = 1, size = 0.3, risk.table.fontsize = 0.2, pch=0.1, ris.table.height = 0.2,title = "b", size = 0.2,pval = "Log-rank p=0.58",pval.size=3,
                                   surv.median.line = "hv",pval.method.coord=T,
                                   pval.coord = c(0, 0.2),linetype = c( "solid","dashed"),legend.labs = c("AL+PQ", "PA+PQ"),legend.title="Arm")
dt6b <- survout6b$data.survplot

# Define custom labels for x-axis
custom_labels <- c(0, 1, 2, 3, 7,14)

# Modify the plot appearance
plot_only_par_pcr<- survout6b$plot +
  xlab("Days") +
  #scale_y_continuous(breaks=c(0.0,0.2,0.4,0.6,0.8,1.0),labels = function(x) paste0(x , "")) +
  scale_y_continuous(breaks=c(0.0,0.2,0.4,0.6,0.8,1.0),labels = scales::number_format(accuracy = 0.1, scale = 1)) +
  ylab("Proportion of parasite positive") +
  #ylab("") +
  scale_x_continuous(limits = c(0, 5), breaks = c(0, 1, 2, 3, 4,5), labels = custom_labels) +
  coord_cartesian(clip = "off") +
  theme_pubr() +
  labs(title = "b")+
  theme(#legend.position = "none",
        axis.line = element_line(size = 0.2),  # Adjust axis line size
        axis.ticks = element_line(size = 0.15),  # Adjust the length of the ticks here
        axis.ticks.length = unit(0.05, "cm"),  # Adjust the length of the ticks here
        strip.background = element_blank(),
        axis.title =element_text(size = 8),
        axis.text.x = element_text(size = 8),  # Adjust x-axis label size
        axis.text.y = element_text(size = 8)  # Adjust y-axis label size
  ) +
  scale_color_manual(values = c("red", "darkblue"), labels = c( "AL+PQ", "PA+PQ")) +
  scale_fill_manual(values = c("red", "darkblue"),, labels = c( "AL+PQ", "PA+PQ")) +
  theme(
    legend.position = c(0.9, 0.8),  # Preferred x and y coordinates of the legend
    #legend.margin = margin(0.2, 0.5, 0.2, 0.5),  # Set margin to 0 to place the legend exactly at the specified coordinates
    # legend.box = "horizontal",  # Legend box type (horizontal or vertical)
    #legend.background = element_rect(fill = "white", color = "black"),  # Background color and border of the legend box
    #legend.title = element_text(face = "bold"),  # Legend title appearance
    legend.text = element_text(size = 10)  # Legend text appearance
  )+theme(legend.box = "none")+theme(legend.text = element_text(size = 10)) 
## risk table only
km6b <- survfit(Surv(day_To_transformed1, status) ~ arm, data = pcr_surv)
survout6b <- survminer::ggsurvplot(km6b, conf.int = FALSE, break.time.by = 1, risk.table = TRUE, risk.table.y.text.col = FALSE,
                                   xlim = c(0, 5), ncensor.plot = TRUE,censor.size = 1, size = 0.2, risk.table.fontsize = 0.25, pch=0.1, ris.table.height = 0.2)
dt6b <- survout6b$data.survplot

# Define custom labels for x-axis
custom_labels <- c(0, 1, 2, 3, 7,14)
risk_table_pcr=  survout6b$data.survtable [survout6b$data.survtable$time %in% c(0:5),] %>%
  ggplot(aes(x = time, y = strata, label = n.risk)) +
  #geom_text(size = 3, color = "black") +  # Adjust the size here to control the text size
  geom_text(data = survout6b$data.survtable, aes(x = time, y = strata, label = paste0(n.risk, "(", n.censor , ")")), vjust = 0.5, size =3) +
  scale_y_discrete(labels = c( "AL+PQ", "PA+PQ")) +
  theme_classic() +
  theme(axis.title.y = element_blank()) +
  theme(
    axis.title.y = element_blank(),
    plot.title = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line = element_blank(),
    axis.text.y = element_text(colour = c("tomato", "skyblue"), size = 8)) +
  #axis.text.y = element_text(colour = c("#FBB4AEFF", "#B3CDE3FF", "#CCEBC5FF", "#DECBE4FF", "#FED9A6FF"), size = 10)) +
  xlab("Time (days)") +
  scale_x_continuous(limits = c(0, 5), breaks = c(0, 1, 2, 3,4, 5), labels = custom_labels)

######
km6b <- survfit(Surv(day_To_transformed1, status) ~ arm, data = pcr_surv)
survout6b <- survminer::ggsurvplot(km6b, conf.int = FALSE, break.time.by = 1, risk.table = TRUE, risk.table.y.text.col = FALSE,
                                   xlim = c(0, 5), ncensor.plot = TRUE,censor.size = 1, size = 0.2, risk.table.fontsize = 0.2, pch=0.1, ris.table.height = 0.2)
dt6b <- survout6b$data.survplot
# Define custom labels for x-axis
custom_labels <- c(0, 1, 2, 3, 7,14)
censor_pcr=  survout6b$ncensor.plot +
  labs(x = "", y = "censor") +
  #labs(x = "", y = "") +
  scale_x_continuous(limits = c(0, 5), breaks = c(0:5), labels = custom_labels) +
  ##scale_x_continuous(limits = c(0, 28), breaks = c(0:3, 7,14,21,28)) +
  # scale_x_continuous(limits = c(0, 7), breaks = c(0, 1, 2, 3, 4, 5, 6, 7), labels = custom_labels) 
  theme_classic() +
  theme(legend.position = "top",
        legend.text = element_text(size = 6),
        plot.title = element_blank(),
        axis.line = element_line(size = 0.1),
        legend.box = "none") +
  theme(legend.position = "none") +  # Remove the legend
  guides(fill = guide_legend(title = NULL)) +  # Exclude legend title for fill scale
  scale_color_manual(values = c("tomato", "skyblue")) +
  scale_fill_manual(values = c("tomato", "skyblue"))


#####change ggsurvplot in to ggplotGrop
micplot=ggplotGrob(plot_only_par)
mic_table=ggplotGrob(risk_table)
pcrplot=ggplotGrob(plot_only_par_pcr)
pcr_table=ggplotGrob(risk_table_pcr)


all =grid.arrange(micplot,pcrplot,mic_table,pcr_table,nrow = 2, ncol = 2, heights = c(0.8,0.3))

### figure 2c and Figure 2d
### reading data 

pfall=read_csv("Pf all analysis_2023_20_02_new_data .csv")


asex=pfall%>%
  dplyr:: select("arm",starts_with('pfasecdens_d'))

names(asex)=c("Arm","0","1","2","3","7","14","28","35","42")
asex=asex%>%
  dplyr::select("Arm","0","1","2","3","7","14")

### changing data in to long form 
asex=melt(asex,id="Arm")
parc=pfall%>%
  dplyr:: select("arm" ,starts_with('pf18scopyulbd'))

names(parc)=c("Arm","0","1","2","3","7","14")

### changing data in to long form 

parc=melt(parc,id="Arm")


parc1 <- parc %>%
  mutate(result = ifelse(value > 2.9,"Positive","Negative"))


parc$value=as.numeric(parc$value)
### violine plot



####
parc$value=as.numeric(parc$value)

summary(parc$value)
parc=parc%>%filter(value>=3)
summ_par=parc %>%  group_by(variable,Arm) %>% summarise(n=n(), avg = max(value,na.rm=T)) 

pf18=ggviolin(parc,  "variable", "value",  color = "Arm",
              palette = c("tomato", "skyblue"), add = c("boxplot","jitter",jitter.width = 0.1,                # Jitter width (reduces spread)
                                                        jitter.size = 0.1,stroke=0.15,            # Smaller points
                                                        jitter.alpha = 0.1), size = 0.25)+scale_y_log10(label =comma_format(big.mark = "")) +
  labs(x="Time (days)",title = "d",
       y="Total parasites/µL")+theme_classic()+scale_y_log10(breaks=c(1,10,100,1000,10000,100000,1000000),limits = c(1, NA),
                                                             labels = trans_format("log10", math_format(10^.x)))+
  theme(legend.position = "bottom")+
  stat_summary(aes(group=Arm), fun.y=median, geom="point",
               fill="black", shape=21, size=0.5, position = position_dodge(width = .9))+
  theme(legend.position = "none") +
  theme_pubr() +
  theme(axis.title = element_text(size = 8),
       axis.line = element_line(size = 0.2),
        axis.text = element_text(size = 8,face = "bold"),
        axis.ticks = element_line(size = 0.1),
        axis.ticks.length = unit(0.05, "cm")) +

  guides(color = "none", fill = "none") 

########### median test pf18s vs arm

asex$value=as.numeric(asex$value)
summary(asex$value)

sum_asex=asex %>%  group_by(variable,Arm) %>% summarise(n=n(), avg = max(value,na.rm=T)) 

ase=ggviolin(filter(asex,variable!=7,variable!=14),  "variable", "value",  color = "Arm",
             palette = c("tomato", "skyblue"), add =c("boxplot","jitter",jitter.width = 0.1,                # Jitter width (reduces spread)
                                                      jitter.size = 0.1, stroke=0.15,          # Smaller points
                                                      jitter.alpha = 0.1 ),size=0.25)+
  scale_y_log10(label =comma_format(big.mark = "")) +
  labs(x="Time (days)",title = "c",
       y="Asexual parasites/µL")+theme_classic()+
  scale_y_log10(breaks = c(1,10,100,1000,10000,100000,1000000),limits = c(1, 1000000),
                                                               labels = trans_format("log10", math_format(10^.x)))+  theme(legend.position = "bottom")+
  stat_summary(aes(group=Arm), fun.y=median, geom="point",
fill="black", shape=21, size=0.5,face = "bold", position = position_dodge(width = .9))+theme(legend.position = "none") +
  theme_pubr() +
  theme( #legend.position = "none",
    axis.title = element_text(size=8),
        axis.line = element_line(size=0.2),
        #axis.text.y =  element_text(size=8,face = "bold"),
        axis.text = element_text(size=8,face = "bold"),
        axis.ticks = element_line(size = 0.1),  # Adjust the length of the ticks here
        axis.ticks.length = unit(0.05, "cm")
       )  +
  
  guides(color = "none", fill = "none") 
  #### merge figurs
## figure 2c and 2d
fig2_cd=grid.arrange(ase,pf18,nrow = 1,ncol = 2)

##arrange all figure together
fig2a_d=ggarrange(all,C_D, ncol = 1,nrow=2)

