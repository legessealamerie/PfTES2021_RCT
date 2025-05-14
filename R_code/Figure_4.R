
####  Figure 4 

### density plot for RDT vs Pf18s copy and hrp2/3 concentration

#### load packages
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
library(gridExtra)
library(grid)
library(showtext)
font_add("Times New Roman", regular = "times.ttf")
showtext_auto()

### reading  data 
pfall=read_csv("Pf_TES_data.csv")
#######

pfall$pf18scopyulbd_d0=as.numeric(pfall$pf18scopyulbd_d0)

pfall$pfparperulbd_d0=pfall$pf18scopyulbd_d0/5

rdt_par=pfall%>%filter(rdt_d0!="Missed")

rdt_par=rdt_par%>%mutate(rdt_d0=case_when(rdt_d0=="P. falciparum"~"Positive",
                                          rdt_d0=="Negative"~"Negative"))



means <- rdt_par %>% group_by(rdt_d0) %>% 
  summarise(mean_pf18s = 10^mean(log10(pfparperulbd_d0), na.rm=TRUE))

cols = c( "skyblue1", "tomato")


rdt_par$pf18scopyulbd_d0=as.numeric(rdt_par$pf18scopyulbd_d0)
##rdt_par$pf_paradens_perulbd_d0=(rdt_par$pf18scopyulbd_d0)/5

### RDT
rdt <- ggplot() +
  geom_density(data = rdt_par, aes(x = pfparperulbd_d0, fill = rdt_d0, color = rdt_d0),
               alpha = 0.2, na.rm=T ,linewidth = 0.8) +
  geom_vline(data = means, aes(xintercept = mean_pf18s, color = rdt_d0),
             linewidth = 1.3) +
  scale_x_log10(limits=c(-1,100000),breaks=c(-1,0,1,10,100,1000,10000),label =comma_format(big.mark = ""))+
  scale_y_continuous(breaks=seq(0,0.7,0.1))+
  scale_fill_manual("RDT Result ", values = cols) +
  scale_color_manual("", values = cols) +
  labs(x= "Total  parasites/μL",y ="Density") +
  ggtitle("") +
  labs(title = "a")+
  theme_pubr()+
  guides(fill = F)+theme(
    legend.position = c(0.15, 0.98),
    legend.text = element_text(size = 10)  
  )+theme(legend.box = "none")+theme(legend.text = element_text(size = 12))+
  theme(
    text = element_text(
      family = "Times New Roman",
      size = 12
    )
  )



###hrp2/3 distribution

rdt_par1=rdt_par%>%filter(hrp2_status_d0!=0)
means <- rdt_par1 %>% group_by(rdt_d0) %>% 
  summarise(mean_pf18s = 10^mean(log10(conchrp2_d0), na.rm=TRUE))

cols = c( "skyblue1", "tomato")


### 
hrp2 <- ggplot() +
  geom_density(data = rdt_par1, aes(x = conchrp2_d0, fill = rdt_d0, color = rdt_d0),
               alpha = 0.2, na.rm=T ,linewidth = 0.8) +
  geom_vline(data = means, aes(xintercept = mean_pf18s, color = rdt_d0),
             linewidth = 1.3) +
  scale_x_log10(label =comma_format(big.mark = ""))+scale_y_continuous(limits=c(0,0.6),breaks=seq(0,0.6,0.1))+
  scale_fill_manual("  ", values = cols) +
  scale_color_manual("RDT status ", values = cols) +
  labs(x = "Pfhrp2 gene copies/μL", y = "Density") +
  ggtitle("") +
  labs(title = "b")+
  theme_pubr()+guides(fill = FALSE)+theme(legend.position = "none")+
  theme(
    text = element_text(
      family = "Times New Roman",
      size = 12
    )
  )

### filtering data 
rdt_par2=rdt_par%>%filter(hrp3_status_d0!=0)

##means

means <- rdt_par2 %>% group_by(rdt_d0) %>% 
  summarise(mean_pf18s = 10^mean(log10(conchrp3_d0), na.rm=TRUE))


hrp3 <- ggplot() +
  geom_density(data = rdt_par2, aes(x = conchrp3_d0, fill = rdt_d0, color = rdt_d0),
               alpha = 0.2, na.rm=T ,linewidth = 0.8) +
  geom_vline(data = means, aes(xintercept = mean_pf18s, color = rdt_d0),
             linewidth = 1.3) +
  scale_x_log10(label =comma_format(big.mark = ""))+
  scale_fill_manual("  ", values = cols) +
  scale_color_manual(" ", values = cols) +
  labs(x = "Pfhrp3 gene copies/μL", y = "Density") +
  ggtitle("") +
  labs(title = "c")+
  ylim(0,25)+
  theme_pubr()+guides(fill = FALSE)+theme(legend.position = "none")+theme(
    text = element_text(
      family = "Times New Roman",
      size = 12
    )
  )

# Calculate Pearson correlation with confidence intervals

correlation_data <- rdt_par1 %>%
  group_by(rdt_d0) %>%
  summarise(correlation = cor(conchrp2_d0, pfparperulbd_d0),
            lower_ci = cor.test(conchrp2_d0, pfparperulbd_d0)$conf.int[1],
            upper_ci = cor.test(conchrp2_d0, pfparperulbd_d0)$conf.int[2])

correlation_data <- rdt_par1 %>%
  group_by(rdt_d0) %>%
  summarise(correlation = cor(conchrp2_d0, pfparperulbd_d0, method = "pearson")) %>%
  mutate(correlation = round(correlation, 2))

# Create the ggplot with custom annotations
corrhr <- ggplot(rdt_par1, aes(conchrp2_d0, pfparperulbd_d0, col = rdt_d0, shape = rdt_d0)) +
  scale_shape_manual(values = c(16, 16)) +
  geom_point() +
  labs(title = "d")+
  scale_color_manual(values = cols) +
  geom_vline(xintercept = 0.372, col = "red", lty = 2) +
  geom_hline(yintercept = 100, col = "red", lty = 2) +
  labs(x = "Pfhrp2 gene copies/μL", y = "Total parasites/µL") +
  scale_x_log10(breaks = c(-1, 0, 1, 10, 100, 1000, 10000), labels = trans_format("log10")) +
  scale_x_log10(label = comma_format(big.mark = "")) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) +
  theme_pubr() +
  theme(legend.position = "none") +
  theme(legend.position = "none", text = element_text(family = "Times New Roman", size = 12, face = "plain")) + 
  geom_text(data = NULL, aes(x = 10^0, y = 10^4.7, label = "R = 0.23, p = 0.25, n = 26"), hjust = 0.2, vjust = -1, size = 2.5, color = "skyblue1", fontface = "plain") +
  geom_text(data = NULL, aes(x = 10^0, y = 10^4.3, label = "R = 0.84, p < 0.0001, n = 107"), hjust = 0.18, vjust = -1, size = 2.5, color = alpha("tomato", 0.09), fontface = "plain")


## all together
fig4a_d=ggarrange(rdt,hrp2,hrp3,corrhr, common.legend = F,nrow=2,ncol=2,hjust = -4.5,vjust=1) 



