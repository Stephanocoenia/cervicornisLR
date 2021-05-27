# A. cervicornis surveys to fishers and homestay owners

library(tidyverse)
library(dplyr)
library(patchwork)
library(ggplot2)
library(extrafont)
library(artyfarty)


## Close (yes/no) question surveys - Divergent stacked barplot

survey_close <- read.table("supplements/survey_yn.txt", header=T, dec=",")
survey_close2 <- survey_close %>%  mutate(perInv= ifelse(answer =="yes",percentage,percentage*-1))

# Do you know the ecological function of cervicornis?
p1 <-survey_close2 %>% filter(question=="funct") %>% 
  ggplot(aes(x=stakeholder, y=perInv, fill=answer))+
  geom_bar(stat="identity",position="identity", width = 0.3)+
  xlab("stakeholder")+ylab("Frequency (%5)")+
  scale_fill_manual(name="",values = c("#58355E","#7AE7C7"), labels=c("No", "Yes"))+
  coord_flip()+ggtitle("Ecological Function")+
  geom_hline(yintercept=0)+
  xlab("")+ ylab("Frequency (%)")+
  scale_y_continuous(breaks = pretty(survey_close2$perInv),labels = abs(pretty(survey_close2$perInv)))+
  scale_x_discrete(labels=c("Fishers", "Homestays"))+
  theme_scientific()
p1
# Have you observed changes in their population?
p2 <-survey_close2 %>% filter(question=="changes") %>% 
  ggplot(aes(x=stakeholder, y=perInv, fill=answer))+
  geom_bar(stat="identity", width = 0.3)+
  xlab("stakeholder")+ylab("Frequency (%5)")+
  scale_fill_manual(name="",values = c("#58355E","#bcb8b1","#7AE7C7"), labels=c("No", "No answer", "Yes"))+
  coord_flip()+ggtitle("Changes in the populations?")+
  geom_hline(yintercept=0)+
  xlab("")+ ylab("Frequency (%)")+
  scale_y_continuous(breaks = pretty(survey_close2$perInv),labels = abs(pretty(survey_close2$perInv)))+
  scale_x_discrete(labels=c("Fishers", "Homestays"))+
  theme_scientific()
p2
## Radar charts for open questions 

library(fmsb)

# library(viridis)
# library(hrbrthemes)
# library(colormap)

funct_open <- read.table("funct_open.txt", header=T, dec = ",")
funct_open2 <-select(funct_open, -stakeholder)

# Prepare color

colors_border=c(alpha("#0AB4C3", 1),alpha("#58355E", 1))
colors_in=c(alpha("#0AB4C3", 0.2),alpha("#58355E", 0.2))

# Functional ecology Radar chart
p3<-radarchart( funct_open2, axistype=1, 
            
            #custom polygon
            pcol=colors_border , pfcol=colors_in , plwd=2, plty=1 , 
            
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey",caxislabels=seq(0,60,15),calcex=0.8, cglwd=1,
            
            #custom labels
            vlcex=0.6, vlabels=c("All corals do the same", "Unknown","Food source","Fragile organisms",
                                 "Nursery", "Fish nursery", "Sand formation","Fish shelter", "Lobster shelter")
)

# Legend
legend(x=0.7, y=-0.9, legend = c("Fishers", "Homestays"), bty = "n", pch=20 , col=colors_border , text.col = "black", cex=0.9, pt.cex=2)


