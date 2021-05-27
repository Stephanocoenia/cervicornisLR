# Other codes that I used for tryouts

#Data

cover <- read.csv("data/cover_groups.csv", sep = ";",dec = ".")  %>% 
  slice(.,-(9:10)) %>%  #remove BQV (onlt 2 transects)
  mutate(site = factor(site,levels = c("HDMS","IF","CAN", "IL", "ESP","NOR", "VEN", "RAB", "MAD", "BM1", "BM2")))  #mutate to reorder locations to match map coordinates


cover.1 <- cover %>%  
  select(-rdead_ac, -odead_ac, -sand, - rubble, -cyano, -other, -millepora)


cover_mean.1 <-  aggregate(cover.1[, 3:10], list(cover.1$site),mean) %>% 
  pivot_longer(scleractinian:turf.algae, names_to = "group",values_to = "cover") %>% 
  rename(site=Group.1) 

as.factor(cover_mean.1$group) #For stacked bars

#Color trials for Community stacked plots 

pal1 <-ggplot(cover_mean.1) +
  geom_bar(position="stack", stat="identity", 
           aes(fill=group, 
               y=cover, x=site)) +
  theme_scientific()+
  theme(legend.title = element_blank()) +
  labs(x="", y="Cover (%)") +
  scale_fill_viridis(discrete = T, option = "turbo")

pal2 <-ggplot(cover_mean.1) +
  geom_bar(position="stack", stat="identity", 
           aes(fill=factor(group,levels=c("sponge", "octocoral","halimeda",
                                          "turf.algae", "algae", "millepora",
                                          "scleractinian", "cervicornis")), 
               y=cover, x=site)) +
  theme_scientific()+
  theme(legend.title = element_blank()) +
  labs(x="", y="Cover (%)") +
  scale_fill_viridis(discrete = T, option = "turbo", direction = -1)


pal3 <- ggplot(cover_mean.1) +
  geom_bar(position="stack", stat="identity", 
           aes(fill=factor(group,levels=c("sponge", "octocoral","halimeda",
                                          "turf.algae", "algae", "millepora",
                                          "scleractinian", "cervicornis")), 
               y=cover, x=site)) +
  theme_scientific()+
  theme(legend.title = element_blank()) +
  labs(x="", y="Cover (%)") +
  scale_fill_manual(values = c("#AA4499","#882255","#CC6677","#117733",
                              "#44AA99","#88CCEE", "#0077BB","#332288")) 
  
  
pal4 <-ggplot(cover_mean.1) +
  geom_bar(position="stack", stat="identity", 
           aes(fill=factor(group,levels=c("sponge", "octocoral","halimeda",
                                          "turf.algae", "algae", "millepora",
                                          "scleractinian", "cervicornis")), 
               y=cover, x=site)) +
  theme_scientific()+
  theme(legend.title = element_blank()) +
  labs(x="", y="Cover (%)") +
  scale_fill_manual(values = c("#AA4499", "#882255", "#CC6677", 
                             "#999933", "#44AA99","#117733", 
                             "#33BBEE", "#0077BB"))

(pal1 + pal2)/(pal3 + pal4)


# Grouped boxplot A. cervicornis live cover, recent and old mortality

cervi <- cover %>% 
  select(site, transect, cervicornis, rdead_ac, odead_ac)

cervi_box <- cervi %>% 
  pivot_longer(c(cervicornis, rdead_ac, odead_ac),names_to = "group",values_to = "perc") 

ggplot(cervi_box, aes(x=site, y=perc, fill=group, alpha=0.6)) + 
  geom_boxplot()+ theme_scientific()+
  scale_fill_grafify(palette = "muted",labels = c("Live cover", "Old mortality", "Recent mortality")) +
  theme(legend.title = element_blank()) +
  labs(x="", y="Cover (%)")

