cover_mean <- cover2 %>% 
  pivot_longer(scleractinian:dead_ac, names_to = "group",values_to = "cover") %>% 
  group_by(sector, site, group) %>% 
  summarise(group_mean=mean(cover)) %>% 
  mutate(coverInv = ifelse(group == "sand" | group == "dead_ac" | group == "rubble",
                           -1*group_mean, group_mean))
ggplot(cover_mean) +
  geom_bar(position="stack", stat="identity", 
           aes(fill=factor(group,levels=c("sponge", "octocoral","halimeda", 
                                          "turf.algae", "algae",
                                          "scleractinian", "cervicornis", 
                                          "dead_ac", "rubble", "sand")), #Group order inside each bar
               y=coverInv, x=site)) +
  scale_fill_manual(values = c("#AA4499","#882255","#CC6677","#117733",
                               "#44AA99","#0077BB","#332288",
                               "#26272C","#4C4E57","#A8AAB3"),
                    labels = c("Sponge", "Octocoral","Halimeda",
                               "Turf algae","Macroalgae","Scleractinian",
                               "A. cervicornis", "Dead coral","Rubble","Sand")) +
  theme_scientific()+
  theme(legend.title = element_blank()) +
  labs(x="", y="Cover (%)")+
  scale_y_continuous(breaks = pretty(cover_mean$coverInv),
                     labels = abs(pretty(cover_mean$coverInv)))+
  facet_grid(~sector)


#Loop de SIMPER

library(purrr)




map(estupidez,function(x){
  slice(x,c(1:4)) %>% 
    rownames_to_column("group") %>% 
    select(group)
  
}) %>% 
  unlist() %>% 
  table() %>% 
  data.frame() %>% 
  rename("group"=".") %>% 
  arrange(desc(Freq)) %>% 
  mutate(Freq_p=round((Freq/55)*100,1))
  

