prevalence <- readRDS(paste0("data/prevalence.RDS")) %>% 
  mutate(label = paste0(Author,"\n",Study),
         group = factor(group,levels=c("SAID","SIDD","SIRD","MOD","MARD","SIDRD","MD"),ordered=TRUE))


figP <- prevalence %>% 
  ggplot(data=.,aes(x=group,y=label,fill=value,label=sprintf("%0.1f",value))) +
  facet_wrap(~asian,nrow = 1,ncol=2,scales = "free_y") +
  geom_tile() +
  geom_text() +
  scale_fill_gradient(name = "",limits=c(0,100),low="white", high="black") +
  theme_bw() +
  xlab("") +
  ylab("") +
  theme(legend.position = "bottom") 

ggsave(figP,filename = paste0(path_heterogeneity_dm,"/figures/fig_prevalence.png"),height = 8,width=10)
