count <- readxl::read_excel("data/Cluster Summaries.xlsx",sheet="ADA Prevalence") %>% 
  dplyr::filter(include_estimates == "Yes") %>% 
  group_by(Country) %>% 
  tally() %>% 
  arrange(n)

fig_geography <- count %>% 
  ggplot(data=.,aes(x=Country,y=n,label=n)) +
  geom_bar(stat="identity",position = position_dodge()) +
  geom_text(aes(y=n+1)) +
  scale_x_discrete(limits=count$Country[order(-count$n)]) +
  theme_bw() +
  xlab("") +
  ylab("Count") +
  scale_y_continuous(breaks=seq(0,14,by=2))


fig_geography %>% 
  ggsave(.,filename=paste0(path_transportability_phenotypes,"/figures/fig_geography.png"),width=12,height=4)
