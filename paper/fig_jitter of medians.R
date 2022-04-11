source("shiny/var_list.R")

descriptives <- readRDS("data/descriptives.RDS") %>% 
  dplyr::filter(group %in% c("SAID","SIDD","SIRD","MOD","MARD","SIDRD")) %>%
  dplyr::filter(!Study %in% c("INSPIRED Ahlqvist","NCRCMDDC Additional"),variable %in% var_levels) %>% 
  mutate(label = paste0(Author,"\n",Study),
         asian = case_when(Study %in% c("CNDMDS","INSPIRED","INSPIRED Female","INSPIRED Male","ICMR-INDIAB","INSPIRED DTT","Fukushima CKD-DEM",
                                        "SAVOR-TIMI 53","Shenzhen Hospital","Jiading Shanghai","Jiading Shanghai 4Y at Baseline","Jiading Shanghai 4Y at 4Y",
                                        "China cities Males","China cities Females","China cities",
                                        "NCRCMDDC Men","NCRCMDDC Women") ~ "Asian",
                           TRUE ~ "Non Asian"),
         group = factor(group,levels=c("T2D","SAID","SIDD","SIRD","MOD","MARD","SIDRD"),ordered=TRUE),
         
         var_names = factor(variable,levels=var_levels,labels=c(names(continuous_vars),names(categorical_vars)))
  ) %>% 
  dplyr::filter(Study %in% c("ANDIS","ADOPT","RECORD","German DS",
                             "INSPIRED","ICMR-INDIAB","INSPIRED DTT",
                             "Fukushima CKD-DEM","Jiading Shanghai","DEVOTE",
                             "LEADER","SUSTAIN-6","Shenzhen Hospital","East London PC",
                             "Swedish Registry","China cities"))

figM <- descriptives %>% 
  dplyr::filter(variable %in% c("hba1c_pct","bmi","age_diag","homa_b","homa_ir")) %>% 
  ggplot(data=.,aes(x=group,y=central,
                    # col = asian,
                    shape=asian,
                    group=asian)) +
  geom_jitter(position = position_dodge(width=0.5)) +
  coord_flip() +
  facet_grid(~var_names,scales="free_x") +
  theme_bw() +
  theme(legend.position = "bottom") +
  # scale_color_manual(name = "",values=c("black","grey20")) +
  scale_shape_manual(name = "",values=c(21,3)) +
  xlab("") + ylab("")


ggsave(figM,filename=paste0(path_heterogeneity_dm,"/figures/fig_jitter of medians.png"),width = 8,height=4)
