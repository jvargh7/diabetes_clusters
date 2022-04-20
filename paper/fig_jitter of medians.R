source("shiny/var_list.R")

descriptives <- readRDS("shiny/data.RDS") %>% 
  dplyr::filter(group %in% c("SAID","SIDD","SIRD","MOD","MARD","SIDRD")) %>%
  mutate(group = factor(group,levels=c("T2D","SAID","SIDD","SIRD","MOD","MARD","SIDRD"),ordered=TRUE),
         
         var_names = factor(variable,levels=var_levels,labels=c(names(continuous_vars),names(categorical_vars)))
  ) 

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
