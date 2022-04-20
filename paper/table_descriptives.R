
source("shiny/var_list.R")

descriptives <- readRDS("shiny/data.RDS") %>% 
  dplyr::filter(group %in% c("SAID","SIDD","SIRD","MOD","MARD","SIDRD")) %>%
  mutate(group = factor(group,levels=c("T2D","SAID","SIDD","SIRD","MOD","MARD","SIDRD"),ordered=TRUE),
         
         var_names = factor(variable,levels=var_levels,labels=c(names(continuous_vars),names(categorical_vars)))
  ) 


tab <- descriptives %>% 
  dplyr::filter(variable %in% c("hba1c_pct","age_diag","bmi","homa_b","homa_ir")) %>% 
  group_by(asian, group,var_names) %>% 
  summarize(median = median(central),
            q25 = quantile(central,probs=0.25),
            q75 = quantile(central,probs=0.75)) %>% 
  mutate(value = paste0(round(median,1)," [",
                        round(q25,1),", ",
                        round(q75,1),"]")) %>% 
  dplyr::select(asian,group,var_names,value) %>% 
  pivot_wider(names_from=group,values_from=value) %>% 
  arrange(var_names)

write_csv(tab,"paper/table_descriptives.csv")
