source("shiny/var_list.R")

asian <- readxl::read_excel("data/Cluster Summaries.xlsx",sheet="Prevalence",range = "A1:M600") %>% 
  dplyr::select(Author, Study,Count,include_prevalence,include_descriptive,asian) %>% 
  mutate(Author = zoo::na.locf(Author)) %>% 
  dplyr::filter(!is.na(asian),include_descriptive == 1)

descriptives <- readRDS("data/descriptives.RDS") %>% 
  dplyr::filter(group %in% c("T2D","SAID","SIDD","SIRD","MOD","MARD","SIDRD")) %>%
  dplyr::filter(!Study %in% c("INSPIRED Ahlqvist","NCRCMDDC Additional"),variable %in% var_levels) %>%
  mutate(label = paste0(Author,"\n",Study),
         group = factor(group,levels=c("T2D","SAID","SIDD","SIRD","MOD","MARD","SIDRD"),ordered=TRUE),
         
         var_names = factor(variable,levels=var_levels,labels=c(names(continuous_vars),names(categorical_vars)))
  ) %>% 
  left_join(asian %>% dplyr::select(-Author),
            by = c("Study")) %>% 
  dplyr::filter(include_descriptive == 1)

saveRDS(descriptives,"shiny/data.RDS")
