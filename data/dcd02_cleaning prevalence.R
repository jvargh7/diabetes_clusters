prevalence <- readxl::read_excel("data/Cluster Summaries.xlsx",sheet="Prevalence",range = "A1:M43") %>% 
  mutate(Author = zoo::na.locf(Author)) %>% 
  pivot_longer(cols=one_of("SAID","SIDD","SIRD","MOD","MARD","SIDRD","MD"),names_to="group",values_to="value") %>% 
  dplyr::filter(Strata == 1,!is.na(value),Author != "Ahlqvist 2017")


saveRDS(prevalence,paste0("data/prevalence.RDS"))
