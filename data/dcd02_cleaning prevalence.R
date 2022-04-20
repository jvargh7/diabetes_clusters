prevalence <- readxl::read_excel("data/Cluster Summaries.xlsx",sheet="Prevalence",range = "A1:M60") %>% 
  mutate(Author = zoo::na.locf(Author)) %>% 
  pivot_longer(cols=one_of("SAID","SIDD","SIRD","MOD","MARD","SIDRD","MD","UARD","IRD"),names_to="group",values_to="value") %>% 
  dplyr::filter(include_prevalence == 1,!is.na(value),Author != "Ahlqvist 2017")


saveRDS(prevalence,paste0("data/prevalence.RDS"))
