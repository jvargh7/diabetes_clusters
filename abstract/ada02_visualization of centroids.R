descriptives <- readRDS("shiny/data.RDS")

ada_df <- readxl::read_excel("data/Cluster Summaries.xlsx",sheet="ADA Prevalence") %>% 
  dplyr::filter(include_pooling == "Yes") 

plot_df <- descriptives %>% 
  dplyr::filter(Study %in% ada_df$Study) %>% 
  dplyr::filter(variable %in% c("hba1c_pct","bmi","homa_b","age_diag","homa_ir","ldlc","hdlc","tgl"),!group %in% c("T2D","SAID")) %>% 
  dplyr::select(Author,Study,group,variable,central) %>% 
  pivot_wider(names_from=variable,values_from=central)

library(plotly)

plot_df %>% 
  plot_ly(data=.,x = ~hba1c_pct,y=~bmi,z=~age_diag,color=~group,type="scatter3d",colors = "Set1") %>%
  layout(plot_bgcolor='#e5ecf6',
         scene = list(xaxis = list(title = "HbA1c (%)"), 
                      yaxis = list(title = "BMI (kg/m2)"),
                      zaxis = list(title = "Age at diagnosis")))
plot_df %>% 
  dplyr::filter(!is.na(bmi),!is.na(hba1c_pct),!is.na(age_diag)) %>% 
  distinct(Author, Study,.keep_all=TRUE) %>% 
  View()
