source("shiny/var_list.R")

descriptives <- readRDS("data/descriptives.RDS") %>% 
  dplyr::filter(group %in% c("T2D","SAID","SIDD","SIRD","MOD","MARD","SIDRD")) %>%
  dplyr::filter(!Study %in% c("INSPIRED Ahlqvist"),variable %in% var_levels) %>% 
  mutate(label = paste0(Author,"\n",Study),
         asian = case_when(Study %in% c("CNDMDS","INSPIRED","INSPIRED Female","INSPIRED Male","ICMR-INDIAB","INSPIRED DTT","Fukushima CKD-DEM",
                                        "SAVOR-TIMI 53","Shenzhen Hospital","Jiading Shanghai","Jiading Shanghai 4Y at Baseline","Jiading Shanghai 4Y at 4Y",
                                        "China cities Males","China cities Females","China cities Pooled") ~ "Asian",
                           TRUE ~ "Non Asian"),
         group = factor(group,levels=c("T2D","SAID","SIDD","SIRD","MOD","MARD","SIDRD"),ordered=TRUE),
         
         var_names = factor(variable,levels=var_levels,labels=c(names(continuous_vars),names(categorical_vars)))
         )

saveRDS(descriptives,"shiny/data.RDS")

pdf(file = paste0(path_heterogeneity_dm,"/figures/Descriptives.pdf"),width = 15,height=8)
for(v in names(continuous_vars)){
  
  (descriptives %>% 
     dplyr::filter(variable == continuous_vars[v]) %>% 
     ggplot(data=.,aes(x=central,xmin=lower,xmax=upper,y=label,col=asian)) +
     geom_point() +
     xlab(v) +
     ggtitle(v) +
     geom_errorbar(width=0.2) +
     theme_bw() +
     theme(legend.position = "bottom") +
     scale_color_manual(name="",values=c("red","darkblue")) +
     facet_grid(~group)) %>% 
    print(.)
}

for(v in names(categorical_vars)){
  
  (descriptives  %>% 
     dplyr::filter(variable == categorical_vars[v]) %>% 
     ggplot(data=.,aes(x=percentage,y=label,fill=asian)) +
     geom_col() +
     xlab(v) +
     ggtitle(v) +
     theme_bw() +
     theme(legend.position = "bottom") +
     scale_fill_manual(name="",values=c("red","darkblue")) +
     facet_grid(~group)) %>% 
    print(.)
}

dev.off()




