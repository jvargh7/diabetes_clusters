descriptives <- readRDS("shiny/data.RDS")

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




