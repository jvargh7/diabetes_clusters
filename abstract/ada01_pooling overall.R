
ada_df <- readxl::read_excel("data/Cluster Summaries.xlsx",sheet="ADA Prevalence") %>% 
  dplyr::filter(include_pooling == "Yes") %>% 
  dplyr::select(Author:MARD) %>% 
  mutate(Category = case_when(Study == "ANDIS" ~ "Type A1",
                              TRUE ~ Category))

source("C:/code/external/functions/preprocessing/expit.R")

library(metafor)

phenotype = c("SAID","SIDD","SIRD","MOD","MARD")
categories_dd = c("New","Established")
categories_re = c("NH White","NH Asian")
# https://www.metafor-project.org/doku.php/analyses:stijnen2010
diversity <- map_dfr(phenotype,
                                function(p){
                                  
                                  p_df = ada_df;
                                  
                                  if(p!="SAID"){
                                    p_df = ada_df %>% 
                                      mutate(N = case_when(!is.na(SAID) ~ round(N-(SAID*N/100)),
                                                           TRUE ~ N))
                                  }
                                  
                                  p_df = p_df %>% 
                                    dplyr::select(Category,N,duration,include_asian,one_of(p)) %>% 
                                    rename_at(vars(one_of(p)),~"Phenotype") %>% 
                                    mutate(Phenotype = round(N*Phenotype/100)) %>% 
                                    dplyr::filter(!is.na(Phenotype)) %>% 
                                    mutate(prop = Phenotype/N);
                                  
                                  r = rma.glmm(measure="PLO", xi=Phenotype, ni=N, data=p_df)
                                  overall_est = data.frame(strata = "Overall",
                                                           n_study = nrow(p_df),
                                                           n_category = sum(p_df$N),
                                                           n_total = sum(p_df$N),
                                                           beta = as.numeric(r$beta),
                                                           k = r$k,
                                                           se = r$se,
                                                           zval = r$zval,
                                                           pval = r$pval,
                                                           ci.lb = r$ci.lb,
                                                           ci.ub = r$ci.ub,
                                                           tau2 = r$tau2,
                                                           se.tau2 = r$se.tau2,
                                                           sigma2 = r$sigma2,
                                                           I2 = r$I2,
                                                           H2 = r$H2,
                                                           QE.LRT = r$QE.LRT,
                                                           QEp.LRT = r$QEp.LRT,
                                                           QE.Wld = r$QE.Wld,
                                                           QEp.Wld = r$QEp.Wld,
                                                           QE.df = r$QE.df);

                                  d_est = map_dfr(categories_dd,
                                                  function(d){
                                                    d_df = p_df %>% 
                                                      dplyr::filter(duration == d);
                                                    
                                                    r = rma.glmm(measure="PLO", xi=Phenotype, ni=N, data=d_df)
                                                    
                                                    data.frame(strata = d,
                                                               n_study = nrow(d_df),
                                                               n_category = sum(d_df$N),
                                                               n_total = sum(p_df$N),
                                                               beta = as.numeric(r$beta),
                                                               k = r$k,
                                                               se = r$se,
                                                               zval = r$zval,
                                                               pval = r$pval,
                                                               ci.lb = r$ci.lb,
                                                               ci.ub = r$ci.ub,
                                                               tau2 = r$tau2,
                                                               se.tau2 = r$se.tau2,
                                                               sigma2 = r$sigma2,
                                                               I2 = r$I2,
                                                               H2 = r$H2,
                                                               QE.LRT = r$QE.LRT,
                                                               QEp.LRT = r$QEp.LRT,
                                                               QE.Wld = r$QE.Wld,
                                                               QEp.Wld = r$QEp.Wld,
                                                               QE.df = r$QE.df) %>% 
                                                      return(.)
                                                    
                                                  });
                                  
                                  r_est = map_dfr(categories_re,
                                                  function(re){
                                                    r_df = p_df %>% 
                                                      dplyr::filter(include_asian == re);
                                                    
                                                    r = rma.glmm(measure="PLO", xi=Phenotype, ni=N, data=r_df)
                                                    
                                                    data.frame(strata = re,
                                                               n_study = nrow(r_df),
                                                               n_category = sum(r_df$N),
                                                               n_total = sum(p_df$N),
                                                               beta = as.numeric(r$beta),
                                                               k = r$k,
                                                               se = r$se,
                                                               zval = r$zval,
                                                               pval = r$pval,
                                                               ci.lb = r$ci.lb,
                                                               ci.ub = r$ci.ub,
                                                               tau2 = r$tau2,
                                                               se.tau2 = r$se.tau2,
                                                               sigma2 = r$sigma2,
                                                               I2 = r$I2,
                                                               H2 = r$H2,
                                                               QE.LRT = r$QE.LRT,
                                                               QEp.LRT = r$QEp.LRT,
                                                               QE.Wld = r$QE.Wld,
                                                               QEp.Wld = r$QEp.Wld,
                                                               QE.df = r$QE.df) %>% 
                                                      return(.)
                                                    
                                                  });
                                  
                                  
                                  bind_rows(overall_est,
                                            d_est,
                                            r_est)  %>% 
                                    mutate(phenotype = p,
                                           
                                           average_proportion = inv_logit(beta),
                                           average_lci = inv_logit(ci.lb),
                                           average_uci = inv_logit(ci.ub)) %>% 
                                    dplyr::select(strata,everything()) %>% 
                                  return(.)
                                  
                                  
                                })

write_csv(diversity,"abstract/ada01_pooling overall.csv")

tab_diversity = diversity %>% 
  mutate(coef_ci = paste0(round(average_proportion*100,1)," (",
                          round(average_lci*100,1),", ",
                          round(average_uci*100,1),")"),
         n_study = as.character(n_study)) %>% 
  dplyr::select(strata,phenotype,n_study,coef_ci) %>% 
  pivot_longer(cols=c("n_study","coef_ci"),names_to="vars",values_to="vals") %>% 
  pivot_wider(names_from=phenotype,values_from=vals)
write_csv(tab_diversity,"abstract/tab_pooling overall.csv")


z_diversity = diversity %>% 
  dplyr::filter(strata %in% c("NH White","NH Asian","New","Established")) %>% 
  mutate(strata = str_replace(strata,"NH ","")) %>% 
  dplyr::select(strata,phenotype,beta,se) %>% 
  pivot_wider(names_from="strata",values_from=c("beta","se")) %>% 
  mutate(z_duration = abs(beta_New - beta_Established)/sqrt(se_New^2 + se_Established^2),
         z_re = abs(beta_Asian - beta_White)/sqrt(se_Asian^2 + se_White^2)) %>% 
  mutate(p_duration = (1-pnorm(z_duration))*2,
         p_re = (1-pnorm(z_re))*2) 

p_adj = p.adjust(c(z_diversity$p_duration,z_diversity$p_re),method = "BH")

### careful-----------
z_diversity %>%
  mutate(p_duration_adj = p_adj[1:5],
         p_re_adj = p_adj[6:10]) %>% 
  
  mutate(z_p_duration = paste0("z = ",round(z_duration,1),", adj.p = ",round(p_duration_adj,3)),
         z_p_re = paste0("z = ",round(z_re,1),", adj.p = ",round(p_re_adj,3))
         ) %>% 
  dplyr::select(phenotype,z_p_duration,z_p_re) %>% 
  pivot_longer(cols=c("z_p_duration","z_p_re"),values_to="z_p_val",names_to="z_p_var") %>% 
  pivot_wider(names_from=phenotype,values_from="z_p_val") %>% 
  
write_csv(.,"abstract/tab_z diversity.csv")

