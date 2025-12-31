
ada_df <- readxl::read_excel("data/Cluster Summaries.xlsx",sheet="ADA Prevalence") %>% 
  dplyr::filter(include_pooling == "Yes") %>% 
  dplyr::select(Author:MARD) %>% 
  mutate(Category = case_when(Study == "ANDIS" ~ "Type A1",
                              TRUE ~ Category))

source("C:/code/external/functions/preprocessing/expit.R")
# https://www.statology.org/multinomial-test
library(metafor)

phenotype = c("SAID","SIDD","SIRD","MOD","MARD")
categories_md = c("Type A1","Type B","Type C")
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
                       
                       p_est = map_dfr(categories_md,
                                       function(c){
                                         c_df = p_df %>% 
                                           dplyr::filter(Category == c);
                                         
                                         r = rma.glmm(measure="PLO", xi=Phenotype, ni=N, data=c_df)
                                         
                                         data.frame(group = "Overall",
                                                    Category = c,
                                                    n_study = nrow(c_df),
                                                    n_category = sum(c_df$N),
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
                       
                       d1_est = map_dfr(categories_md,
                                       function(c){
                                         c_df = p_df %>% 
                                           dplyr::filter(Category == c,duration=="New");
                                         
                                         if(nrow(c_df)<2){
                                           return(data.frame(group="Duration = New",
                                                             Category = c))
                                         }
                                         
                                         r = rma.glmm(measure="PLO", xi=Phenotype, ni=N, data=c_df)
                                         
                                         data.frame(group = "Duration = New",
                                                    Category = c,
                                                    n_study = nrow(c_df),
                                                    n_category = sum(c_df$N),
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
                       
                       d2_est = map_dfr(categories_md,
                                        function(c){
                                          c_df = p_df %>% 
                                            dplyr::filter(Category == c,duration=="Established");
                                          if(nrow(c_df)<2){
                                            return(data.frame(group="Duration = Established",
                                                              Category = c))
                                          }
                                          r = rma.glmm(measure="PLO", xi=Phenotype, ni=N, data=c_df)
                                          
                                          data.frame(group = "Duration = Established",
                                                     Category = c,
                                                     n_study = nrow(c_df),
                                                     n_category = sum(c_df$N),
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
                       
                       r1_est = map_dfr(categories_md,
                                        function(c){
                                          c_df = p_df %>% 
                                            dplyr::filter(Category == c,include_asian =="NH White");
                                          
                                          if(nrow(c_df)<2){
                                            return(data.frame(group="R/E = NH White",
                                                              Category = c))
                                            }
                                          r = rma.glmm(measure="PLO", xi=Phenotype, ni=N, data=c_df)
                                          
                                          data.frame(group = "R/E = NH White",
                                                     Category = c,
                                                     n_study = nrow(c_df),
                                                     n_category = sum(c_df$N),
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
                       
                       r2_est = map_dfr(categories_md,
                                        function(c){
                                          c_df = p_df %>% 
                                            dplyr::filter(Category == c,include_asian =="NH Asian");
                                          
                                          if(nrow(c_df)<2){
                                            return(data.frame(group="R/E = NH Asian",
                                                              Category = c))
                                          }
                                          
                                          r = rma.glmm(measure="PLO", xi=Phenotype, ni=N, data=c_df)
                                          
                                          data.frame(group = "R/E = NH Asian",
                                                     Category = c,
                                                     n_study = nrow(c_df),
                                                     n_category = sum(c_df$N),
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
                       bind_rows(p_est,
                                 d1_est,
                                 d2_est,
                                 r1_est,
                                 r2_est) %>% 
                       
                         mutate(phenotype = p,
                                
                                average_proportion = inv_logit(beta),
                                average_lci = inv_logit(ci.lb),
                                average_uci = inv_logit(ci.ub)) %>% 
                         dplyr::select(group,Category,n_total,everything()) %>% 
                         return(.)
                       
                       
                     })

write_csv(diversity,"abstract/ada02_pooling by group.csv")



tab_diversity = diversity %>% 
  mutate(coef_ci = paste0(round(average_proportion*100,1)," (",
                          round(average_lci*100,1),", ",
                          round(average_uci*100,1),")"),
         n_study = as.character(n_study)) %>% 
  dplyr::select(group,Category,phenotype,n_study,coef_ci) %>% 
  pivot_longer(cols=c("n_study","coef_ci"),names_to="vars",values_to="vals") %>% 
  pivot_wider(names_from=phenotype,values_from=vals)
write_csv(tab_diversity,"abstract/tab_pooling category.csv")


x2_diversity = diversity %>% 
  mutate(n_phenotype = round(average_proportion*n_category)) %>% 
  dplyr::select(group,Category,phenotype,n_phenotype,n_category) %>%
  dplyr::filter(!is.na(n_phenotype)) %>% 
  group_by(group,phenotype) %>% 
  summarize(x2 = prop.test(n_phenotype,n_category))
  
  pivot_wider(names_from="strata",values_from=c("beta","se")) %>% 
  mutate(z_duration = abs(beta_New - beta_Established)/sqrt(se_New^2 + se_Established^2),
         z_re = abs(beta_Asian - beta_White)/sqrt(se_Asian^2 + se_White^2)) %>% 
  mutate(p_duration = (1-pnorm(z_duration))*2,
         p_re = (1-pnorm(z_re))*2) 
