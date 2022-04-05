# raw_descriptives <- readxl::read_excel("data/Cluster Summaries.xlsx",sheet="Descriptives")

# [1] "count"              "percentage"         "mean (sd)"          "mean pm sd"         "count (percentage)" "median (q25, q75)"  "median (q25 - q75)"
# [8] "mean" 

descriptives <- readxl::read_excel("data/Cluster Summaries.xlsx",sheet="Descriptives") %>% 
  pivot_longer(cols=one_of("SAID","SIDD","SIRD","MOD","MARD","SIDRD","MD","T1D","T2D","LADA"),names_to="group",values_to="value") %>% 
  mutate(value = str_replace_all(value,"·",".")) %>% 
  dplyr::filter(!is.na(value)) %>% 
  mutate(
    count = case_when(statistic == "count" ~ value,
                      statistic == "count (percentage)" ~ str_extract(value,"^[0-9]+"),
                      TRUE ~ NA_character_),
    percentage = case_when(statistic == "percentage" ~ value,
                           statistic == "count (percentage)" ~ str_replace_all(str_extract(value,"\\((.*?)\\)"),"(\\(|\\)|%)","")),
    
    mean = case_when(statistic == "mean" ~ value,
                     statistic == "mean (sd)" ~ str_extract(value,"^[0-9\\.·]+"),
                     statistic == "mean pm sd" ~ str_extract(value,"^[0-9\\.·]+"),
                     ),
    
    sd = case_when(statistic == "mean (sd)" ~ str_replace_all(str_extract(value,"\\((.*?)\\)"),"(\\(|\\)|%)",""),
                   statistic == "mean pm sd" ~ str_extract(value,"[0-9\\.]+$")),
    
    median = case_when(statistic == "median (q25, q75)" ~ str_extract(value,"^[0-9\\.]+"),
                       statistic == "median (q25 - q75)" ~ str_extract(value,"^[0-9\\.]+")),
    
    q25 = case_when(statistic == "median (q25, q75)" ~ str_replace(str_extract(value,"\\([0-9\\.]+"),"\\(",""),
                    statistic == "median (q25 - q75)" ~ str_replace(str_extract(value,"\\([0-9\\.]+"),"\\(","")),
    
    q75 = case_when(statistic == "median (q25, q75)" ~ str_replace_all(str_extract(value,"[0-9\\.\\%]+\\)"),"(\\)|\\%)",""),
                    statistic == "median (q25 - q75)" ~ str_replace_all(str_extract(value,"[0-9\\.\\%]+\\)"),"(\\)|\\%)",""))
                    
    ) %>% 
  dplyr::select(Author,Indicator,Study,group,variable,
                count,percentage,mean,sd,median,q25,q75) %>% 
  pivot_longer(cols=one_of("count","percentage","mean","sd","median","q25","q75"),
               names_to="descriptive",values_to="statistic") %>% 
  mutate(statistic = as.numeric(statistic)) %>% 
  dplyr::filter(!is.na(statistic)) %>% 
  mutate(statistic = case_when(variable == "fpg" & str_detect(Indicator,"mmol") ~ statistic*18.0182,
                               # https://www.mdapp.co/blood-sugar-conversion-calculator-71/
                               variable == "hdlc" & str_detect(Indicator,"mmol") ~ statistic*38.67,
                               # http://www.scymed.com/en/smnxtb/tbcbggp1.htm
                               variable == "tgl" & str_detect(Indicator,"mmol") ~ statistic*88.57,
                               # http://www.scymed.com/en/smnxtb/tbcbmhb1.htm
                               variable == "ldlc" & str_detect(Indicator,"mmol") ~ statistic*38.67,
                               # http://www.scymed.com/en/smnxtb/tbcbgwr1.htm
                               variable == "totalc" & str_detect(Indicator,"mmol") ~ statistic*38.67,
                               # http://www.scymed.com/en/smnxtb/tbcbgwr1.htm
                               TRUE ~ statistic
                               )) %>% 
  dplyr::select(-Indicator) 


unique_descriptives <- descriptives %>% 
  dplyr::filter(descriptive %in% c("mean","sd","median","q25","q75")) %>% 
  distinct(Author,Study,group,variable)

descriptives_clean <- map_dfr(1:nrow(unique_descriptives),
                              function(u){
                                a = unique_descriptives[u,]$Author;
                                s = unique_descriptives[u,]$Study;
                                g = unique_descriptives[u,]$group;
                                v = unique_descriptives[u,]$variable;
                                # https://www.researchgate.net/post/Is_there_any_way_to_get_mean_and_SD_from_median_and_IQR_interquartile_range
                                mean_df = tryCatch({descriptives %>% 
                                  dplyr::filter(Author == a,
                                                Study == s,
                                                group == g,
                                                variable == v,
                                                descriptive %in% c("mean","sd")
                                                ) %>% 
                                  pivot_wider(names_from = descriptive,values_from=statistic) %>% 
                                  dplyr::mutate(central = mean,
                                         lower = mean - (sd*1.35)/2,
                                         upper = mean + (sd*1.35)/2) %>% 
                                  dplyr::select(-mean,-sd)},
                              error = function(e){data.frame(Author = a,
                                                             Study = s,
                                                             group = g,
                                                             variable = v,
                                                             central = NA_real_,
                                                             lower = NA_real_,
                                                             upper = NA_real_)});
                                
                                median_df = tryCatch({descriptives %>% 
                                  dplyr::filter(Author == a,
                                                Study == s,
                                                group == g,
                                                variable == v,
                                                descriptive %in% c("median","q25","q75")
                                  ) %>% 
                                  pivot_wider(names_from = descriptive,values_from=statistic) %>% 
                                  dplyr::rename(central = median,
                                         lower = q25,
                                         upper = q75)},
                                  error = function(e){data.frame(Author = a,
                                                                 Study = s,
                                                                 group = g,
                                                                 variable = v,
                                                                 central = NA_real_,
                                                                 lower = NA_real_,
                                                                 upper = NA_real_)});
                                
                                bind_rows(mean_df,
                                          median_df) %>% 
                                  dplyr::filter(!is.na(central)) %>% 
                                  return(.)
                                
                              })


bind_rows(descriptives_clean,
          descriptives %>% 
            dplyr::filter(descriptive %in% c("count","percentage")) %>% 
            pivot_wider(names_from=descriptive,values_from=statistic)) %>% 
  saveRDS(.,paste0("data/descriptives.RDS"))


    