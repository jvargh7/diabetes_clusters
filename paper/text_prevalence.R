prevalence <- readRDS(paste0("data/prevalence.RDS"))

prevalence %>% 
  group_by(asian, group) %>% 
  summarize(median = median(value),
            min = min(value),
            max = max(value),
            n = n())

# Non-SAID, all -------
prevalence %>% 
  dplyr::filter(group != "SAID") %>% 
  group_by(Author,Study,Strata,asian) %>% 
  mutate(value_new = value*100/sum(value)) %>% 
  ungroup() %>% 
  group_by(group) %>% 
  summarize(median = median(value_new),
            min = min(value_new),
            max = max(value_new),
            n = n())


# Non-SAID, by asian ----------
prevalence %>% 
  dplyr::filter(group != "SAID") %>% 
  group_by(Author,Study,Strata,asian) %>% 
  mutate(value_new = value*100/sum(value)) %>% 
  ungroup() %>% 
  group_by(asian, group) %>% 
  summarize(median = median(value_new),
            min = min(value_new),
            max = max(value_new),
            n = n())
