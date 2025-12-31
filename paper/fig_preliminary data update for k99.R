readxl::read_excel("data/Cluster Summaries.xlsx",sheet="ADA Prevalence") %>% 
  dplyr::filter(include_estimates == "Yes") %>% 
  group_by(Dataset) %>% 
  tally()

count <- readxl::read_excel("data/Cluster Summaries.xlsx",sheet="ADA Prevalence") %>% 
  dplyr::filter(include_estimates == "Yes") %>% 
  mutate(usa = case_when(str_detect(Country,"USA") & duration == "New" ~ "USA New",
                         str_detect(Country,"USA") & duration == "Established" ~ "USA Established",
                         Country == "Global" ~ "Global",
                         TRUE ~ "Others")) %>% 
  group_by(Dataset,usa) %>% 
  tally()
