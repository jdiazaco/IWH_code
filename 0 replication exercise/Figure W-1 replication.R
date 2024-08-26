
# setup problem -------------------------------------------------------------------
rm(list = ls())
library(tidyverse)
library(rstudioapi)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# generate biannual data  -------------------------------------------------
raw_data = read.csv("figure W-1.csv")

### Establishment Level 
i = 2006
estab_level_biannual = raw_data %>% filter(year == i | year== i+1) %>%
   distinct(estab_num) %>% mutate(year_l1 = i, year = i+1) %>%
  left_join(raw_data, 
            by = c("estab_num", "year_l1" = "year")) %>%
  replace_na(list(employees = 0))  %>% rename(firm_id_l1= firm_id, employees_l1 =employees) %>%
  left_join(raw_data, by = c("estab_num", "year"))  %>%  replace_na(list(employees = 0)) %>%
  mutate(X = (employees + employees_l1)/2,
         X_total = sum(X), 
         X_share = X/ X_total,
         g =  (employees - employees_l1)/X,
         status =  if_else(g== -2, "died",if_else(g==2,"born","continuer")),
         firm_id = if_else(is.na(firm_id), firm_id_l1, firm_id))   %>%  arrange(estab_num) 

### Firm Level 
firm_level_biennial = estab_level_biannual %>% group_by(firm_id) %>% 
  summarize(g = weighted.mean(g, X, na.rm = TRUE),
            X_share = sum(X_share, na.rm =  TRUE) ) %>% 
  mutate(year_l1 = i, year = i+1,
         biennial= paste(as.character(i), "-",as.character(i+1))) 

# generate growth rate decomps  -------------------------------------------------
## by category at the establishment level 
growth_rate_by_category_estab_level = estab_level_biannual %>% group_by(status) %>% 
  summarize(g = sum(g* X_share))  %>% 
  mutate(year_l1 = i, year = i+1,
         biennial= paste(as.character(i), "-",as.character(i+1) ))

## by category at the firm level
growth_rate_by_category_firm_level = firm_level_biennial %>% mutate(
  status =  if_else(g== -2, "died",if_else(g==2,"born","continuer"))) %>%
  group_by(status) %>%  
  summarize(g = sum(g*X_share))
  



