# Setup -------------------------------------------------------------------
rm(list = ls())
library(tidyverse)
library(rstudioapi)
library(data.table)
library(haven)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd('prodfra_concordance_tables')

##Goal 
## Create a consistent set of codes for 2009-2021 by forming disjoint sets of 
## codes that share ancestors / progeny 

## 1) import correspondence tables  -------------------------------------------
  start = 2010
  end = 2021
  
  #import and merge; use a right merge since only want families of codes present in
  #final year 
  for (yr in start:end){
    print(yr)
    dt_temp = fread(paste0('concordances_',yr-1,'_to_',yr,'.csv'))
    dt_temp = unique(dt_temp %>%  select(prodfra_n_1, prodfra_n) %>% mutate_all( ~substring(str_trim(.),1,8)))
  colnames(dt_temp) = c(paste0('prodfra_',yr-1),paste0('prodfra_',yr))
  
  if (yr == start){
    dt = dt_temp
  }else{
    dt = unique(merge(dt,dt_temp, by = paste0('prodfra_',yr-1), all.y = T))
  }
  }
  dt = dt %>% select(sort(colnames(dt)))



## 2) generate an 1:n mapping of final codes to year codes  ------------------
  start = 2009
  end = 2021
  dt$ignorable = T
  
  ## mark rows that we don't need to deal with bc they never share a code value 
  ## with any other row 
    for (yr in start:end){
      prodfra_yr = paste0('prodfra_',yr)
      dt = dt %>% group_by(get(prodfra_yr)) %>% 
        mutate(x = ifelse(!is.na(get(prodfra_yr)),n(),NA), 
               ignorable = ifelse(x==1 | is.na(x),ignorable, F))
      dt = dt %>%select(-x)
    }
    
    unproblematic_codes = dt %>% filter(ignorable) %>% mutate(prodfra_plus = prodfra_2021)
    problematic_codes = as.data.table(dt %>% filter(!ignorable) %>% mutate(prodfra_plus = prodfra_2021))
    problematic_codes$index = 1:nrow(problematic_codes)
    
    
  ## For the rest of rows, generate the harmonized code prodfra_plus by combining groups that have members 
  ## who share common ancestors / progeny 
    i_cleared = F
    for (yr in start:end){
      print(yr)
      year_var = paste0('prodfra_', yr)
        for (i in 1:nrow(problematic_codes)){
          year_value = problematic_codes[[year_var]][i]
          final_value = problematic_codes$prodfra_plus[i]
          
          while (!i_cleared){
            unmatched_indeces = problematic_codes[index>i & get(year_var) == year_value & prodfra_plus != final_value,index]
            if(length(unmatched_indeces) > 0){
              current_l = length(unmatched_indeces); growing =T
              while(growing){
                unmatched_indeces_new = problematic_codes[prodfra_plus %in% problematic_codes$prodfra_plus[unmatched_indeces] |
                                                          get(year_var) %in% problematic_codes[[year_var]][unmatched_indeces]] %>%
                  pull(index)
                new_l =length(unmatched_indeces_new)
                if(new_l > current_l){
                  current_l = new_l; unmatched_indeces = unmatched_indeces_new
                } else{
                  growing = F
                }
              }
              problematic_codes[get(year_var) == year_value | index %in% unmatched_indeces, prodfra_plus := final_value]
            }else{
              break
              }
          }
        }
    }
  

## 3) double check that we performed the harmonization correctly -------------
## each year code should have 1 unique final code 
  for (yr in start:end){
    year_var = paste0('prodfra_',yr)
    temp = problematic_codes %>% group_by(get(year_var)) %>%
      filter(!duplicated(prodfra_plus)) %>%
      mutate(count = ifelse(!is.na(get(year_var)), n(), NA)) %>%
      filter(count>1)
    
    if (nrow(temp) > 0){
      print(paste('error in',yr))
    }
}

## 4) export harmonization output -------------
  harmonized_prodfra = rbind(problematic_codes, unproblematic_codes, fill = T) %>% select(-ignorable, -index)
  write.csv(harmonized_prodfra, '../harmonized codes/prodfra_harmonized_2009to2021.csv', row.names =F)
  
