# setup
rm(list = ls())
packages = c('data.table', 'haven', 'readxl', 'openxlsx', 'stringr', 'readr', 'dplyr',
             'tidyverse', 'janitor', 'Matrix','parallel', 'bigmemory','bit64', 'xml2',
             'doParallel', 'foreach', 'tictoc', 'R.utils', 'stringi')
lapply(packages, function(package){
  tryCatch({ library(package, character.only = T)},error = function(cond){
    install.packages(package);library(package, character.only = T)
  })})
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source('0_helper_functions/helper_functions.R')


# import the siren numbers / set up lists to iterate through  -----------------------------------------------
siren_numbers =fread('0_input_data/StockUniteLegaleHistorique_utf8 2.csv',
                     select = c('denominationUniteLegale', 'siren'),  colClasses = list(character = "siren"))
siren_numbers = siren_numbers[denominationUniteLegale!= ""]
siren_numbers = siren_numbers[!duplicated(siren)]
siren_numbers = siren_numbers %>% select(siren)
siren_numbers$category = numeric()
fwrite(siren_numbers, '1_patent/siren_numbers.csv')
fwrite(siren_numbers, '2_trademark/siren_numbers.csv')



##import date file we need to iterate through
dates = data.frame(start_dates = seq(ymd("1990-01-01"), ymd("2023-01-01"), by="1 weeks"),
                   end_dates = seq(ymd("1990-01-07"), ymd("2023-01-07"), by="1 weeks"),
                   completed = 0)

fwrite(dates,'1_patent/dates.csv')
fwrite(dates,'2_trademark/dates.csv')



# 1 scrape all patent data from the time period ---------------------------------------------------------
type = '1_patent'
keep_going = fread(file.path(type, 'dates.csv')) %>% pull(completed) %>% min() == 0
num_cores = detectCores() - 1

split_dates = ceiling(nrow(df) / x)



while (keep_going){
  login_info = fread('0_input_data/login_info.csv')
  i = which(login_info$availability == 0)[1]
  
  if(is.na(i)){ # if no available nodes just reset those on cool-down and try again.
    login_info$availability = 0
    fwrite(login_info,'0_input_data/login_info.csv')
  } else{
    login_info$availability[i] = 1
    fwrite(login_info,'0_input_data/login_info.csv')
    scraping_iteration_dates_only(login_info$user_name[i],
                                  login_info$password[i], type)
    print(paste('login num:', i))
  }
  keep_going = fread(file.path(type, 'dates.csv')) %>% pull(completed) %>% min() == 0
}


# Scrape all the trademark data from the time period-------------------------------------------
type = '2_trademark'
keep_going = fread(file.path(type, 'dates.csv')) %>% pull(completed) %>% min() == 0
while (keep_going){
  login_info = fread('0_input_data/login_info.csv')
  i = which(login_info$availability == 0)[1]
  
  if(is.na(i)){ # if no available nodes just reset those on cool-down and try again. 
    login_info$availability = 0
    fwrite(login_info,'0_input_data/login_info.csv')
  } else{
    login_info$availability[i] = 1
    fwrite(login_info,'0_input_data/login_info.csv')
    scraping_iteration_dates_only(login_info$user_name[i],
                                  login_info$password[i], type)
    print(paste('login num:', i))
  }
  keep_going = fread(file.path(type, 'dates.csv')) %>% pull(completed) %>% min() == 0
}




# 4 go through all siren codes searching for patents  ---------------------------------------------------------
type = '1_patent'
keep_going = any(is.na(fread(file.path(type,'siren_numbers.csv'))[['category']]))

while (keep_going){
  login_info = fread('0_input_data/login_info.csv')
  chosen_index = which(login_info$availability == 0)[1]
  
  if(is.na(chosen_index)){ # if no available nodes just reset those on cool-down and try again. 
    login_info$availability = 0
    fwrite(login_info,'0_input_data/login_info.csv')
  } else{
    scraping_iteration(login_info$user_name[chosen_index],
                       login_info$password[chosen_index], type)
    login_info$availability[chosen_index] = 1
    fwrite(login_info,'0_input_data/login_info.csv')
    print(paste('login num:', chosen_index))
  }
  keep_going = any(is.na(fread(file.path(type,'siren_numbers.csv'))[['category']]))
}

# 5 go through all siren codes searching for trademarks  ---------------------------------------------------------
type = '2_trademark'
keep_going = any(is.na(fread(file.path(type,'siren_numbers.csv'))[['category']]))

while (keep_going){
  login_info = fread('0_input_data/login_info.csv')
  chosen_index = which(login_info$availability == 0)[1]
  
  if(is.na(chosen_index)){ # if no available nodes just reset those on cool-down and try again. 
    login_info$availability = 0
    fwrite(login_info,'0_input_data/login_info.csv')
  } else{
    login_info$availability[chosen_index] = 1
    fwrite(login_info,'0_input_data/login_info.csv')
    scraping_iteration(login_info$user_name[chosen_index],
                       login_info$password[chosen_index], type)
    print(paste('login num:', chosen_index))
  }
  keep_going = any(is.na(fread(file.path(type,'siren_numbers.csv'))[['category']]))
}

# 4 combine scraped data into unique files  -------------------------------------
file_list = dir('1_patent/results_time')

patent_from_dates_raw = lapply(file_list, function(file){
  file = fread(file.path('1_patent/results_time', file))
  file = file %>% mutate(country = substr(file$PUBN,10,11),
                         across(c(DEPN, PUBN),  ~(sub(".*<doc-number>(\\d+)</doc-number>.*", "\\1", .)))) %>%
    select(country,everything())
}) %>% rbindlist(use.names = T, fill = T) %>% unique()
fwrite(patent_from_dates_raw, '1_patent/patents_from_time.csv')


# 6 compile all the results from patent search --------------------------------------------------------
file_list = dir('1_patent/results_siren')
patents_siren = lapply(file_list, function(file){
  siren_code = substr(file,9, 17)
  file = fread(file.path('1_patent/results_siren', file))
  file = file %>% mutate(siren = siren_code,
                         country = substr(file$PUBN,10,11),
                         across(c(DEPN, PUBN),  ~(sub(".*<doc-number>(\\d+)</doc-number>.*", "\\1", .)))) %>%
    select(siren, country,everything())
}) %>% rbindlist(use.names = T, fill = T)
fwrite(patent_from_siren_raw, '1_patent/patents_from_siren.csv')



patents_siren = fread('1_patent/patents_from_siren.csv')
patents_siren_unnested  = patents_siren %>% rename(DENE_list = DENE) 

patents_siren_unnested = patents_siren_unnested[, DENE := str_split(str_trim(DENE_list), "\\s*,\\s*")]
patents_siren_unnested = patents_siren_unnested[, DENE_list_length := lengths(DENE)] %>% unnest(DENE) %>% as.data.table() 
patents_siren_unnested[, DENE :=  str_cleaning(DENE)]

### MAKE SURE TO HANDLE THE POSSIBILITY OF DUPLICATE OBSERVATIONS (multiple sirens attached to the same patent)


## use the official siren-name list as 1st most reliable  
siren_DENE_official =fread('0_input_data/StockUniteLegaleHistorique_utf8 2.csv',
                           select = c('denominationUniteLegale', 'siren'),  colClasses = list(character = "siren")) %>%
  rename(DENE = denominationUniteLegale)
siren_DENE_official[, DENE := str_cleaning(DENE)]
siren_DENE_official = siren_DENE_official[!duplicated(DENE) & DENE!= ""] 


## in cases where our siren code search returned an entry with one DENE entry;
## assign the applicant to that siren code as 2nd most reliable 
siren_DENE_unique_ids = patents_siren_unnested[DENE_list_length == 1] %>% 
  select(siren, DENE) %>% unique() %>% arrange(siren, DENE)

## now identify which DENE entry was most commonly associated with a given siren code 
## as 3rd most reliable
siren_DENE_dominant_ids <- patents_siren_unnested[, .N, by = .(siren, DENE)][order(-N), .SD[1], by = siren] 

### IF A DENE remains unmatched after all these processes
### assign it to the siren code it was found with as fourth most reliable 

### Finally, use the most similiar matched siren code as our last check




