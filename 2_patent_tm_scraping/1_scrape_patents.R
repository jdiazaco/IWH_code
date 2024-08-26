# setup
rm(list = ls())
packages = c('data.table', 'haven', 'readxl', 'openxlsx', 'stringr', 'readr', 'dplyr',
             'tidyverse', 'janitor', 'Matrix','parallel', 'bigmemory','bit64', 'xml2',
             'doParallel', 'foreach', 'tictoc', 'R.utils', 'stringi')
lapply(packages, function(package){
  tryCatch({ library(package, character.only = T)},error = function(cond){
    install.packages(package);library(package, character.only = T)
  })})
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); setwd('../..')
source('IWH_code/2_patent_tm_scraping/0_helper_functions/helper_functions.R')
helper_functions = ls() 

# import the siren numbers / set up lists to iterate through  -----------------------------------------------
# siren_numbers =fread('data/2_patent_tm_scraping/1_raw/1_StockUniteLegaleHistorique_utf8.csv',
#                select = c('denominationUniteLegale', 'siren'),  colClasses = list(character = "siren")) %>%
#                .[denominationUniteLegale != ""] %>% .[!duplicated(siren)] %>% select(siren) %>%
#                .[,category := numeric()]
# fwrite(siren_numbers, 'data/2_patent_tm_scraping/2_working/patent_siren_numbers.csv')
# fwrite(siren_numbers, 'data/2_patent_tm_scraping/2_working/tm_siren_numbers.csv')
#
# 
# # ##import date file we need to iterate through
# dates = data.frame(start_dates = seq(ymd("1990-01-01"), ymd("2023-01-01"), by="1 weeks"),
#                    end_dates = seq(ymd("1990-01-07"), ymd("2023-01-07"), by="1 weeks"),
#                    completed = 0)
# 
# fwrite(dates,'data/2_patent_tm_scraping/2_working/patent_dates.csv')
# fwrite(dates,'data/2_patent_tm_scraping/2_working/tm_dates.csv')


DENE_list = lapply(dir('data/2_patent_tm_scraping/2_working/patent_time'), function(stub){
            fread(paste0('data/2_patent_tm_scraping/2_working/patent_time/', stub)) %>% select(DENE)
            }) %>% rbindlist() %>% unique() %>% as.data.table() %>% .[, DENE := str_cleaning(DENE)] %>% 
            separate_rows(DENE, sep = ",") %>% as.data.table() %>% .[, DENE := str_trim(DENE)] %>% unique() 



# 1 scrape all patent data using application date periods ---------------------------------------------------------
type = 'patent'; num_cores = 8


## SET UP THE INPUTS FOR EACH NODE 
dirlist = dir('data/2_patent_tm_scraping/2_working/') %>% .[grepl(paste0(type,"_dates_"),.)] 
if (length(dirlist) ==0){
  dates = fread(paste0('data/2_patent_tm_scraping/2_working/',type,'_dates.csv'))
}else{
  dates = lapply(dirlist,function(stub){
          file_name = paste0('data/2_patent_tm_scraping/2_working/',stub)    
          output = fread(file_name); file.remove(file_name); return(output)}) %>% rbindlist()
  fwrite(dates, paste0('data/2_patent_tm_scraping/2_working/',type,'_dates.csv'))
  
}
dates = dates %>% split(., ceiling(seq_len(nrow(.)) / ceiling(nrow(.) / num_cores)))
for (i in 1:num_cores){fwrite(dates[[i]], paste0('data/2_patent_tm_scraping/2_working/',type,'_dates_',i,'.csv'))}


### RUN THE SCRAPING CODE IN PARALLEL
cl <- makeCluster(num_cores); clusterExport(cl,c(helper_functions,"type"));
clusterEvalQ(cl, {lapply(packages, library, character.only = T)})
parLapply(cl,1:num_cores, function(node_num){
  login_info = fread('IWH_code/2_patent_tm_scraping/0_helper_functions/login_info.csv')
  while((fread(paste0('data/2_patent_tm_scraping/2_working/',type,'_dates_',node_num,'.csv')) %>% pull(completed) %>% min) == 0){
    login_index = which(login_info$availability == 0)[1]
    if(is.na(login_index)){
      login_info$availability = 0
    }else{
      print(paste0("starting: ",login_index))
      user_name = login_info$user_name[login_index];
      password = login_info$password[login_index]
      login_info$availability[login_index] = 1
      scraping_date_version(node_num, user_name, password, type)
    }
  }
})
stopCluster(cl)
    
    




# 2 scrape all patent data using siren codes  -----------------------------
type = 'patent'; num_cores = 8

## SET UP THE INPUTS FOR EACH NODE 
dirlist = dir('data/2_patent_tm_scraping/2_working/') %>% .[grepl(paste0(type,"_siren_numbers_"),.)] 
if (length(dirlist) ==0){
  siren_numbers = fread(paste0('data/2_patent_tm_scraping/2_working/',type,'_siren_numbers.csv'),
                  colClasses = list(character = "siren"))
}else{
  siren_numbers = lapply(dirlist,function(stub){
                  file_name = paste0('data/2_patent_tm_scraping/2_working/',stub)    
                  output = fread(file_name, colClasses = list(character = "siren"))
                  file.remove(file_name); return(output)}) %>% rbindlist()
  fwrite(siren_numbers, paste0('data/2_patent_tm_scraping/2_working/',type,'_siren_numbers.csv'))
}
siren_numbers = siren_numbers %>% split(., ceiling(seq_len(nrow(.)) / ceiling(nrow(.) / num_cores)))
for (i in 1:num_cores){fwrite(siren_numbers[[i]], paste0('data/2_patent_tm_scraping/2_working/',type,'_siren_numbers_',i,'.csv'))}


### RUN THE SCRAPING CODE IN PARALLEL
cl <- makeCluster(num_cores); clusterExport(cl,c(helper_functions,"type"));
clusterEvalQ(cl, {lapply(packages, library, character.only = T)})

parLapply(cl,1:num_cores, function(node_num){
  # node_num = 1
  siren_path = paste0('data/2_patent_tm_scraping/2_working/',type,'_siren_numbers_',node_num,'.csv')
  login_path = 'IWH_code/2_patent_tm_scraping/0_helper_functions/login_info.csv'

  while(fread(siren_path) %>% pull(category) %>% any(is.na(.))){
    login_info = fread(login_path); login_index = which(login_info$availability == 0)[1]
    if(is.na(login_index)){
      login_info %>% mutate(availability = 0) %>% fwrite(., login_path)
    }else{
      login_info$availability[login_index] = 1; fwrite(login_info, login_path);
      print(paste0("starting: ",login_index))
      user_name = login_info$user_name[login_index];
      password = login_info$password[login_index]
      scraping_siren_version(node_num, user_name, password, type)
    }  
  }
})
stopCluster(cl)













