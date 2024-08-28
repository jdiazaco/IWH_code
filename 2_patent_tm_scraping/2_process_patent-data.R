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




# Firm Names associated with 1 siren code per year ------------------------
period = 1990:2023


siren_numbers = fread('data/2_patent_tm_scraping/1_raw/1_StockUniteLegaleHistorique_utf8.csv', nrows = 1000,
                select = c('denominationUniteLegale', 'siren', 'dateDebut', 'dateFin'), colClasses = list(character = "siren")) %>%
                rename(dene = denominationUniteLegale) %>% .[dene != "" & !is.na(year(dateDebut))]

output = lapply(period, function(yr){
         temp = siren_numbers[year(dateDebut) <= yr & (yr <= year(dateFin) | is.na(year(dateFin)))] %>%
         .[, c('dateDebut', 'dateFin') := NULL] %>% unique() %>% .[, count := .N, by = .(dene)] %>%
         .[count == 1] %>% .[,year := yr] %>% .[,count := NULL]}) %>%
         rbindlist()


fwrite(output,"data/2_patent_tm_scraping/2_working/DENE_siren_admin_version.csv")







