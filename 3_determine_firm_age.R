# setup
rm(list = ls())
packages = c('data.table', 'haven', 'readxl', 'openxlsx', 'stringr', 'readr', 'dplyr',
             'tidyverse', 'janitor', 'Matrix','parallel', 'bigmemory','bit64', 'xml2',
             'doParallel', 'foreach', 'tictoc', 'R.utils')
lapply(packages, function(package){
  tryCatch({ library(package, character.only = T)},error = function(cond){
    install.packages(package);library(package, character.only = T)
  })})
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))



# import ------------------------------------------------------------------

siren_numbers =fread('../3_patent_scraping/0_input_data/StockUniteLegaleHistorique_utf8 2.csv', 
                     colClasses = list(character = "siren"), select = c('siren', 'dateDebut')) 
setorder(siren_numbers, siren, dateDebut)
siren_numbers = siren_numbers[!duplicated(siren)] 
fwrite(siren_numbers, 'siren_debut_dates.csv')