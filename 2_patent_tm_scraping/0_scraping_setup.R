# setup
rm(list = ls())
packages = c('data.table', 'haven', 'readxl', 'openxlsx', 'stringr', 'readr', 'dplyr',
             'tidyverse', 'janitor', 'Matrix','parallel', 'bigmemory','bit64', 'xml2',
             'doParallel', 'foreach', 'tictoc', 'R.utils', 'stringi','googledrive')
lapply(packages, function(package){
  tryCatch({ library(package, character.only = T)},error = function(cond){
    install.packages(package);library(package, character.only = T)
  })})
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); setwd('../..')


# Make necessary directories  ---------------------------------------------
directories = c("data", 'data/2_patent_tm_scraping',
              paste0('data/2_patent_tm_scraping/', c("1_raw", '2_working', '3_final')),
              paste0('data/2_patent_tm_scraping/2_working/',
              c('patent_time', 'patent_siren', 'tm_time', 'tm_siren')))
for (directory in directories){dir.create(directory)}



# import starting data ----------------------------------------------------
setwd('data/2_patent_tm_scraping/1_raw')
drive_deauth()
drive_user()
public_file <-  drive_get(as_id("10D-4izcUbs5VvLRdyk6gjZLBabsS-gZs"))
drive_download(public_file, overwrite = TRUE)




