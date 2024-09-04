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

# 1 import and clean scraped data ---------------------------------
failure_path = 'data/2_patent_tm_scraping/3_final/failed_to_import.csv'
data.frame(file_path = character()) %>% fwrite(., failure_path)
output = lapply(c("time","siren"), function(scrape_list){
  scrape_dir = paste0('data/2_patent_tm_scraping/2_working/patent_',scrape_list)
  output = lapply(dir(scrape_dir), function(file_name){ 
    
    temp = tryCatch({temp = fread(file.path(scrape_dir, file_name)) %>% .[, c("inventor_name") := NA_character_]
    if ("INVNE" %in% names(temp)){temp[str_trim(INVNE) != "", inventor_name := INVNE]} 
    temp = temp %>% mutate(application_date = as.Date(as.character(DEPD), format = "%Y%m%d"),
                           publication_date = as.Date(as.character(PUBD), format = "%Y%m%d"),
                           application_year = year(application_date), 
                           publication_year = year(publication_date),
                           applicant_name = ifelse(str_trim(DENE) != "",str_cleaning(DENE), NA),
                           type = ifelse(str_trim(NAT) != "",NAT, NA),
                           collection = substr(PUBN,10,11),
                           ipcr_list = gsub("\\s+", " ", IPCR) %>% gsub(" ,",",",.),
                           publication_number = sub(".*<doc-number>([0-9]+(?:\\.[0-9]+)?)</doc-number>.*", "\\1", PUBN),
                           application_number = sub(".*<doc-number>([0-9]+(?:\\.[0-9]+)?)</doc-number>.*", "\\1", DEPN),
                           title = ifelse(str_trim(TIT) != "",str_cleaning(TIT), NA)) %>%
      select(application_number, publication_number,collection, type, title,
             application_date, publication_date, application_year, publication_year, inventor_name, applicant_name, ipcr_list)
    if(scrape_list == 'time'){
      temp = separate_rows(temp,applicant_name, sep = ",\\s*") %>%
        mutate(applicant_name = str_trim(applicant_name))}
    if(scrape_list == 'siren'){temp$siren = gsub("results|.csv", "",file_name)}
    return(temp)},error = function(e){ 
      fread(failure_path) %>% rbind(., file.path(scrape_dir, file_name), use.names = F) %>% fwrite(., failure_path)
      print(file.path(scrape_dir, file_name));
      return(data.frame())})
    return(temp)
  }) %>% rbindlist()})

siren_scraped = output[[2]] %>% 
  .[, .(siren = NA_string_collapse(siren, ",")), by = setdiff(names(.), 'siren')] %>%
  .[, saturated := str_count(siren, ",") >= str_count(applicant_name, ",")]


fwrite(output[[1]] %>% unique(), 'data/2_patent_tm_scraping/2_working/patent_time_scraped_collapsed.csv')
fwrite(siren_scraped, 'data/2_patent_tm_scraping/2_working/patent_siren_scraped_collapsed.csv')

# Firm Names associated with 1 siren code per year / presence of siren numbers in given year ------------------------
period = 1990:2023
siren_numbers = fread('data/2_patent_tm_scraping/1_raw/1_StockUniteLegaleHistorique_utf8.csv',
                      select = c('denominationUniteLegale', 'siren', 'dateDebut', 'dateFin'), colClasses = list(character = "siren")) %>%
  rename(dene = denominationUniteLegale) %>% .[dene != "" & !is.na(year(dateDebut))]

siren_active_years = lapply(period, function(yr){
  temp = siren_numbers[year(dateDebut) <= yr & (yr <= year(dateFin) | is.na(year(dateFin)))] %>%
    .[, c('dateDebut', 'dateFin') := NULL]  %>% .[,year := yr] %>% unique()
}) %>% rbindlist() 

dene_siren_admin = siren_active_years %>% .[, count := .N, by = .(dene, year)] %>%
  .[count == 1] %>%  .[,count := NULL] 

fwrite(dene_siren_admin,"data/2_patent_tm_scraping/2_working/DENE_siren_admin_version.csv")
fwrite(siren_active_years %>% .[,c("count", 'dene') := NULL] %>% unique(),"data/2_patent_tm_scraping/2_working/siren_active_years.csv")


# siren codes attached to one firm name -----------------------------------
# identify which observations have a 1-1 siren - applicant name relationship, filter out applicant names where we already know the siren
# from the admin data, then generate a list of applicant-name siren codes for all years when that siren code is active 

dene_from_admin = fread("data/2_patent_tm_scraping/2_working/DENE_siren_admin_version.csv",  colClasses = list(character = "siren")) %>%
  .[, dummy := 1] %>% .[,siren := NULL]
siren_active_years = fread("data/2_patent_tm_scraping/2_working/siren_active_years.csv", colClasses = list(character = "siren"))

siren_scraped = fread('data/2_patent_tm_scraping/2_working/patent_siren_scraped_collapsed.csv', colClasses = list(character = "siren")) %>%
  .[!grepl(",", applicant_name)] %>% select(applicant_name, application_year, siren) %>% unique() %>% 
  merge(.,dene_from_admin, by.x = c('applicant_name', 'application_year'), by.y = c('dene', 'year'), all = T) %>% 
  .[is.na(dummy)] %>% select(applicant_name, siren) %>% unique() %>% na.omit() %>% 
  merge(.,siren_active_years, by = 'siren')

fwrite(siren_scraped, "data/2_patent_tm_scraping/2_working/DENE_siren_one_to_one.csv")


# combine and perform final cleaning -----------------------------------
dene_siren_one_to_one = fread("data/2_patent_tm_scraping/2_working/DENE_siren_one_to_one.csv", colClasses = list(character = "siren"))
dene_from_admin = fread("data/2_patent_tm_scraping/2_working/DENE_siren_admin_version.csv",  colClasses = list(character = "siren"))
time_scraped = fread('data/2_patent_tm_scraping/2_working/patent_time_scraped_collapsed.csv', colClasses = list(character = "siren"))
siren_scraped = fread('data/2_patent_tm_scraping/2_working/patent_siren_scraped_collapsed.csv', colClasses = list(character = "siren")) %>% select(-saturated)

merge_cols = c("application_number","publication_number","collection")
already_ided = fread('data/2_patent_tm_scraping/2_working/patent_siren_scraped_collapsed.csv', 
                     select= c( merge_cols,'saturated')) %>% 
  .[saturated == T]

## for patent documents that we have not fully identified from the siren scraping, attempt to match using
## admin data and then from other documents 1-1 applicant_name / siren number matches 
final_output = merge(time_scraped, already_ided, by = merge_cols, all.x = T) %>%
  .[,saturated := replace_na(saturated, F)] %>% .[saturated != T] %>% .[,saturated :=NULL] %>%
  merge(., dene_from_admin, by.x = c('applicant_name', 'application_year'),
        by.y = c('dene', 'year'), all.x = T) %>% 
  merge(., dene_siren_one_to_one, by.x = c('applicant_name', 'application_year'), 
        by.y = c('applicant_name', 'year'), all.x = T) %>% .[, siren := ifelse(is.na(siren.x), siren.y, siren.x)] %>%
  .[,c('siren.x', 'siren.y') := NULL]

## collapse the time scraped data back down to the document level (from the document - applicant name level)
final_output = final_output %>% .[, .(siren = NA_string_collapse(siren, ",", T),
                                      applicant_name = NA_string_collapse(applicant_name, ",", T)), 
                                  by = setdiff(names(.), c('siren', 'applicant_name'))]

## merge the time scraped data with siren scraped data and combine their lists of siren codes for each document 
final_output = final_output %>% merge(., siren_scraped, by = setdiff(names(.), c('siren', 'applicant_name')), all = T) %>%
  .[,applicant_name := ifelse(is.na(applicant_name.x), applicant_name.y, applicant_name.x)] %>% 
  .[, c('applicant_name.x', 'applicant_name.y') := NULL] %>% rename_all(~gsub("\\.", "_",.)) %>%
  .[, `:=`(siren_x = str_split(siren_x, ","),siren_y = str_split(siren_y, ","))]  %>% rowwise() %>% 
  mutate(siren = union(siren_x, siren_y) %>% NA_string_collapse(., ",", T),
         siren_x = NULL, siren_y = NULL)

fwrite(final_output, "data/2_patent_tm_scraping/3_final/patent_data.csv")



