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
source('0_input_data/helper_functions.R')

numCores <- 1 # detectCores()  # Detect the number of cores
cl <- makeCluster(numCores)  # Create a cluster
registerDoParallel(cl)  # Register the cluster
clusterExport(cl, exclude_from(ls(), 'cl'))
clusterEvalQ(cl, {lapply(packages,library, character.only = T)})



# import patent data ------------------------------------------------------
file_list = dir('1_patent/results_time')

patent_from_dates_raw = lapply(file_list, function(file){
  file = fread(file.path('1_patent/results_time', file))
  file = file %>% mutate(country = substr(file$PUBN,10,11),
                         across(c(DEPN, PUBN),  ~(sub(".*<doc-number>(\\d+)</doc-number>.*", "\\1", .)))) %>%
  select(country,everything())
}) %>% rbindlist(use.names = T, fill = T)

fwrite(patent_from_dates_raw, '1_patent/patents_from_time.csv')
  
 
file_list = dir('1_patent/results_siren')
patent_from_siren_raw = lapply(file_list, function(file){
  siren_code = substr(file,9, 17)
  file = fread(file.path('1_patent/results_siren', file))
  file = file %>% mutate(siren = siren_code,
                         country = substr(file$PUBN,10,11),
                         across(c(DEPN, PUBN),  ~(sub(".*<doc-number>(\\d+)</doc-number>.*", "\\1", .)))) %>%
    select(siren, country,everything())
}) %>% rbindlist(use.names = T, fill = T)
fwrite(patent_from_siren_raw, '1_patent/patents_from_siren.csv')


# hi ----------------------------------------------------------------------
str_cleaning = function(x){
  x %>% str_trim(.) %>% str_replace(., "&apos;", "'") %>% toupper(.) %>% stri_trans_general(.,"Latin-ASCII")
}
siren_numbers =fread('0_input_data/StockUniteLegaleHistorique_utf8 2.csv',
                     select = c('denominationUniteLegale', 'siren'),  colClasses = list(character = "siren")) %>%
  rename(DENE = denominationUniteLegale)
siren_numbers[, DENE := str_cleaning(DENE)]
siren_numbers = siren_numbers[!duplicated(DENE) & DENE!= ""] 

patents= fread( '1_patent/patents_from_time.csv') 

patents_unnested  = patents %>% rename(DENE_list = DENE) %>%
  mutate(DENE = str_split(str_trim(DENE_list), "\\s*,\\s*")) %>%
  unnest(DENE) %>% as.data.table() 

patents_unnested[, DENE :=  str_cleaning(DENE)]
patents_unnested = patents_unnested %>% merge(.,  siren_numbers, all.x = T)

patents_unnested  =patents_unnested[!DENE == ""]
patents_unnested[, holder_is_inventor := mapply(word_match, DENE, INVNE)]
patents_unnested[, DENE := str_replace(DENE,"&apos;", "'")]
patents_unnested = patents_unnested %>% merge(.,  siren_numbers, all.x = T)



