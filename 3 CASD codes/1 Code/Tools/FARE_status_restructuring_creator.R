# libraries

library(data.table)
library(haven)
library(readxl)
library(openxlsx)
library(stringr)
library(readr)
library(dplyr)

# setwd("C:/Users/Public/1. Microprod/0. Raw data processing/")
setwd("C:/Users/NEWPROD_J_DIAZ-AC/Documents/Raw_data/Data/")



start = 2009
end = 2021

## import BR data
br_data = rbindlist(lapply(c(start:end),function(yr){
  print(yr)
  ## import BR data
  br_path = paste0("br/br",yr,".csv" )
  br_data_temp = fread(br_path)
  br_data_temp[,NACE_BR := NACE_M]
  br_data_temp[, `:=`(firmid = as.character(ENT_ID))]
}), fill = T)

br_data<-br_data %>% select(firmid, year, status, restructuring)

write_rds(br_data, "C:/Users/NEWPROD_J_DIAZ-AC/Documents/Reallocation/6 Publish/2 Data/br_status_restructuring.RDS")