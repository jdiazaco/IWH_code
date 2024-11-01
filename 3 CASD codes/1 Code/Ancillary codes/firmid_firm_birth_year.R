rm(list = ls())
gc()
packages = c('data.table', 'haven', 'readxl', 'openxlsx','stringr', 'readr', 'dplyr', 'tidyverse', 'zoo', 'reshape2','rstudioapi')
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
#Packages loading
invisible(lapply(packages, library, character.only = TRUE))

# set up directories 
setwd('C:/Users/NEWPROD_J_DIAZ-AC/Documents/Reallocation/6 Publish/2 Data/')
raw_dir = "C:/Users/NEWPROD_J_DIAZ-AC/Documents/Reallocation/6 Publish/2 Data/"
raw_dir_public = "C:/Users/Public/1. Microprod/0. Raw data processing/Data/"
output_dir<-"C:/Users/NEWPROD_J_DIAZ-AC/Documents/Reallocation/3 Output/graphs/product breakdown/19.04.24/"

#Bring tools
tools_dir <- 'C:/Users/NEWPROD_J_DIAZ-AC/Documents/Reallocation/6 Publish/1 Code/Tools/'
source(paste0(tools_dir, "description.R"))
source(paste0(tools_dir, "deflate.R"))
source(paste0(tools_dir, "summary stats helper.R"))

start<-2009
end<-2021

##import data 
start = 1994
end = 2021
sbs_data = readRDS('sbs_br_combined.rds') 
sbs_data <- sbs_data %>% select(firmid, year, firm_birth_year)


sbs_data <- sbs_data %>% group_by(firmid) %>% summarise(firm_birth_year=mean(firm_birth_year, na.rm = T))
hist(sbs_data$firm_birth_year)
fwrite(sbs_data, "firm_birth_year")
