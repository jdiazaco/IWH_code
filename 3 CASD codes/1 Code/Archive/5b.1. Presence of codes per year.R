"

This file creates xlsx tables with the codes present in the EAP panel at the NACE 2 digit and 4 digit level for each year.
These tables also include the 8-digit prodcom and 10-digit prodfra list of codes present in the panel for each year.
I use two different sources: 
1. The EAP product-firm panel including all prodfra codes regardless of their status with respect to prodcom in the EAP
2. The EAP product-firm panel including only prodfra codes within prodcom.
The process creates 4 tables: by source (all prodfra vs. only prodfra in prodcom) and by code disaaggregation (8 digit v. 10 digit).

Last edit: 04-04-2024
Author: Julián Díaz

"


# setup -------------------------------------------------------------------
# libraries
rm(list = ls())
gc()
packages = c('data.table', 'haven', 'readxl', 'openxlsx','stringr', 'readr', 'dplyr', 'tidyverse', 'zoo', 'reshape2','rstudioapi', "plm", 'foreign', "fixest", 'data.table', 'haven', 'stringr', 'readr', 'dplyr',
             'ggplot2', 'tidyverse', 'rstudioapi', 'zoo', 'reshape2',
             'patchwork', 'latex2exp', "RColorBrewer")
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
#Packages loading
invisible(lapply(packages, library, character.only = TRUE))

# set up directories 
raw_dir = "C:/Users/Public/1. Microprod/0. Raw data processing/Data/"
setwd('C:/Users/NEWPROD_J_DIAZ-AC/Documents/Reallocation/2 Data/reallocation_construction_output/Product breakdown/')
output_dir<-"C:/Users/NEWPROD_J_DIAZ-AC/Documents/Reallocation/3 Output/graphs/product breakdown/05.04.24/"

##import tools
tools_dir <- 'C:/Users/NEWPROD_J_DIAZ-AC/Documents/Reallocation/1 Code/Product decomposition/Tools/'
source(paste0(tools_dir, "description.R"))

# set time parameters
start<-2009
end<-2020
interval<-c(start:end)

for(pc in c("pcc8", "prodfra")){
  for(ind1 in c(TRUE, FALSE)){
    # for(ind2 in c(TRUE, FALSE)){
      print(pc)
      print(ind1)
      # print(ind2)
      
      # Set parameters for prodfra-pcc8 and energy
      prodfra_or_pcc8<-pc
      only_prodfra_in_prodcom<-ind1
      # exclude_energy<-ind2
      filter<-paste0(if(prodfra_or_pcc8=="prodfra") "10_digit" else if(prodfra_or_pcc8=="pcc8") "8_digit" else prodfra_or_pcc8, 
                     "_", 
                     if(only_prodfra_in_prodcom) "only_prodfra_in_prodcom" else "all_prodfra")
      
      # Set descriptions for tables
      description_digits<- if(prodfra_or_pcc8=="pcc8") "8 digit codes" else "10 digit codes"
      description_PF_PC <- if(only_prodfra_in_prodcom) "all prodfra codes" else "only prodfra codes in prodcom"
      # description_energy<-if(exclude_energy) "excluding energy" else "including energy"
      
      ##Import product data and adjust relevant variables
      product_data<-readRDS(paste0("product_level_growth_jul_", filter,  "_.RDS"))
      product_data$NACE_4digits<-substr(product_data$prodfra_plus, 1,4)
      product_data$NACE_2digits<-substr(product_data$prodfra_plus, 1,2)
      
      codes<-list()
      wb<-createWorkbook()
      
      #Loop through variables to get the list of codes per year
      var_list=c("NACE_2digits", "NACE_4digits", "prodfra_plus")
      for(var in var_list){
        for(yr in interval){
          product_data_temp<-product_data %>% filter(year==yr)
          unique_codes<-unique(product_data_temp[[var]]) 
          codes[[paste0("codes_", yr)]] <-unique_codes
          assign(paste0("codes_", var), codes)
        }
        
        #Create the dataframe with the panel of codes
        code_list<-paste0("codes_", var)
        all_codes<-sort(unique(unlist(get(code_list))))
        df<-data.frame(matrix(NA, nrow = length(all_codes), ncol=length(codes)))
        colnames(df)<-names(codes)
        for(i in 1:length(codes)){
          df[, i]<-ifelse(all_codes %in% codes[[i]], all_codes, NA)
        }
        assign(paste0("df_", var), df)
        
        #Create xlsx sheets
        addWorksheet(wb, sheetName = if(var=="prodfra_plus") pc else var)
        writeDataTable(wb, x=get(paste0("df_", var)), sheet= if(var=="prodfra_plus") pc else var)
      }
      
      #Save tables
      saveWorkbook(wb, file=paste0(output_dir, "NACE_codes_time_", filter, ".xlsx"), overwrite=T)
      description(paste0("NACE_codes_time_", filter, ".xlsx"), 
                  paste0("Table with codes present in the EAP panel (with ", description_PF_PC, "), at the NACE 2 digit and 4 digit level, Prodfra at the ", description_digits, " level."))
  }
}



