
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
type<-"all"
output_dir<-"C:/Users/NEWPROD_J_DIAZ-AC/Documents/Reallocation/3 Output/graphs/product breakdown/05.04.24/"

# Set parameters for prodfra-pcc8 and excluded industries
prodfra_or_pcc8<-"prodfra"
only_prodfra_in_prodcom<-FALSE
exclude_industries<-TRUE
filter<-paste0(if(prodfra_or_pcc8=="prodfra") "10_digit" else if(prodfra_or_pcc8=="pcc8") "8_digit" else prodfra_or_pcc8, 
               "_", 
               if(only_prodfra_in_prodcom) "only_prodfra_in_prodcom" else "all_prodfra",
               "_",
               if(exclude_industries) "exclude_industries" else "not_exclude_industries")

#Bring tools
tools_dir <- 'C:/Users/NEWPROD_J_DIAZ-AC/Documents/Reallocation/1 Code/Product decomposition/Tools/'
source(paste0(tools_dir, "description.R"))
source(paste0(tools_dir, "deflate.R"))
source(paste0(tools_dir, "summary stats helper.R"))


# Set descriptions for graphs
description_digits<- if(prodfra_or_pcc8=="pcc8") "8 digit codes" else "10 digit codes"
description_PF_PC <- if(only_prodfra_in_prodcom) "only prodfra codes in prodcom" else "all prodfra codes" 
description_exclude_industries<-if(exclude_industries) "excluding utilities and wholesale trade" else "including utilities and wholesale trade"

#Bring in product firm data
product_firm_data_path<-paste0("product_firm_data.RDS")
product_firm_data<-readRDS(product_firm_data_path)


#2) Firm-product match quality data construction ------------------------------------


regression_data<- product_firm_data %>% select(firmid, year, prodfra_plus, product_tenure, age_firm, empl, capital, rev, active, NACE_BR)
regression_data<-regression_data %>% group_by(firmid, year) %>% mutate(tot_val_shipments=sum(rev, na.rm=T))
regression_data<-regression_data %>% mutate(log_rev=log(rev),
                                            log_emp=log(empl),
                                            log_age=log(age_firm),
                                            log_tot_val_shipments=log(tot_val_shipments),
                                            log_cap_inten=log(capital/empl),
                                            product_tenure=log(product_tenure)
)
regression_data<- regression_data %>% select(firmid, year, prodfra_plus, log_rev, log_emp, log_age, log_tot_val_shipments, log_cap_inten, product_tenure, NACE_BR)

# Deal with NAs: remove log_rev=NA
regression_data[]<-lapply(regression_data, function(col){
  replace(col, is.nan(col) |is.infinite(col), NA)
})
regression_data<-regression_data %>% filter (!(is.na(log_rev)))

# Create fixed effects variables
regression_data$firm_prodfra_index<-as.factor(paste0(regression_data$firmid, "_", regression_data$prodfra_plus))
regression_data$year_prodfra_index<-as.factor(paste0(regression_data$prodfra_plus, "_", regression_data$year))
regression_data$year_industry_index<-as.factor(paste0(regression_data$NACE_BR, "_", regression_data$year))

#Run the FE estimation
fixed_effects_model<-feols(log_rev ~ product_tenure + log_emp + log_age + log_tot_val_shipments + log_cap_inten | 
                             firm_prodfra_index + year_prodfra_index + year_industry_index, regression_data)

#Extract match quality
p_i_fixed_effects<-fixef(fixed_effects_model)
data<-as.data.frame(p_i_fixed_effects$firm_prodfra_index)
data<-add_rownames(data)
colnames(data) = c('firm_prodfra_index', 'firm_product_quality_index')
data<-separate(data, col=firm_prodfra_index, into=c("firmid", "prodfra_plus"), sep="_")

#Normalize estimates
min_fpqi<-min(data$firm_product_quality_index, na.rm = T)
max_fpqi<-max(data$firm_product_quality_index, na.rm = T)
data <- data %>% mutate(firm_product_quality_index=((firm_product_quality_index-min_fpqi)/(max_fpqi-min_fpqi)))
saveRDS(product_firm_data, paste0("fpqi.RDS"))


product_firm_data<-merge(product_firm_data, data, by=c("firmid", "prodfra_plus"), all.x = T)


rm(list=setdiff(ls(), c("fixed_effects_model", "product_firm_data", "graph_dir", "raw_dir", "filter")))
gc()

saveRDS(product_firm_data, paste0("product_firm_data_fpqi.RDS"))

