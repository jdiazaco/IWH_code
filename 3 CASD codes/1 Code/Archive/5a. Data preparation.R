"
This file is mostly based on '2 reallocation dataset construction.R', created by Alex Magnuson

It takes information from BS, BR and PRODCOM information to create a dataset with the variables 
needed for our product reallocation analysis.

Most meaningful changes with respect to the original file include:
- Deflate and description functions.
- Import BS data to get capital, intermediate input costs and labor cost measures (necessary for productivity estimation)
- Adjust firmid in all datasets to be 9 digits long (fill left side with 0s when number of characters is lower than 9)
- Create firm_birth_year, a variable with the year the firm was created according to BR
- Add turnover, raw_materials, labor_cost, firm_birth_year in the biennial version of sbs/br
- Allow the option of constructing the prodcom dataset with (i) 8-digit prodcom codes or 10-digit prodfra codes, (ii) excluding or not certain industries and (iii) prodfra codes based on their relationship with prodcom. Save files with according names
- Add the PC8 entry year based on Eurostat concordance tables
- Replace NAs with 0s in the prodcom creation routine
- Deflate prodcom revenue data using industry deflators
- Create prodcom coverage gap variable to identify the year of entry and exit for discontinuities in prodcom coverage. This variable takes the value of 0 (indicating no gap) if, for a given product within a firm, the previous observation occurred in the previous calendar year and the subsequent observation occurs in the following calendar year. Otherwise, it takes a value of 1.
- For years flagged with gap (exit and reentry of the product), change rev_growth, rev_reallocation and rev_bar to 0.
- Redefine rev_growth as (rev - rev_l)/rev_bar and define rev_reallocation as |rev-rev_l|/rev_bar
- Divide the creation and the cleaning of prodcom data into two different sections (3 and 4)
- Distinguish (creating two different datasets for sections 5 and 6) between product reallocation and growth at the intensive margin
- Create enterprise level combined dataset. As of 22/06/2024 this is not finished yet.

Author: Alex Magnuson
Edits: Julián Díaz-Acosta
Last update: 22/06/2024
"

# setup -------------------------------------------------------------------
# libraries
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
setwd('C:/Users/NEWPROD_J_DIAZ-AC/Documents/Reallocation/2 Data/reallocation_construction_output/Product breakdown/')
raw_dir = "C:/Users/NEWPROD_J_DIAZ-AC/Documents/Raw_data/Data/"
raw_dir_public = "C:/Users/Public/1. Microprod/0. Raw data processing/Data/"
output_dir<-"C:/Users/NEWPROD_J_DIAZ-AC/Documents/Reallocation/3 Output/graphs/product breakdown/19.04.24/"

#Bring tools
tools_dir <- 'C:/Users/NEWPROD_J_DIAZ-AC/Documents/Reallocation/1 Code/Product decomposition/Tools/'
source(paste0(tools_dir, "description.R"))
source(paste0(tools_dir, "deflate.R"))
source(paste0(tools_dir, "summary stats helper.R"))

start<-2009
end<-2021



#0) Import Business Registry / SBS data -------------------------------------------------------------------------
# import both datasets then filter BR data by whether it is in SBS,
# which underwent additional cleaning to ensure panel without breaks 

start = 1994
end = 2021

## import BR data
br_data = rbindlist(lapply(c(start:end),function(yr){
  print(yr)
  ## import BR data
  br_path = paste0(raw_dir,"br/br",yr,".csv" )
  br_data_temp = fread(br_path, select = c('ENT_ID','year','empl', 'NACE_M', "Start_Ent"))
  br_data_temp[,NACE_BR := NACE_M]
  br_data_temp[, `:=`(firmid = as.character(ENT_ID))]
  br_data_temp = br_data_temp %>% select(firmid, year,empl, NACE_BR, Start_Ent)
}), fill = T)


## import SBS data 
interest_vars = c('ENT_ID','year',"SBS_12110")
sbs_data = rbindlist(lapply(c(start:end),function(yr){
  print(yr)
  ## import SBS 
  path = paste0(raw_dir,"sbs/sbs",yr,".csv" )
  sbs_data_temp <- fread(path, nrows = 1)
  available_vars = intersect(interest_vars, colnames(sbs_data_temp))
  sbs_data_temp <- fread(path, select = available_vars)
  sbs_data_temp[, ENT_ID := as.character(ENT_ID)]
  sbs_data_temp = sbs_data_temp %>% select(intersect(interest_vars, colnames(sbs_data_temp)))
}), use.names = T, fill = T)
sbs_data = sbs_data %>% select(interest_vars)
colnames(sbs_data) = c('firmid', 'year','nq')

## Julián: import BS data to get capital measures
interest_vars = c('ENT_ID', 'year',"capital", "turnover", "raw_materials", "labor_cost")
bs_data = rbindlist(lapply(c(start:end),function(yr){
  print(yr)
  ## import BS 
  path = paste0(raw_dir,"bs/bs",yr,".csv" )
  bs_data_temp <- fread(path, nrows = 1)
  available_vars = intersect(interest_vars, colnames(bs_data_temp))
  bs_data_temp <- fread(path, select = available_vars)
  bs_data_temp[, ENT_ID := as.character(ENT_ID)]
  bs_data_temp = bs_data_temp %>% select(intersect(interest_vars, colnames(bs_data_temp)))
}), use.names = T, fill = T)
bs_data = bs_data %>% select(interest_vars)
colnames(bs_data) = c('firmid', 'year','capital', "turnover", "raw_materials", 'labor_cost')

#There are some firmid codes with less than 9-digits that are 9-digit codes in the other database
#This happens because they start with 0 in the other database
#Here I add an initial 0 to all firmids that have less than 9 digits

br_data$firmid<-str_pad(br_data$firmid, width = 9, side="left", pad="0")
bs_data$firmid<-str_pad(bs_data$firmid, width = 9, side="left", pad="0")
sbs_data$firmid<-str_pad(sbs_data$firmid, width = 9, side="left", pad="0")

## merge data and remove those not in sbs / without labor
sbs_br_combined = merge(sbs_data, br_data, all.x = T)
sbs_br_combined = merge(sbs_br_combined, bs_data, all.x = T)

## define active firms as those with post employees; 
## define firm birth/death as first/last time 
## with positive employees in the data 
sbs_br_combined = sbs_br_combined[empl >0]
sbs_br_combined[, `:=`(birth_year = min(year),
                       death_year = max(year)), by = firmid]
sbs_br_combined[birth_year == start, birth_year:= NA]
sbs_br_combined[death_year == end, death_year:= NA]

#Julián: Create variable with the year the firm was created according to br
sbs_br_combined$date_length<-nchar(sbs_br_combined$Start_Ent)
sbs_br_combined$firm_birth_year <- substr(sbs_br_combined$Start_Ent, nchar(sbs_br_combined$Start_Ent)-3, nchar(sbs_br_combined$Start_Ent))
sbs_br_combined$firm_birth_year <- as.integer(sbs_br_combined$firm_birth_year)
sbs_br_combined <- sbs_br_combined %>% mutate(firm_birth_year=ifelse(firm_birth_year <1900 | firm_birth_year>2020, NA, firm_birth_year))

sbs_br_combined<-deflate(sbs_br_combined, "NACE_BR", c("nq", "capital", "turnover", "raw_materials"), 2009)
# make_summary_stats(sbs_br_combined, 
#                    c("nq", "empl", "capital", "turnover", "raw_materials", "birth_year", "death_year", "firm_birth_year"), 
#                    "year", 
#                    "sbs_br_bs_year.xlsx")
# description("sbs_br_bs_year.xlsx", "This table presents descriptive statistics of revenue, 
#             employment, capital, input costs and firm age on the panel of FARE/FICUS for each year between 1994 and 2021.\n")

## export data for later use 
saveRDS(sbs_br_combined, 'sbs_br_combined.RDS')

#1) use the BR data to retrieve firms' dominant industry by year ----------------------------------------------
## import necessary data 
division_levels = as.data.table(read_excel("C:/Users/Public/1. Microprod/Reallocation_work/2 Data/product_harmonization_output/division_to_tech_level.xlsx", 
                                           sheet = "division_to_tech_level"))
NACE_data = readRDS('sbs_br_combined.rds') %>% select(c('firmid', 'year', 'NACE_BR'))


## the NACE data reported in the BR prior to 2008 uses NAF revision 1 (see ficus documentation) which is equivalent to NACE
## through the first three digits. Since those are the only ones we need to match we're golden 
NACE_data[, NACE_version := ifelse(year<2008, 1.1, 2)]

## add in the the sector (manufac/ services) and tech_level
NACE_data[, industry := as.numeric(str_sub(NACE_BR,1,2))]
#Julián: add the by argument, otherwise there is error
NACE_data = merge(NACE_data, division_levels, all.x = T, by=c("industry", "NACE_version"))

#fix the random 3 digit cases from NACE 1.1
NACE_data[ as.numeric(str_sub(NACE_BR,1,3)) == 244 & NACE_version == 1.1, tech_level := 1]
NACE_data[ as.numeric(str_sub(NACE_BR,1,3)) == 353 & NACE_version == 1.1, tech_level := 1]
NACE_data[ as.numeric(str_sub(NACE_BR,1,3)) == 351 & NACE_version == 1.1, tech_level := 3]

# categorize high tech-low tech 
NACE_data[,high_tech := tech_level <3]
NACE_data[,low_tech := !high_tech]
## export
saveRDS(NACE_data,'firm_NACE_BR.rds')

#2) make biennial version of sbs/br for analysis ----------
##import data 
start = 1994
end = 2021
sbs_data = readRDS('sbs_br_combined.rds') 
setorder(sbs_data, firmid, year)

## generate lagged variables Julián: add capital  
normal_cols = c('nq','empl', 'capital', "turnover", "raw_materials", "labor_cost", "firm_birth_year")
lag_cols = paste0(normal_cols,'_l')

sbs_data_l = sbs_data[year<end] %>% mutate(year = year + 1) %>% select(-NACE_BR)
colnames(sbs_data_l)[names(sbs_data_l) %in% normal_cols] = lag_cols
sbs_data = merge(sbs_data, sbs_data_l,by=c('firmid','year', 'birth_year', 'death_year'), all = T) 

## Define birth and death, make corrections to first and last year
sbs_data[, `:=`(born = !is.na(birth_year) & birth_year == year,
                died = !is.na(death_year) & death_year < year)]

sbs_data[, status:= ifelse(born, 'born', ifelse(died, 'died', 'survived'))]

## correct first / last year 
for(i in seq_along(normal_cols)){
  sbs_data[born == T, lag_cols[i]:= 0]
  sbs_data[died == T, normal_cols[i]:= 0] 
}

## Define 2-year averages,quantiles, growth_rates, shares
bar_cols = paste0(normal_cols, '_bar')
growth_cols = paste0(normal_cols, '_growth')
reallocation_cols = paste0(normal_cols, '_reallocation')
growth_weighted_cols = paste0(growth_cols, '_weighted')
reallocation_weighted_cols = paste0(reallocation_cols, '_weighted')

share_cols = paste0(normal_cols, '_share')



## 2-year averages, share, growth, weighted growth 
for (i in seq_along(normal_cols)){
  col = normal_cols[i]; lag = lag_cols[i]
  
  sbs_data[, bar_cols[i] := .5*(get(col)+get(lag))]
  sbs_data[year == start,bar_cols[i] := NA]
  
  if(normal_cols[i]!='nq'){
    sbs_data[, share_cols[i] := get(bar_cols[i])/ sum(get(bar_cols[i]), na.rm = T), by = year] 
    sbs_data[, reallocation_cols[i] :=  abs(ifelse(get(bar_cols[i]) != 0,
                                                   (get(col)-get(lag))/get(bar_cols[i]), 0))]
    sbs_data[, growth_cols[i] :=  ifelse(get(bar_cols[i]) != 0,
                                         (get(col)-get(lag))/get(bar_cols[i]), 0)]
    sbs_data[, growth_weighted_cols[i] := get(share_cols[i])* get(growth_cols[i])]
    sbs_data[, reallocation_weighted_cols[i] := get(share_cols[i])* get(reallocation_cols[i])]
    
  }
}

## merge in the NACE data 
sbs_data = merge(sbs_data, readRDS('firm_NACE_BR.rds'), by = c('firmid', 'year','NACE_BR'), all.x =T)

## add variables for later analysis 
sbs_data = sbs_data[, superstar := ifelse(is.na(NACE_BR), NA, nq /sum(nq, na.rm =T)> .01), by =.(year,NACE_BR)]
sbs_data[, `:=`(age = year - birth_year,
                young = ifelse(is.na(birth_year),ifelse(year - start > 4,F,NA),
                               ifelse(year-birth_year < 5,T,F)))]

## generate employment buckets (I use the divisions present in the ICT data)
breaks = c(-Inf, 10,50,250, Inf)
categories = c('<10', '10-50', '50-250', '250+')
sbs_data[, labor_bucket:= cut(empl_bar, breaks=breaks, labels = categories, right = F)]

# generate dummies for each sector 
sbs_data[, `:=`(manufacturing = !is.na(sector) & sector == 1,
                services = !is.na(sector) & sector == 2,
                other_sector = is.na(sector) & !is.na(NACE_BR))]

## combine employment bucket + sector 
sbs_data[,sector_labor_bucket := ifelse(is.na(empl_bar),NA,
                                        ifelse(manufacturing,paste0('manufacturing:', as.character(labor_bucket)),
                                               ifelse(services, paste0('services:', labor_bucket),
                                                      ifelse(other_sector,paste0('other sector:', labor_bucket),NA ))))]


## keep only necessary variables 
sbs_data = sbs_data %>% select(c("firmid", "NACE_BR" ,'year', "birth_year" , "death_year" ,'age','young',"born", "died",
                                 "status",'empl','nq',"empl_share" , "empl_growth" ,"empl_growth_weighted" ,"industry",
                                 "empl_reallocation", "empl_reallocation_weighted", "turnover", "raw_materials", "firm_birth_year.x",
                                 "sector" ,'sector_labor_bucket','manufacturing','services','other_sector', "labor_cost", "firm_birth_year",
                                 "high_tech" , "low_tech" ,"superstar" ,"labor_bucket", "capital" ))   

##define component datasets for later use & export results 
birth_death = unique(sbs_data %>% select(firmid, birth_year, death_year))
active_firm_list = sbs_data[year <= death_year | is.na(death_year)] %>% select(firmid, year)


saveRDS(sbs_data, 'sbs_br_combined_cleaned.rds')
saveRDS(birth_death, 'firm_birth_death.rds')
saveRDS(active_firm_list, 'active_firm_list.rds')

#3) extract product level data from Prodcom  ----------------------------------------------

# We can:
# 1. Use 8-digit (prodcom) or 10-digit (prodfra) product codes. Based on this, we bring in 8 or 10 digit product code harmonization to create the Prodcom database
# 2. EAP has three different categorizations for prodfra codes: prodfra nested in prodcon, prodfra outside of prodcom or prodfra not nested, but with Prodcom information.
  # We can decide to use only prodfra codes nested in prodcom or use all prodfra codes
# 3. Exclude certain industries (e.g. public utilities industries)
# We do the above by setting parameters below. The name of the files we will save changes depending on those parameters

# Set parameters for prodfra-pcc8 and excluded industries
prodfra_or_pcc8<-"prodfra"
only_prodfra_in_prodcom<-FALSE
# exclude_industries<-FALSE
filter<-paste0(if(prodfra_or_pcc8=="prodfra") "10_digit" else if(prodfra_or_pcc8=="pcc8") "8_digit" else prodfra_or_pcc8, 
               "_",
               if(only_prodfra_in_prodcom) "only_prodfra_in_prodcom" else "all_prodfra")
               # "_",
               # if(exclude_industries) "exclude_industries" else "not_exclude_industries")
raw_dir_prodcom<-paste0(raw_dir,'prodcom/new/', if(only_prodfra_in_prodcom) "only_prodfra_in_prodcom/" else "all_prodfra/")

##import supplementary data
harmonized_prodfra = fread(paste0('C:/Users/NEWPROD_J_DIAZ-AC/Documents/Reallocation/2 Data/product_harmonization_output/harmonized codes/prodfra_harmonized_2009to2022_', prodfra_or_pcc8, '.csv'))
active_firm_list = readRDS('active_firm_list.rds')
birth_death = readRDS('firm_birth_death.rds') 
#Julián: Pc8_entry_year
PC8_entry_year <- fread('PC8_prodfra_years_entry.csv', select=c("codes", "code_entry_year"))

## import the prodcom data + harmonize product codes 
start = 2009
end = 2021

product_data = rbindlist(lapply(c(start:end),function(yr){
  # yr<-2022
  print(yr)
  # import product data 
  # filepath = paste0(raw_dir,'prodcom/prodcom',yr,'.csv')
  filepath = paste0(raw_dir_prodcom,'prodcom',yr, '.csv')
  dta_temp = fread(filepath)
  dta_temp[, firmid := as.character(firmid)]
  dta_temp[, pcc8 := as.character(pcc8)]
  dta_temp$firmid<-str_pad(dta_temp$firmid, width = 9, side="left", pad="0")
  
  #Julián: Replace NAs with 0s
  vars<-c("rev", "prod_q", "sold_q")
  for(var in vars){
    print(paste0("Number of NAs in ", var, " in year ", yr, ": ",  sum(is.na(dta_temp[[var]])), ". Total obs: ", length(dta_temp$firmid)))
    dta_temp[[var]][is.na(dta_temp[[var]])]<-0
    print(paste0("Number of NAs in ", var, " in year ", yr, ": ",  sum(is.na(dta_temp[[var]])), ". Total obs: ", length(dta_temp$firmid)))
  }
  
  #keep product values that aren't dropped in harmonization
  prodfra_var = paste0('prodfra_',yr)
  prodfra_codes =  unique(harmonized_prodfra %>% select(prodfra_var,prodfra_plus) %>% filter(!is.na(prodfra_var)))
  dta_temp = merge(dta_temp, prodfra_codes,by.x = prodfra_or_pcc8, by.y = prodfra_var)

  #keep firms that employ labor
  dta_temp = merge(dta_temp, active_firm_list, by = c('year','firmid'))

  # define active as positive rev or quantities, keep only active products
  dta_temp = dta_temp[(rev + sold_q + prod_q)>0]

  #Julián: Merge information about the year of codes' first appearance in harmonized PC8 tables
  dta_temp = merge(dta_temp, PC8_entry_year, by.x = 'pcc8', by.y = "codes", all.x = T)

  # collapse data to firm-prodfra_plus level
  dta_temp = dta_temp[, .(rev = sum(rev, na.rm = T), code_entry_year=min(code_entry_year)),
                      by= .(firmid, prodfra_plus, year)]#, code_entry_year)]
  table(dta_temp$code_entry_year)

  #generate product-HHI variable
  dta_temp[, total_rev := sum(rev, na.rm = T), by = firmid]
  dta_temp[,HHI := ifelse(total_rev > 0, sum((rev/total_rev)^2),NA), by = firmid]
  dta_temp[, total_rev:=NULL]

  ## generate active status marker
  dta_temp[,active := 1]
}))

saveRDS(product_data, paste0("product_data_", filter,  "_.RDS"))


#4) clean product level data from Prodcom  ----------------------------------------------

# Set parameters for prodfra-pcc8 and excluded industries
prodfra_or_pcc8<-"prodfra"
only_prodfra_in_prodcom<-FALSE
filter<-paste0(if(prodfra_or_pcc8=="prodfra") "10_digit" else if(prodfra_or_pcc8=="pcc8") "8_digit" else prodfra_or_pcc8, 
               "_",
               if(only_prodfra_in_prodcom) "only_prodfra_in_prodcom" else "all_prodfra")

product_data<-readRDS(paste0("product_data_", filter,  "_.RDS"))

exclude_industries<-TRUE
filter<-paste0(filter, if(exclude_industries) "_exclude_industries" else "_not_exclude_industries")

# Set start and end years
start = 2009
end = 2021

# Bring in BR NACE information
active_firm_list = readRDS('active_firm_list.rds')
birth_death = readRDS('firm_birth_death.rds') 

# Create alternative NACE information: Take the first 4 digits of the prodfra_plus code
product_data<-product_data %>% mutate(NACE=substr(prodfra_plus, 1, 4))

# Exclude industries if the parameter is set above
# These are industries that are not continuously covered by prodcom (specifically, they were not covered in 2009)
if(exclude_industries){
  ind_to_exclude <- c(35,36,37,38,39,46) 
  product_data<-product_data %>% filter(!(substr(NACE, 1, 2) %in% ind_to_exclude))
}

# Deflate prodcom revenue data using industry deflators
product_data<-deflate(product_data, "NACE", "rev", start)
# make_summary_stats(product_data, c("rev", "HHI"), "full_sample", "product_data.xlsx")
# description("product_data.xlsx", "Summary statistics on revenue and within-firm concentration using PRODCOM data.\n")
# make_summary_stats(product_data, c("rev", "HHI"), "DEFind", "product_data_industry.xlsx")
# description("product_data_industry.xlsx", "Summary statistics on revenue and within-firm concentration using PRODCOM data, per NACE industry.\n")

## generate lags 
normal_cols = c('active', 'rev')
lag_cols = paste0(normal_cols, '_l')
product_data_l = product_data[year<end, ] %>% mutate(year = year + 1) %>%
  select(firmid,year, prodfra_plus, normal_cols)
colnames(product_data_l)[names(product_data_l) %in% normal_cols] = lag_cols

product_data = merge(product_data, product_data_l, by=c("firmid", "year", "prodfra_plus"), all = T)

# Fix NAs in NACE 
product_data<-product_data %>% mutate(NACE=substr(prodfra_plus, 1, 4))

#Julián: add code_entry_year
columns = c('firmid', 'prodfra_plus', 'year', "code_entry_year")
for (i in seq_along(normal_cols)){
  product_data[is.na(get(normal_cols[i])), normal_cols[i]:=0]
  product_data[is.na(get(lag_cols[i])), lag_cols[i]:=0]
  columns = c(columns, lag_cols[i], normal_cols[i])
}
product_data[,`:=`(active = active==1, active_l = active_l==1)]

## add in birth/death status,
product_data = merge(product_data,birth_death, by=c('firmid'))
product_data[, `:=`(born = !is.na(birth_year) & birth_year == year,
                    died = !is.na(death_year) & death_year < year)]
product_data[, status:= ifelse(born, 'born', ifelse(died, 'died', 'survived'))]

## fix first / last year of data 
product_data[born == T, lag_cols:= 0]
product_data[died == T, normal_cols := 0] 

setorder(product_data, firmid, prodfra_plus, year)
product_data<-product_data %>% group_by(firmid) %>% mutate( first_year=min(year), last_year=max(year) )
product_data <- product_data %>% group_by(firmid, prodfra_plus) %>% mutate(forward_year=dplyr::lead(year), lag_year=dplyr::lag(year))
#Julián: Create prodcom coverage gap variables
product_data <- product_data %>% group_by(firmid, prodfra_plus) %>% 
  mutate(gap=ifelse(forward_year==(year+1) & lag_year==(year-1), 0, ifelse(forward_year==year | lag_year==year, "same year", 1)))
product_data <- product_data %>% mutate(gap=ifelse(is.na(gap),0,gap))

product_data <- product_data %>% select(firmid, prodfra_plus, year, first_year, last_year, gap, forward_year, lag_year, everything())
product_data<-as.data.table(product_data)
product_data[, rev_bar := .5*(rev + rev_l)]
#Julián: delete absolute values to have actual revenue growth (not reallocation)
product_data[, rev_growth := ifelse(rev_bar != 0, (rev - rev_l)/rev_bar, 0)]
product_data[, rev_reallocation := abs(ifelse(rev_bar != 0, abs(rev - rev_l)/rev_bar, 0))]
#Julián: Change gap years rev_growth=0, rev_reallocation=0 and rev_bar=0
product_data[, rev_growth := ifelse(gap==1,0, rev_growth)]
product_data[, rev_reallocation := ifelse(gap==1,0, rev_reallocation)]
product_data[, rev_bar := ifelse(gap==1,0, rev_bar)]
product_data[, within_firm_rev_share :=  rev_bar/ sum(rev_bar, na.rm = T),
             by = .(firmid,year)]
product_data[is.nan(within_firm_rev_share), within_firm_rev_share := 0]
product_data[, within_economy_rev_share :=  rev_bar/ sum(rev_bar, na.rm = T),
             by = .(year)]

## generate product status variables 
product_data[, active_year := ifelse(active, year, NA)]
product_data[, first_introduction := year == min(active_year, na.rm = T), by= .(firmid, prodfra_plus)]
product_data[, `:=`(reintroduced = !first_introduction & active & !active_l,
                    discontinued = active_l & !active,
                    incumbent = active_l & active)]

## export the data 
product_data = product_data %>% arrange(firmid, prodfra_plus, year) %>% select(columns, everything())

# make_summary_stats(product_data, c("rev", "rev_l", "gap", "HHI", "rev_growth"), "year", "product_data2_year")
# description("product_data2_DEFind.xlsx", 
#             "Summary statistics on revenue and within-firm concentration using PRODCOM data excluding utilities, per NACE 2 digit codes. \n")

saveRDS(product_data, paste0("product_level_growth_", filter,  "_.RDS"))



#5) prepare firm level decompositions of product data ------------------------------------


# Set parameters for prodfra-pcc8 and excluded industries
prodfra_or_pcc8<-"prodfra"
only_prodfra_in_prodcom<-FALSE
filter<-paste0(if(prodfra_or_pcc8=="prodfra") "10_digit" else if(prodfra_or_pcc8=="pcc8") "8_digit" else prodfra_or_pcc8, 
               "_",
               if(only_prodfra_in_prodcom) "only_prodfra_in_prodcom" else "all_prodfra")
exclude_industries<-TRUE
filter<-paste0(filter, if(exclude_industries) "_exclude_industries" else "_not_exclude_industries")


## import data and generate potential firm-year level observations
product_data<-readRDS(paste0("product_level_growth_", filter,  "_.RDS"))
# product_data = readRDS('product_level_reallocation.rds')
# birth_death = readRDS('firm_birth_death.rds') 
#product_data = fread('../fake_product_data.csv')  ## used for easy testing 

##5.1) calculate extensive margin product growth/reallocation rate -------------------
firm_data_ex = product_data[, .(products_l = sum(active_l),
                                products = sum(active),
                                prod_added = sum(active & !active_l),
                                prod_removed = sum(!active & active_l)),
                            by = .(firmid, year, birth_year, death_year, status)]
firm_data_ex[, `:=`(entrance_growth = prod_added / products,
                    exit_growth = prod_removed / products_l,
                    entrance_share = products/sum(products),
                    exit_share = products_l / sum(products_l)),
             by =year]

firm_data_ex[status == 'born', exit_growth:=0]
firm_data_ex[status == 'died', entrance_growth:=0]
firm_data_ex[,status:= NULL]
firm_data_ex[, `:=`(entrance_growth_weighted  = entrance_growth * entrance_share,
                    exit_growth_weighted      = exit_growth     * exit_share)] 

vars<-c("entrance", "exit")
for (var in vars){
  firm_data_ex[[paste0(var, "_reallocation")]]<-firm_data_ex[[paste0(var, "_growth")]]
  firm_data_ex[[paste0(var, "_reallocation_weighted")]]<-firm_data_ex[[paste0(var, "_growth_weighted")]]
}

firm_data_ex = firm_data_ex %>% select(-c('products_l', 'products', 'prod_added', 'prod_removed'))
saveRDS(firm_data_ex, 'firm_level_ex_margin.rds')

# make_summary_stats(firm_data_ex, c("entrance_growth", "exit_growth", "entrance_share", "exit_share"), "year", "ex_margin_year.xls")
# description("ex_margin_year.csv", "Summary statistics for average entrance growth, exit growth, entrance share and exit share per year.\n")
# make_summary_stats(firm_data_ex, c("entrance_growth", "exit_growth", "entrance_share", "exit_share"), "full_sample", "ex_margin.xls")
# description("ex_margin.csv", "Summary statistics for average entrance growth, exit growth, entrance share and exit share.\n")

##5.2) calculate intensive margin product reallocation rate -------------------
product_data = product_data %>% select(firmid, prodfra_plus, year, rev_l, rev, rev_bar,
                                       rev_reallocation, within_firm_rev_share, everything())
firm_data_in = product_data[, .(rev=sum(rev),
                                rev_l=sum(rev_l),
                                rev_bar = .5*(sum(rev_bar)),#Why times 0.5?
                                rev_reallocation = sum(rev_reallocation*within_firm_rev_share)),
                            by = .(firmid, year,birth_year, death_year)] #Here aggregating by firm and year, deleting product-lines.
firm_data_in[, rev_share :=  rev_bar /sum(rev_bar), by = year] #Here aggregating by year in a new variable, getting the revenue share for the whole economy, but not collapsing the firm-year information.
firm_data_in[, `:=`(rev_reallocation_weighted = rev_reallocation * rev_share, rev_bar =NULL)]
saveRDS(firm_data_in, 'firm_level_reallocation_in_margin.rds')

# make_summary_stats(firm_data_in, c("rev_reallocation"), "year", "in_margin_reallocation_year.xls")
# description("in_margin_reallocation_year.xls", "Summary statistics for average revenue reallocation rate. Source: Prodcom.\n")

##5.3) calculate intensive margin product growth rate -------------------

product_data = product_data %>% select(firmid, prodfra_plus, year, rev_l, rev, rev_bar,
                                       rev_growth, within_firm_rev_share, everything())
firm_data_in = product_data[, .(rev=sum(rev),
                                rev_l=sum(rev_l),
                                rev_bar = .5*(sum(rev_bar)),#Why times 0.5?
                                rev_growth = sum(rev_growth*within_firm_rev_share)),
                            by = .(firmid, year,birth_year, death_year)] #Here aggregating by firm and year, deleting product-lines.
firm_data_in[, rev_share :=  rev_bar /sum(rev_bar), by = year] #Here aggregating by year in a new variable, getting the revenue share for the whole economy, but not collapsing the firm-year information.
firm_data_in[, `:=`(rev_growth_weighted = rev_growth * rev_share, rev_bar =NULL)]
saveRDS(firm_data_in, 'firm_level_growth_in_margin.rds')

# make_summary_stats(firm_data_in, c("rev_growth"), "year", "in_margin_growth_year.xls")
# description("in_margin_growth_year.xls", "Summary statistics for average groeth reallocation rate, per year. Source: Prodcom.\n")


#6) generate final firm Level dataset - growth and reallocation --------------------------------------------------------
rm(list = ls())
gc()
setwd('C:/Users/NEWPROD_J_DIAZ-AC/Documents/Reallocation/2 Data/reallocation_construction_output/Product breakdown/')

## import data
sbs_data = readRDS('sbs_br_combined_cleaned.rds')
firm_data_ex = readRDS('firm_level_ex_margin.rds')
firm_data_in_reallocation = readRDS('firm_level_reallocation_in_margin.rds')
firm_data_in_growth = readRDS('firm_level_growth_in_margin.rds')


## merge together 
product_data = merge(firm_data_in_reallocation,firm_data_ex, all = T) %>% mutate(in_prodcom = T)
combined_data = merge(sbs_data, product_data, by = c('firmid', 'year', 'birth_year', 'death_year'), all = T)
combined_data[is.na(in_prodcom ), in_prodcom := F]
combined_data[,full_sample:= 1]
saveRDS(combined_data, 'combined_sbs_br_prodcom_data.rds')

## merge together 
product_data = merge(firm_data_in_growth,firm_data_ex, all = T) %>% mutate(in_prodcom = T)
combined_data = merge(sbs_data, product_data, by = c('firmid', 'year', 'birth_year', 'death_year'), all = T)
combined_data[is.na(in_prodcom ), in_prodcom := F]
combined_data[,full_sample:= 1]
saveRDS(combined_data, 'combined_sbs_br_prodcom_data_growth.rds')








#7) generate final enterprise Level dataset -reallocation - NOT FINISHED YET --------------------------------------------------------
rm(list = ls())
gc()

raw_ep_path<-"//casd.fr/casdfs/Projets/NEWPROD/Data/Statistique annuelle d'entreprise_Contours des entreprises profilées_"

start = 2009
end = 2015

## import ep data
ep_data = rbindlist(lapply(c(start:end),function(yr){
  # yr<-2009
  print(yr)
  ## import BR data
  ep_path = (paste0(raw_ep_path, yr, "/contour", substr(yr,3,4),".sas7bdat" ))
  ep_data_temp = data.table(read_sas(ep_path))
  
  if(yr<2016){
    ep_data_temp <- ep_data_temp %>% select(IDENT_EP, RS_EP, IDENT_UL, MILLESIME, APE_EP, APE_UL, TAUX)
    colnames(ep_data_temp) = c('ENT_ID', "ep_name", "firmid", 'year', "ape_ep", 'ape_ul', "rate")
    return(ep_data_temp)
  }
    
    if(yr>=2016){
      ep_data_temp <- ep_data_temp %>% select(sirus_id, siren, effet_daaaa, ape_ep, ape_ul, taux)
      colnames(ep_data_temp) = c('ENT_ID', "firmid", 'year', "ape_ep", 'ape_ul', "rate")
      return(ep_data_temp)
    }
  
}), fill = T)

#Number of enterprises and firms per year
ep_data_count <-ep_data %>% group_by(year) %>% summarise(n_ep=n_distinct(ENT_ID), n_firms=n_distinct(firmid)) %>% mutate(av_ul=n_firms/n_ep)
setorder(ep_data, firmid, ENT_ID, year)

ep_data <- ep_data %>% group_by(firmid) %>% summarise(ENT_ID=unique(ENT_ID), 
                                                      n_ep=n_distinct(ENT_ID), 
                                                      n_years=n(), 
                                                      min_year=min(year), 
                                                      max_year=max(year), 
                                                      ep_name=paste(unique(ep_name), sep=","), 
                                                      rate=mean(rate)) 


ep_data <- ep_data %>% group_by(firmid) %>% summarise(ENT_ID=unique(ENT_ID), n_ep=n()) 




setwd('C:/Users/NEWPROD_J_DIAZ-AC/Documents/Reallocation/2 Data/reallocation_construction_output/Product breakdown/')

## import data
sbs_data = readRDS('sbs_br_combined_cleaned.rds')
firm_data_ex = readRDS('firm_level_ex_margin.rds')
firm_data_in_reallocation = readRDS('firm_level_reallocation_in_margin.rds')
firm_data_in_growth = readRDS('firm_level_growth_in_margin.rds')


## merge together 
product_data = merge(firm_data_in_reallocation,firm_data_ex, all = T) %>% mutate(in_prodcom = T)
combined_data = merge(sbs_data, product_data, by = c('firmid', 'year', 'birth_year', 'death_year'), all = T)
combined_data[is.na(in_prodcom ), in_prodcom := F]
combined_data[,full_sample:= 1]
saveRDS(combined_data, 'combined_sbs_br_prodcom_data.rds')

## merge together 
product_data = merge(firm_data_in_growth,firm_data_ex, all = T) %>% mutate(in_prodcom = T)
combined_data = merge(sbs_data, product_data, by = c('firmid', 'year', 'birth_year', 'death_year'), all = T)
combined_data[is.na(in_prodcom ), in_prodcom := F]
combined_data[,full_sample:= 1]
saveRDS(combined_data, 'combined_sbs_br_prodcom_data_growth.rds')








