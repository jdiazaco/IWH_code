"
This file is mostly based on '2 reallocation dataset construction.R'
Most meaningful changes from '2 reallocation dataset construction.R' can be identified by a comment that begins with 'Julián:'



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
output_dir<-"C:/Users/NEWPROD_J_DIAZ-AC/Documents/Reallocation/3 Output/graphs/product breakdown/26.04.24/"

#Bring tools
tools_dir <- 'C:/Users/NEWPROD_J_DIAZ-AC/Documents/Reallocation/1 Code/Product decomposition/Tools/'
source(paste0(tools_dir, "description.R"))
source(paste0(tools_dir, "deflate.R"))
source(paste0(tools_dir, "summary stats helper.R"))

start<-2009
end<-2021


# 1) Prodcom-firm data --------------------

#Bring in information---------
#Combined data
combined_data = readRDS('combined_sbs_br_prodcom_data.rds')
# make_summary_stats(combined_data, c("empl", "nq", "raw_materials", "capital"), "year", "combined_year") #This takes a long time
combined_data<-combined_data %>% filter(year>=2009)

#Product information
prodfra_or_pcc8<-"prodfra"
only_prodfra_in_prodcom<-FALSE
filter<-paste0(if(prodfra_or_pcc8=="prodfra") "10_digit" else if(prodfra_or_pcc8=="pcc8") "8_digit" else prodfra_or_pcc8, 
               "_",
               if(only_prodfra_in_prodcom) "only_prodfra_in_prodcom" else "all_prodfra")
product_data<-readRDS(paste0("product_data_", filter,  "_.RDS"))
exclude_industries<-TRUE
filter<-paste0(filter, if(exclude_industries) "_exclude_industries" else "_not_exclude_industries")

# Nace information
nace_DEFind <- fread("C:/Users/Public/1. Microprod/1. Microprod-H2020/NEW_INFRASTRUCTURE/Infra/MetaData/nace_DEFind.conc", colClasses = c('character'))


#Clean information---------

# Create in_prodcom variable from EAP df
product_data<-product_data %>% group_by(firmid) %>% summarise(year=unique(year)) %>% mutate(in_prodcom=T)
# Delete in_prodcom from combined_data because it has misleading info (last years in prodcom, which we created manually to be 0, are considered "in_prodcom")
combined_data$in_prodcom<-NULL

# Merge
combined_data<-merge(combined_data, product_data, by=c("firmid", "year"), all.x=T)
combined_data<-combined_data %>% mutate(in_prodcom=ifelse(is.na(in_prodcom), F, in_prodcom))
rm(product_data); gc()

# Define small and large firms
combined_data <- combined_data %>% mutate(size=ifelse(empl>20 | nq>5000, "large", "small"))

# Select relevant variables, clean NACE information and bring in DEFind information
combined_data_empl<-combined_data %>% filter(status!="died") %>% select(firmid, year, empl, in_prodcom, size, NACE_BR, status)
combined_data_empl$NACE_BR<-str_pad(combined_data_empl$NACE_BR, 4, side = "left", pad = "0")
combined_data_empl<-merge(combined_data_empl, nace_DEFind, by.x = "NACE_BR", by.y = "nace", all.x=T)
sum(is.na(combined_data_empl$DEFind))
combined_data_empl$sector<-substr(combined_data_empl$DEFind, 1,1)


#Calculate longitudinal component measures using only prodcom data------
table(combined_data_empl$sector, combined_data_empl$in_prodcom)

combined_data_prodcom <- combined_data_empl %>% filter(in_prodcom)
combined_data_prodcom <- combined_data_prodcom %>% group_by(firmid) %>% summarise(prodcom_gaps=ifelse(n()==(max(year)-min(year)+1), "continuous", "intermittent"),
                                                                                  size_switch=ifelse(n_distinct(size, na.rm=T)==1, FALSE, TRUE),
                                                                                  size=ifelse(size_switch, "switch", unique(na.omit(size))),
                                                                                  n_years=n_distinct(year),
                                                                                  single_year=ifelse(n_years==1, max(year), NA),
                                                                                  multi_year=ifelse(n_years>1, "multiple year", ifelse(year==2021, "2021", "single year")))
table(combined_data_prodcom$prodcom_gaps)
table(combined_data_prodcom$prodcom_gaps, combined_data_prodcom$size)
table(combined_data_prodcom[year<2021,]$prodcom_gaps, combined_data_prodcom[year<2021,]$size, combined_data_prodcom[year<2021,])

table(combined_data_prodcom$prodcom_gaps, is.na(combined_data_prodcom$size))


#Calculate longitudinal component measures using BR and prodcom data--------
#Define prodcom industries
ind_in_prodcom<-c("C", "B", "D", "E")

combined_data_firm <- combined_data_empl 
combined_data_firm <- combined_data_firm %>% group_by(firmid, in_prodcom) %>% 
  summarise(max_year=max(year), min_year=min(year),
            n_years=n_distinct(year),
            gaps=ifelse(n_years==1, "single year",
                        ifelse(n()==(max_year-min_year+1),"continuous", "intermittent")),
            size_switch=ifelse(n_distinct(size, na.rm=T)==1, FALSE, TRUE),
            size=ifelse(size_switch, "switch", unique(na.omit(size))),
            sector=ifelse(n_distinct(sector)==1, unique(sector),
                          ifelse( sum(unique(sector) %in% ind_in_prodcom)>0, "changing industry but in prodcom", "changing industry outside prodcom")))
            # sector=names(sort(table(sector, useNA="ifany"), decreasing=TRUE)[1]))

combined_data_firm<- combined_data_firm %>% mutate(sector=ifelse(is.na(sector), "NA", sector))
combined_data_firm_descriptives <- combined_data_firm %>% group_by(firmid) %>% 
  summarise(gap=ifelse(n_distinct(in_prodcom)==1,
                   ifelse(in_prodcom==T,
                          ifelse(min_year==2021 | n_years==1, 
                                 ifelse(min_year==2021, "2021 and complete in prodcom", "single year and complete in prodcom"),
                                 ifelse(gaps=="continuous", 
                                        "continuous and complete in prodcom", 
                                        "intermittent in prodcom and FARE/FICUS")),
                          ifelse(sector %in% c(ind_in_prodcom, "changing industry but in prodcom"), 
                                 "not in prodcom (PC industries)",
                                 ifelse(sector=="NA", "NA sector", "not in prodcom (sector outside PC)"))),
                   ifelse(gaps[in_prodcom==T]=="continuous",
                          "continuous but incomplete in prodcom",
                          ifelse(gaps[in_prodcom==T]=="single year", 
                                 ifelse(min_year[in_prodcom==T]==2021, "2021 and incomplete in prodcom", "single year and incomplete in prodcom"),
                                 "intermittent and incomplete in prodcom"))), 
            size_switch=ifelse(n_distinct(size, na.rm=T)==1, FALSE, TRUE),
            size=ifelse(size_switch, "switch", unique(na.omit(size))),
            sector=ifelse(n_distinct(sector)==1, unique(sector),
                          ifelse( sum(unique(sector) %in% c(ind_in_prodcom, "changing industry but in prodcom"))>0, "changing industry but in prodcom", "changing industry outside prodcom")))
combined_data_firm_descriptives <- combined_data_firm_descriptives %>% group_by(firmid) %>% mutate(n=n())
  

table(combined_data_firm$gap)
table(combined_data_firm_descriptives$gap, combined_data_firm_descriptives$size)
table(combined_data_firm$gap, is.na(combined_data_firm$size))



# 2) Firm data --------------------
#Bring in information---------
#Firm data
firm_data = readRDS('sbs_br_combined_cleaned.RDS')
firm_data <- firm_data %>% mutate(age=year-firm_birth_year.x)
make_summary_stats(firm_data, c("empl", "nq", "raw_materials", "capital", "labor_cost", "age"), "full_sample", "firm") 
    
# Product data
product_data = readRDS('product_level_growth_10_digit_all_prodfra_exclude_industries_.RDS')
product_data<-merge(product_data, combined_data_empl, by=c("firmid", "year"), all.x = T)

# Create summary statistics---------
make_summary_stats(product_data, c("rev", "HHI", "gap"), "full_sample", "product") 
make_summary_stats(product_data, c("rev", "HHI", "gap"), "year", "product_year") 
make_summary_stats(product_data, c("rev", "HHI", "gap"), "DEFind", "product_DEFind") 

length(unique(product_data$firmid))
length(unique(product_data$prodfra_plus))

#Prodfra codes embedded in Prodcom---------
prodfra<-unique(product_data$prodfra_plus)
codes<-as.data.frame(prodfra) 
codes<-codes %>% mutate(prodcom=substr(prodfra, 1, 8))
codes<-codes %>% group_by(prodcom) %>% summarise(n_prodfra=n_distinct(prodfra))
write.xlsx(codes, paste0(output_dir, "n_prodfra_in_prodcom.xlsx"))
codes<-codes %>% group_by(n_prodfra) %>% summarise(n=n_distinct(prodcom))
write.xlsx(codes, paste0(output_dir, "n_prodfra_in_prodcom_addition.xlsx"))


# 3) Product and firm data ----------------

#Bring in information---------
combined_data = readRDS('combined_sbs_br_prodcom_data.rds')
product_data = readRDS('product_level_growth_10_digit_all_prodfra_exclude_industries_.RDS')
nace_DEFind <- fread("C:/Users/Public/1. Microprod/1. Microprod-H2020/NEW_INFRASTRUCTURE/Infra/MetaData/nace_DEFind.conc", colClasses = c('character'))

# Clean data and define terms---------
combined_data<-combined_data %>% filter(year>=start)
combined_data <- combined_data %>% mutate(size=ifelse(empl>20 | nq>5000000, "large", "small"))
size<-combined_data %>% select(firmid, year, size)
rm(combined_data); gc()

product_data$DEFind<-NULL
product_data<-merge(product_data, nace_DEFind, by.x="NACE", by.y="nace", all.x=T); rm(nace_DEFind); gc()
product_data$NACE_sector<-substr(product_data$DEFind, 1,1)

# Create summary statistics---------
n_size_multiproduct <- product_data %>% group_by(year, firmid) %>% summarise(n_products=n_distinct(prodfra_plus),
                                                                            n_sectors=n_distinct(NACE_sector),
                                                                            n_industries=n_distinct(DEFind),
                                                                            multiproduct=ifelse(n_products>1, T, F),
                                                                            multisector=ifelse(n_sectors>1, T, F),
                                                                            multiindustry=ifelse(n_industries>1, T, F),
                                                                            within_economy_rev_share=sum(within_economy_rev_share)
                                                                            )

n_size_multiproduct<-merge(n_size_multiproduct, size, by=c("firmid", "year"), all.x = T)


n_shares <- n_size_multiproduct %>% group_by(year) %>% summarise(n_firms=n_distinct(firmid),
                                                                 n_products=sum(n_products, na.rm = T),
                                                          share_large=sum(size=="large", na.rm = T)/n_firms,
                                                          share_small=sum(size=="small", na.rm=T)/n_firms,
                                                          share_multiproduct=sum(multiproduct)/n_firms,
                                                          share_multisector=sum(multisector)/n_firms,
                                                          share_multiindustry=sum(multiindustry)/n_firms,
                                                          rev_share_large=sum(within_economy_rev_share*(size=="large"), na.rm = T),
                                                          rev_share_small=sum(within_economy_rev_share*(size=="small"), na.rm = T),
                                                          rev_share_multiproduct=sum(within_economy_rev_share*multiproduct, na.rm = T),
                                                          rev_share_multisector=sum(within_economy_rev_share*multisector, na.rm = T),
                                                          rev_share_multiindustry=sum(within_economy_rev_share*multiindustry, na.rm = T)
                                                          ) 

n_products <- product_data %>% group_by(year) %>% summarise(n_distinct_products=n_distinct(prodfra_plus))

n_shares <- merge(n_shares, n_products, by="year")
n_shares <- n_shares %>% mutate(av_prod_firm=n_products/n_firms)

write.xlsx(n_shares, paste0(output_dir, "prodcom_descriptives.xlsx"))
description("prodcom_descriptives.xlsx", 
            "Table with descriptive statistics (number and share of products, as well as revenue shares) per firm. Source: EAP.\n")


# Graph---------

ggplot(n_firms, aes(x = year)) +
  geom_line(aes(x=year, y=n_firms)) +
  # geom_line(aes(x=year, y=n_products))+
  scale_x_continuous(breaks =  seq(start, end, by = 1)) +  
  labs(title = paste0("Number of distinct products and firms in prodcom panel"),  x = element_blank(),  y = paste0("Number"), fill = "Product status", colour="agg") +
  theme_classic() +theme(legend.position = 'bottom')



