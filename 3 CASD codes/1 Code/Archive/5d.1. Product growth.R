
# setup -------------------------------------------------------------------
# libraries
rm(list = ls())
gc()
packages = c('data.table', 'haven', 'readxl', 'openxlsx','stringr', 'readr', 'dplyr', 'tidyverse', 'zoo', 'reshape2','rstudioapi', "plm", 'foreign', "fixest", 'data.table', 'haven', 'stringr', 'readr', 'dplyr',
             'ggplot2', 'tidyverse', 'rstudioapi', 'zoo', 'reshape2',
             'patchwork', 'latex2exp', "RColorBrewer", "texreg", "stargazer", "modelsummary")
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
output_dir<-"C:/Users/NEWPROD_J_DIAZ-AC/Documents/Reallocation/3 Output/graphs/product breakdown/31.05.24/"

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

start<-2009
end<-2021

#1) Data import and cleaning ------------------------------------

# Bring in firm data
firm_data<-readRDS('sbs_br_combined_cleaned.rds') 

# Create firm age and clean NACE information
firm_data <- firm_data%>% mutate(firm_age=year-firm_birth_year,
                                               NACE_BR=str_pad(NACE_BR, 4, side="left", pad="0"),
                                               NACE_2d=substr(NACE_BR, 1,2))


# Fix missing NACE_BR 
firm_data<-firm_data %>% group_by(firmid) %>%  arrange(firmid, year) %>% 
  mutate(NACE_BR=ifelse(is.na(NACE_BR), lag(NACE_BR), NACE_BR)) %>% fill(NACE_BR)

# Reconstruct empl_l variable
firm_data_lag<- firm_data %>% select(firmid, year, empl, nq)
setDT(firm_data_lag)
firm_data_lag[, `:=`(empl_l=empl,
                     nq_l=nq,
                     year=year+1)]
firm_data_lag$empl<-NULL
firm_data_lag$nq<-NULL

# firm_data<-merge(firm_data, firm_data_lag, by=c("firmid", "year"), all.x = T)
# firm_data[, birth_year := min(year), by = firmid]
# firm_data[!is.na(birth_year) & birth_year == year, empl_l:= 0]
# firm_data_lag<-NULL


# Bring in product data
product_data<-readRDS(paste0("product_level_growth_", filter,  "_.RDS"))

# Bring in Nace industry information
nace_DEFind <- fread("C:/Users/Public/1. Microprod/1. Microprod-H2020/NEW_INFRASTRUCTURE/Infra/MetaData/nace_DEFind.conc", colClasses = c('character'))
nace_ind_sector <- read_excel("NACE_industry_sector.xlsx")



# Clean firm_data to only relevant variables and select firms in FARE/FICUS that have been covered by PRODCOM
prodcom_sectors<-c("B", "C") #Prodcom usually covers sectors B, C, D and E, but we decided to exclude sectors D and E. If this changes, we should change this
unique_firmid<-unique(product_data$firmid)
firm_data_select <- firm_data %>% select(firmid, year, empl, empl_growth, NACE_BR, firm_birth_year, nq, firm_age, NACE_2d) 
setDT(firm_data_select)

#Bring in concordance auxiliary data
source("C:/Users/Public/1. Microprod/1. Microprod-H2020/NEW_INFRASTRUCTURE/Infra/Rtools/nace_conc.R")
nace_concordance<-read_dta("C:/Users/Public/1. Microprod/1. Microprod-H2020/NEW_INFRASTRUCTURE/Infra/AuxData/nace_rev11_rev2.dta")

#Conduct NACE concordance
test<-firm_data_select
test$NACE_BR_og<-test$NACE_BR
test<-conc(DT=test,
     id='firmid',
     t='year',
     ind='NACE_BR',
     foryears = c(1994:2007),
     toyears =c(2008:2021),
     concfile=nace_concordance,
     origin_class='nace_rev11_digit4',
     target_class='nace_rev2_digit4')
firm_data_select<-test

#Bring in lagged variables to firm data and create measures of firm-level revenue growth
firm_data_select<-merge(firm_data_select, firm_data_lag, by=c("firmid", "year"), all.x = T)
firm_data_select[, nq_bar := .5*(nq + nq_l)]
firm_data_select[, nq_growth := ifelse(nq_bar != 0, (nq - nq_l)/nq_bar, 0)]
rm(firm_data_lag); gc()

#Clean firm data with appropriate years and prodcom sectors
firm_data_select<- firm_data_select[year>=2008, ] # If NACE has not been concorded between v1.1 and v2, we remove observations prior to 2008
firm_data_select <- merge(firm_data_select, nace_DEFind, by.x="NACE_BR", by.y = "nace", all.x = T)
firm_data_select[, sector:=substr(DEFind,1,1)]
firm_data_select$DEFind<-NULL
firm_data_select <- firm_data_select[ sector %in% prodcom_sectors]
firm_data_select[, birth_year := min(year), by = firmid]
firm_data_select[!is.na(birth_year) & birth_year == year, empl_l:= 0]
firm_data_lag<-NULL


# Save firm_data with only relevant variables and firms in industries covered by prodcom
saveRDS(firm_data_select, "sbs_br_data_prodcom_firms.RDS")

# Create and save dataframe with firm-level revenue from prodcom and other firm information from FARE/FICUS
product_data_aggr<-product_data %>% group_by(firmid, year) %>% summarise(rev_agg=sum(rev))
firm_empl_rev <- merge(firm_data_select, product_data_aggr, by=c("firmid", "year"), all.x=T) 
firm_empl_rev <- firm_empl_rev %>% mutate(rev_diff=nq-rev_agg)
firm_empl_rev <- firm_empl_rev %>% group_by(firmid) %>% mutate(firm_birth_year=ifelse(is.na(firm_birth_year), min(firm_birth_year, na.rm = T), firm_birth_year))
# saveRDS(firm_empl_rev, "product_data_aggr_firm_empl_rev.RDS")

# Create and save dataframe with product-level revenue and firm level information
product_data <- merge(product_data, firm_empl_rev, by=c("firmid", "year"), all.x=T)

# Establish firm-product lines for which there is gap in coverage
product_data_temp <- product_data[, .(gap_ind=as.integer(any(gap==1))), by=.(firmid, prodfra_plus)] 
table(product_data_temp$gap_ind)
# 8.778 out of 158.098 (5,5%) firm-product lines have gaps 

#Delete firm-product lines for which there is gap in coverage
product_data <- merge(product_data, product_data_temp, by=c("firmid", "prodfra_plus"))
product_data <- product_data[gap_ind!=1]
# deletes 66.654 (6,1%) out of 1.078.159 observations 
product_data$gap_ind<-NULL
rm(product_data_temp); gc()

#Create annual product growth dataset with relevant information
product_growth_annual <- product_data %>% group_by(year, 
                                                   prodfra_plus) %>% summarise(age=mean(firm_age, na.rm = T),
                                                                               age_w=sum((firm_age*rev), na.rm = T)/sum(rev, na.rm = T),
                                                                               empl=mean(empl, na.rm = T),
                                                                               empl_w=sum(empl*rev, na.rm = T)/sum(rev, na.rm = T),
                                                                               # empl_w=weighted.mean(empl, rev, na.rm=T),
                                                                               rev_first=sum(rev*first_introduction, na.rm=T),
                                                                               rev_reintroduced=sum(rev*reintroduced, na.rm=T),
                                                                               rev_discontinued=sum(rev*discontinued, na.rm=T),
                                                                               rev_incumbent=sum(rev*incumbent, na.rm=T),
                                                                               rev_gap=sum(rev*gap, na.rm=T),
                                                                               rev=sum(rev, na.rm = T),
                                                                               rev_l=sum(rev_l, na.rm=T),
                                                                               first_introduction=sum(first_introduction),
                                                                               reintroduced=sum(reintroduced),
                                                                               discontinued=sum(discontinued),
                                                                               incumbent=sum(incumbent),
                                                                               gap=sum(gap),
                                                                               # rev_bar=sum(rev_bar, na.rm=T),
                                                                               n_obs=n())
#Growth and industry variables
product_growth_annual <- product_growth_annual %>% mutate(rev_bar=(rev + rev_l)/2,
                                                          rev_growth=ifelse(rev_bar==0, 0, ((rev - rev_l)/rev_bar)) ,
                                                          NACE=substr(prodfra_plus, 1, 4),
                                                          NACE_2d=substr(prodfra_plus, 1, 2))#%>% select(year, prodfra_plus, rev_growth)

# Age brackets
breaks = c(0, 10, 20, 30, 40, Inf)
categories = c('0-10', '10-20', "20-30", "30-40", "40+")
product_growth_annual<- product_growth_annual %>% mutate(age_bucket= cut(age, breaks=breaks, labels = categories, right = F))
product_growth_annual<- product_growth_annual %>% mutate(age_w_bucket= cut(age_w, breaks=breaks, labels = categories, right = F))
firm_empl_rev<- firm_empl_rev %>% mutate(age_bucket= cut(firm_age, breaks=breaks, labels = categories, right = F))

# Employment brackets
breaks = c(-Inf, 10, 20, 50, 80, 250, 600, 3000, Inf)
categories = c('<10', '10-20', "20-50", "50-80", '80-250', "250-600", "600-3000", '3000+')
product_growth_annual<- product_growth_annual %>% mutate(empl_bucket= cut(empl, breaks=breaks, labels = categories, right = F))
product_growth_annual<- product_growth_annual %>% mutate(emp_w_bucket= cut(empl_w, breaks=breaks, labels = categories, right = F))
firm_empl_rev<- firm_empl_rev %>% mutate(empl_bucket= cut(empl, breaks=breaks, labels = categories, right = F))

# Sector and industry
product_growth_annual<-merge(product_growth_annual, nace_ind_sector, by="NACE_2d", all.x=T)
firm_empl_rev<-merge(firm_empl_rev, nace_ind_sector, by="NACE_2d", all.x = T)

# Growth buckets
breaks = c(-2, -1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2, Inf)
product_growth_annual<- product_growth_annual %>% mutate(growth_bucket= cut(rev_growth, breaks=breaks, right = F))
firm_empl_rev<- firm_empl_rev %>% mutate(growth_bucket= cut(empl_growth, breaks=breaks, right = F))

#Rearrange variables and obs and save
product_growth_annual<- product_growth_annual %>% select(prodfra_plus, rev_growth, year, industry, empl_bucket, age_bucket, sector, everything())
setorder(product_growth_annual, rev_growth, age_bucket, empl_bucket, sector, industry)

saveRDS(product_data, "product_firm_data_regressions.RDS")
saveRDS(product_growth_annual, "product_growth_annual.RDS")
saveRDS(firm_empl_rev, "product_data_aggr_firm_empl_rev.RDS")

#2) Graphs ------------------------------------

product_growth_annual<- readRDS("product_growth_annual.RDS")
firm_empl_rev <-readRDS("product_data_aggr_firm_empl_rev.RDS")


#Define variables and categories for graphs
vars<-c("age_bucket", "empl_bucket", "industry", "sector")
descr<-c("Age", "Workers", "Industry", "Sector")
conditions<-c(unlist(sort(unique(product_growth_annual$growth_bucket))))
conditions<-as.vector(conditions)
conditions<-c(conditions, "all")

#Graphs
for (j in seq_along(vars)) {
  for(i in seq_along(conditions)){

    print(paste0(descr[j], "; ", conditions[i]))
    
    #Define colors
    number_colors<-max(n_distinct(product_growth_annual[[vars[j]]]), n_distinct(firm_empl_rev[[vars[j]]]))
    colors<-rainbow(number_colors)
    
    if(vars[j]=="industry"){
      set.seed(2013)
      colors<-sample(colors)
    }
    
    if(vars[j]=="sector"){
      set.seed(2013)
      colors<-c("Agriculture" ="darkred",
                "Construction" = "darkorange",
                "Manufacturing" = "darkgreen",
                "Mining and quarrying" ="darkcyan",
                "Services"="darkblue",
                "NA"="darkgrey")
    }
    
    #Create conditions for all observations
    if(conditions[i]=="all"){
      c_temp<-c(unlist(sort(unique(product_growth_annual$growth_bucket))))
      c_temp<-as.vector(c_temp)
    } else{
      c_temp<-conditions[i]
    }
    
    #Graph of product level revenue growth
    ggplot(product_growth_annual %>% filter(growth_bucket %in% c_temp), aes(x=rev_growth, fill=factor(get(vars[j]))))+
      geom_histogram(binwidth = 0.05, color="black") + 
      scale_fill_manual(values=colors)+
      labs(title = paste0("Distribution of Annual Product Revenue Growth Rates"), 
           subtitle =paste0("Category: ", descr[j], ". Growth Range: ", conditions[i]),  
           x = "Annual Revenue Growth Rate",  y = paste0("N. Obs."), fill = descr[j]) +
      theme_classic() + if(vars[j]=="industry"){
        theme(legend.position = 'bottom', legend.box = "horizontal", legend.text = element_text(size=5), legend.key.size = unit(0.5, "lines"))} else {
          theme(legend.position = 'bottom', legend.box = "horizontal")
        }
          
    ggsave(paste0(output_dir, "/product_revenue/", vars[j], "/product_rev_growth_", vars[j], "_cond", i, ".png"), width = 7, height = 5)

    #Graph of firm level employment growth
    ggplot(firm_empl_rev %>% filter(growth_bucket %in% c_temp), aes(x=empl_growth, fill=factor(get(vars[j]))))+
      geom_histogram(binwidth = 0.05, color="black") + 
      scale_fill_manual(values=colors)+
      labs(title = paste0("Distribution of Annual Firm Labor Growth Rates"), 
           subtitle =paste0("Category: ", descr[j], ". Growth Range: ", conditions[i]),  
           x = "Annual Revenue Growth Rate",  y = paste0("N. Obs."), fill = descr[i]) +
      theme_classic() + if(vars[j]=="industry"){
        theme(legend.position = 'bottom', legend.box = "horizontal", legend.text = element_text(size=5), legend.key.size = unit(0.5, "lines"))} else {
          theme(legend.position = 'bottom', legend.box = "horizontal")
        }
    
    ggsave(paste0(output_dir, "/firm_labor/", vars[j], "/firm_empl_growth_", vars[j], "_cond", i, ".png"), width = 7, height = 5)
    
  }
  
}

# Measures of product revenue growth and firm employment growth for industry
industry_rev<-product_growth_annual %>% group_by(industry, sector) %>%  summarise(rev_growth=mean(rev_growth, na.rm = T),
                                                                                  w_rev_growth=sum(rev_growth*rev/sum(rev, na.rm = T), na.rm=T))
industry_empl<-firm_empl_rev %>% group_by(industry, sector) %>%  summarise(empl_growth=mean(empl_growth, na.rm = T),
                                                                           w_empl_growth=sum(empl_growth*rev_agg/sum(rev_agg, na.rm = T), na.rm = T))
industry<-merge(industry_rev, industry_empl, by=c("industry", "sector"))
write.xlsx(industry, paste0(output_dir, "industry_growth_rates.xlsx"))


#3) Regressions ------------------------------------

product_growth_annual<- readRDS("product_growth_annual.RDS")
firm_empl_rev <-readRDS("product_data_aggr_firm_empl_rev.RDS")
# product_firm_data<-readRDS("product_firm_data.RDS") 
#Compared to product_firm_data, product_data_regression has fewer firms, the result of the drop of products with gaps in the previous step
#We keep product_firm_data_regressions for consistency in the analysis of this module
product_firm_data_regressions<-readRDS("product_firm_data_regressions.RDS")

# Regression of annual product revenue growth on average age, average employment and sector-----------------------
#Define reference levels
product_growth_annual$ref_age<-relevel(product_growth_annual$age_bucket, ref="10-20")
product_growth_annual$ref_empl<-relevel(product_growth_annual$empl_bucket, ref="20-50")
product_growth_annual$ref_industry<-relevel(factor(product_growth_annual$industry), ref="Paper")
product_growth_annual$ref_sector<-relevel(factor(product_growth_annual$sector), ref="Manufacturing")
product_growth_annual$ref_NACE<-relevel(factor(product_growth_annual$NACE), ref="2790")

firm_empl_rev$ref_NACE<-relevel(factor(firm_empl_rev$NACE_BR), ref="2790")

product_firm_data_regressions[, `:=`(NACE_product=substr(prodfra_plus, 1,4),
                                   NACE_2d_product=substr(prodfra_plus, 1,2),
                                   NACE_2d_BR=substr(NACE_BR, 1,2))]
product_firm_data_regressions$ref_NACE<-relevel(factor(product_firm_data_regressions$NACE_product), ref="2790")



# Regression of annual product revenue growth on average age, average employment and sector-----------------------

#Define filters for the regressions
regression_data<-product_growth_annual %>% filter(year>2009)

# Regression of annual product revenue growth on average age, average employment and sector
product_rev_g_reg<-lm(rev_growth ~ factor(paste(ref_age, year, sep="_")) +  ref_empl + ref_sector, data = regression_data )
summary_reg<-summary(product_rev_g_reg)
reg_table<-as.data.frame(cbind(summary_reg$coefficients[, 1:2], summary_reg$coefficients[, 3:4]))
reg_table$variable<-rownames(reg_table)
reg_table<-reg_table %>% select(variable, everything())
write.xlsx(reg_table, paste0(output_dir, "Regression_age_time_empl_sector.xlsx"))
write(texreg(product_rev_g_reg), file = paste0(output_dir, "Regression_age_time_empl_sector.txt"))


# Regression of annual product revenue growth on average age, average employment and sector-----------------------


#Define filters for the regressions
regression_data_product<-product_growth_annual %>% filter(year>2009, gap==0, rev_growth<2, rev_growth>-2)
regression_data_firm<-firm_empl_rev %>% filter(year>2009, empl_growth<2, empl_growth>-2)
regression_data_firm_product<-product_firm_data_regressions %>% filter(year>2009, rev_growth<2, rev_growth>-2)


# Define vectors with variables, thresholds and descriptions
levels<-c("product", "firm", "firm_product")
growth_vars<-c("rev", "empl", "rev")
unit_vars<-list("prodfra_plus", "firmid", c("firmid", "prodfra_plus"))
nace_vars<-c("NACE", "NACE_BR", "NACE_product")
high_growth_thresholds<-c(0.25, 0.3, 0.25)
descriptions<-c("product revenue growth, entering and exiting products removed",
                "employment growth, entering and exiting firms removed",
                "firm-product revenue growth, entering and exiting products removed")


for (i in 1:length(growth_vars)){
  i<-2
  
  #Define relevant variables
  level<-levels[i]
  var<-growth_vars[i]
  var_l<-paste0(var, "_l")
  var_growth<-paste0(var, "_growth") 
  unit_var<-unit_vars[[i]]
  high_growth_threshold<-high_growth_thresholds[i]
  nace_var<-nace_vars[i]
  
  #Get the data
  data<-get(paste0("regression_data_", level))
  setDT(data)
  
  # Create risk-adjusted version of growth
  data<-data[, `:=`(growth_sd= sd(get(var_growth), na.rm=T),
                    growth_adj=get(var_growth)/sd(get(var_growth), na.rm=T)), by=unit_var]
  
  # Get an analogue for high growth threshold in terms of the risk-adjusted version of growth
  ecdf_growth<-ecdf(data[[var_growth]])
  prob_high_growth<-ecdf_growth(high_growth_threshold)
  growth_adj_thresh<-quantile(data$growth_adj, probs=prob_high_growth, na.rm = T)
  print(paste0("The analogue of ", var, " high growth threshold (", high_growth_threshold, ") is ", round(growth_adj_thresh, 2), " in terms of SDs"))
  #"The analogue of rev high growth threshold (0.25) is 0.91 in terms of SDs"
  #"The analogue of empl high growth threshold (0.3) is 1.45 in terms of SDs"
  #"The analogue of empl high growth threshold (0.3) is 1.37 in terms of SDs"
  #"The analogue of rev high growth threshold (0.25) is 1.15 in terms of SDs"
  
  # Define additional variables for regression
  data<-data[, high_growth:=ifelse(growth_adj>=growth_adj_thresh,1,0)]
  data<-data[, delta_var:=get(var)-get(var_l)]
  
  # Especial regressions
  if(level=="firm"){
    
    product_regression<-lm(high_growth ~ 0+factor(get(nace_var)) , data = data)
    product_regression_unweighted_positive<-lm(high_growth ~ 0+factor(get(nace_var)), data = data[data$delta_var>0, ])
    product_regression_weighted_positive<-lm(high_growth ~ 0+factor(get(nace_var)), weight=delta_var, data = data[data$delta_var>0, ])
    
    
    firm_empl_rev_regression<-lm(empl_growth  ~ nq_growth, data=data)
    firm_empl_rev_regression_fe<-feols(empl_growth  ~ nq_growth |  factor(NACE_BR) + factor(firmid) , data=data)
    firm_empl_rev_regression_fe_t<-feols(empl_growth  ~ nq_growth |  factor(NACE_BR) + factor(firmid) +factor(year) , data=data)
    
    data[, young:=ifelse(firm_age<=5, 1,0)]
    firm_empl_rev_young_regression_fe<-feols(empl_growth  ~ nq_growth*young |  factor(NACE_BR) + factor(firmid) , data=data)
    firm_empl_rev_young_regression_fe_t<-feols(empl_growth  ~ nq_growth*young |  factor(NACE_BR) + factor(firmid) +factor(year), data=data)
    
    data[, `:=`(micro=ifelse(empl<=10, 1,0),
                small=ifelse(empl>10 & empl<=50, 1,0),
                medium=ifelse(empl>50 & empl<=250, 1,0),
                large=ifelse(empl>250 , 1,0))]
    firm_empl_rev_size_regression_fe<-feols(empl_growth  ~ nq_growth*small + nq_growth*medium + nq_growth*large |  factor(NACE_BR) + factor(firmid) , data=data)
    firm_empl_rev_size_regression_fe_t<-feols(empl_growth  ~ nq_growth*small + nq_growth*medium + nq_growth*large |  factor(NACE_BR) + factor(firmid) +factor(year), data=data)

    
    models<-list(firm_empl_rev_regression, 
                 firm_empl_rev_regression_fe,
                 firm_empl_rev_regression_fe_t,
                 firm_empl_rev_young_regression_fe,
                 firm_empl_rev_young_regression_fe_t,
                 firm_empl_rev_size_regression_fe,
                 firm_empl_rev_size_regression_fe_t)
    modelsummary(models, output=paste0(output_dir, "Export/", level, "/firm_empl_rev_regressions.tex"))
    
  }else{
    data<-data[, n_products_industry:=n_distinct(prodfra_plus), by=nace_var]
    
    # Run regressions
    product_regression<-lm(high_growth ~ 0+factor(get(nace_var)) + n_products_industry, data = data)
    product_regression_unweighted_positive<-lm(high_growth ~ 0+factor(get(nace_var)) + n_products_industry, data = data[data$delta_var>0, ])
    product_regression_weighted_positive<-lm(high_growth ~ 0+factor(get(nace_var)) + n_products_industry, weight=delta_var, data = data[data$delta_var>0, ])
    
  }

  #Create a loop to export regression results in xlsx format and graphs for estimates
  suffix<-c("", "_unweighted_positive", "_weighted_positive")
  for(j in 1:length(suffix)){
    regression<-get(paste0("product_regression", suffix[j]))
    summary_reg<-summary(regression)
    reg_table<-as.data.frame(cbind(summary_reg$coefficients[, 1:2], summary_reg$coefficients[, 3:4]))
    reg_table$variable<-rownames(reg_table)
    reg_table<-reg_table %>% select(variable, everything())
    write.xlsx(reg_table, paste0(output_dir, "Export/", level, "/Regression_", level, "_sector_n_products", suffix[j], ".xlsx"))
    assign(paste0("model_", i), regression)
    
    ggplot(reg_table, aes(x=Estimate, y=reorder(rownames(reg_table), Estimate), xmin=Estimate - (1.96*`Std. Error`), xmax=Estimate + (1.96*`Std. Error`))) +
      geom_point() + 
      geom_errorbarh(height=0)+
      geom_vline(xintercept=0, linetype="dashed", color="red")+
      labs(title="Estimate and Confidence Interval", subtitle=paste0("Sample: ", descriptions[i]), x="Estimate", y="")+
      theme(axis.text.y=element_blank())
    ggsave(paste0(output_dir, "Export/", level, "/coef_plot_", level, suffix[j], ".png"))
    
  }
}



write(texreg(list(model_1, model_2, model_3, model_4)), file = paste0(output_dir, "Regression_rev_sector.txt"))


i<-5
high_growth_threshold<-0.10

data<-get(paste0("regression_data_", i))
data$ref_NACE<-relevel(factor(data$NACE_BR), ref="2790")
data<-data %>% mutate(high_growth_firm=ifelse(empl_growth>high_growth_threshold, 1,0))

firm_regression<-lm(high_growth_firm ~ get(var), weights = abs(empl-empl_l), data = data )

summary_reg<-summary(firm_regression)
reg_table<-as.data.frame(cbind(summary_reg$coefficients[, 1:2], summary_reg$coefficients[, 3:4]))
reg_table$variable<-rownames(reg_table)
reg_table<-reg_table %>% select(variable, everything())
write.xlsx(reg_table, paste0(output_dir, paste0("Regression_rev_sector_", i, ".xlsx")))
assign(paste0("model_", i), firm_regression)

ggplot(reg_table, aes(x=Estimate, y=reorder(rownames(reg_table), Estimate), xmin=Estimate - (1.96*`Std. Error`), xmax=Estimate + (1.96*`Std. Error`))) +
  geom_point() + 
  geom_errorbarh(height=0)+
  geom_vline(xintercept=0, linetype="dashed", color="red")+
  labs(title="Estimate and Confidence Interval", subtitle=paste0("Sample: ", descriptions[i]), x="Estimate", y="")+
  theme(axis.text.y=element_blank())
ggsave(paste0(output_dir,"coef_plot_", i, ".png"))







texreg(list(model_1, model_2, model_3, model_4))


  



firm_empl_rev$ref_age<-relevel(firm_empl_rev$age_bucket, ref="10-20")
firm_empl_rev$ref_empl<-relevel(firm_empl_rev$empl_bucket, ref="20-50")
firm_empl_rev$ref_industry<-relevel(factor(firm_empl_rev$industry), ref="Paper")
firm_empl_rev$ref_sector<-relevel(factor(firm_empl_rev$sector), ref="Manufacturing")


var_time<-paste0(var, "_year")
firm_empl_rev[[var_time]]<-factor(paste())


firm_empl_rev<-firm_empl_rev %>% mutate(log_rev=log(rev))
firm_empl_rev <- firm_empl_rev %>% mutate(log_empl=log(empl))

product_rev_g_reg<-lm(empl_growth ~ ref_age + ref_empl + ref_industry, data = firm_empl_rev )
# product_rev_g_reg<-lm(rev_growth ~ ref_age +  ref_empl + ref_sector, data = firm_empl_rev )
summary_reg<-summary(product_rev_g_reg)
reg_table<-as.data.frame(cbind(summary_reg$coefficients[, 1:2], summary_reg$coefficients[, 3:4]))
reg_table$variable<-rownames(reg_table)
reg_table<-reg_table %>% select(variable, everything())
write.xlsx(reg_table, paste0(output_dir, "Regression_age_empl_industry.xlsx"))
write(texreg(product_rev_g_reg), file = paste0(output_dir, "Regression_age_empl_industry.txt"))





model= log_growth ~ factor(age_bucket) +  factor(empl_bucket) + factor(sector) 


superstar <- merge(superstar, nace_DEFind, by.x="NACE", by.y = "nace", all.x = T)

colors<-rainbow(7)
# set.seed(1996)
# colors<-sample(colors)
var_fill<-"age_bucket"

ggplot(superstar, aes(x=n_obs))+
  geom_histogram(binwidth = 20, color="skyblue") + 
  scale_fill_manual(values=colors)


ggplot(superstar, aes(x=rev_growth, fill=factor(get(var_fill))))+
  geom_histogram(binwidth = 0.05, color="black") + 
  scale_fill_manual(values=colors)

ggplot(superstar %>% filter(rev_growth>0.5 & rev_growth<2), aes(x=rev_growth, fill=factor(get(var_fill))))+
  geom_histogram(binwidth = 0.05, color="black") + 
  scale_fill_manual(values=colors)

ggplot(superstar %>% filter(rev_growth<(-0.5) & rev_growth>(-2)), aes(x=rev_growth, fill=factor(get(var_fill))))+
  geom_histogram(binwidth = 0.05, color="black") + 
  scale_fill_manual(values=colors)


ggplot(superstar %>% filter(gap==0), aes(x=rev_growth))+
  geom_histogram(binwidth = 0.05, fill="skyblue") + 




ggplot(superstar, aes(x=rev_growth))+
  geom_density(fill="skyblue")


hist(superstar$rev_growth)
