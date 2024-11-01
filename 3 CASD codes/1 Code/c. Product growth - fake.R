"
This file uses firm (FARE/FICUS) and product data (EAP) 

Part 1 and 1* clean the data

Part 2 graphs the distributions of annual product revenue growth and annual firm employment growth and colors the graph by industry.
It is a first attempt at answering: Which were the key industries in terms of product growth and labor growth.

Part 3 is mainly focused on analysing the elasticity of employment growth and capital growth to revenue growth.
These regressions include age, size and superstar status controls
It uses two samples: one with only continuing firms, other with both continuing and exiting firms.
It runs both weighted and unweighted regressions.

Part 4 creates graphs of high growth vs. high decline for NACE industries


Author: Juli?n D?az-Acosta
Last update: 18/09/2024
"



# setup -------------------------------------------------------------------

source("C:/Users/nb/Dropbox/Reallocation - shared folder/Code/IWH_code/3 CASD codes/1 Code/Main - fake.R")
output_dir<-paste0(output_dir, "Product reallocation and firm dynamics/Export 09.11/")
output_dir_creator(output_dir)

#1) Data import and cleaning (fixing NACE codes, reconstructing lag and reallocation measures, selecting only relevant variables) ------------------------------------

# Bring in firm data, product data and NACE industry information
firm_data<-readRDS('sbs_br_combined_cleaned.rds') #Coming from "a. Data preparation.R" part 2
product_data<-readRDS(paste0("product_level_growth_", filter_indicator,  "_.RDS")) #Coming from "a. Data preparation.R" part 4
nace_DEFind <- fread("nace_DEFind.conc", colClasses = c('character'))
nace_ind_sector <- read_excel("NACE_industry_sector.xlsx")

firm_data_select <- firm_data %>% select(firmid, year, 
                                         empl, empl_l, empl_bar, empl_growth,
                                         nq, nq_l, nq_bar, 
                                         capital, capital_l, capital_bar, capital_growth, 
                                         born, died, size, young,
                                         NACE_BR, firm_birth_year, nq, firm_age, NACE_2d,
                                         high_tech, superstar) 
setDT(firm_data_select)

#Bring in lagged variables to firm data and create measures of firm-level revenue growth using revenue information from the business registry
# firm_data_select<-merge(firm_data_select, firm_data_lag, by=c("firmid", "year"), all.x = T)
# firm_data_select[, nq_bar := .5*(nq + nq_l)]
firm_data_select[, nq_growth := ifelse(nq_bar != 0, (nq - nq_l)/nq_bar, 0)]
firm_data_select[, nq_reallocation := ifelse(nq_bar != 0, abs(nq - nq_l)/nq_bar, 0)]
saveRDS(firm_data_select, "firm_data_select.RDS")

#Conduct NACE concordance
# CAUTION: the concordance operation can take up to 8 hours to run
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
saveRDS(test, "firm_data_select_NACE_concorded.RDS")
#After concording, I noticed some firms may not have been well concorded (e.g. firm 05480546,
# for which NACE code up to 2008 is concorded as 2222)
#Because of this, for now I carry on with the analysis without the concordance

#1*) Data import and cleaning (calculate industry HHI and median size, select only prodcom firms/sector, create product growth dataset)------------------------------------

#Bring in necessary firm and product information
firm_data_select<-readRDS("firm_data_select.RDS")
product_data<-readRDS(paste0("product_level_growth_", filter_indicator,  "_.RDS"))
nace_DEFind <- fread("nace_DEFind.conc", colClasses = c('character'))
nace_ind_sector <- read_excel("NACE_industry_sector.xlsx")

setDT(firm_data_select)

#Calculate industry HHI
total_revenue<-firm_data_select[, .(total_revenue=sum(nq, na.rm=T)), by=.(year, NACE_BR)]
total_revenue<-merge(firm_data_select, total_revenue, by=c("year", "NACE_BR"))
total_revenue[, market_share:=nq/total_revenue]
total_revenue[, market_share_squared:=market_share^2]
hhi<-total_revenue[, .(HHI_industry=sum(market_share_squared, na.rm=T),
                       av_firm_size_empl=mean(empl, na.rm=T),
                       median_firm_size_empl=median(empl, na.rm=T),
                       av_firm_size_nq=mean(nq, na.rm=T),
                       median_firm_size_nq=median(nq, na.rm=T)), by=.(year, NACE_BR)]
total_revenue<-NULL; gc() #Delete total_revenue and clean revenue
# firm_data_select<-merge(firm_data_select, hhi, by=c("year", "NACE_BR"))

#Identify prodocm firms and sectors for filtering later
prodcom_firms<-unique(product_data$firmid)
prodcom_sectors<-c("B", "C") #Prodcom usually covers sectors B, C, D and E, but we decided to exclude sectors D and E. If this changes, we should change this

#Clean firm data with appropriate years and select firms/sectors in FARE/FICUS that have been covered by PRODCOM
firm_data_select<- firm_data_select[year>=2008, ] # If NACE has not been concorded between v1.1 and v2, we remove observations prior to 2008
firm_data_select <- merge(firm_data_select, nace_DEFind, by.x="NACE_BR", by.y = "nace", all.x = T)
firm_data_select[, sector_NACE:=substr(DEFind,1,1)]
firm_data_select$DEFind<-NULL
firm_data_select[, birth_year := min(year), by = firmid]
firm_data_select[!is.na(birth_year) & birth_year == year, empl_l:= 0]
firm_data_lag<-NULL




## Filter by prodcom firms or sectors
firm_data_select_prodcom_firms<-firm_data_select[firmid %in% prodcom_firms]
firm_data_select_prodcom_sectors <- firm_data_select[ sector_NACE %in% prodcom_sectors]

# Save firm_data with only relevant variables and firms in industries covered by prodcom
saveRDS(firm_data_select_prodcom_firms, "sbs_br_data_prodcom_firms.RDS")
saveRDS(firm_data_select_prodcom_sectors, "sbs_br_data_prodcom_sectors.RDS")

# Select either prodcom_firms or prodcom_sectors dataset to continue
firm_data_select<-firm_data_select_prodcom_firms

# Create and save dataframe with firm-level revenue from prodcom and other firm information from FARE/FICUS
product_data_aggr<-product_data %>% group_by(firmid, year) %>% summarise(rev_agg=sum(rev))
firm_empl_rev <- merge(firm_data_select, product_data_aggr, by=c("firmid", "year"), all.x=T) 
firm_empl_rev <- firm_empl_rev %>% mutate(rev_diff=nq-rev_agg)
firm_empl_rev <- firm_empl_rev %>% group_by(firmid) %>% mutate(firm_birth_year=ifelse(is.na(firm_birth_year), min(firm_birth_year, na.rm = T), firm_birth_year))
# saveRDS(firm_empl_rev, "product_data_aggr_firm_empl_rev.RDS")

product_data<-product_data[, -"NACE_BR"]

# Create and save dataframe with product-level revenue and firm level information
product_data <- merge(product_data, firm_empl_rev, by=c("firmid", "year"), all.x=T)

# Identify firm-product lines for which there is gap in coverage
product_data_temp <- product_data[, .(gap_ind=as.integer(any(gap==1))), by=.(firmid, prodfra_plus)] 
table(product_data_temp$gap_ind)
# 8.329 out of 153,255 (5,4%) firm-product lines have gaps 

#Delete firm-product lines for which there is gap in coverage
product_data <- merge(product_data, product_data_temp, by=c("firmid", "prodfra_plus"))
product_data <- product_data[gap_ind!=1]
# deletes 66.654 (6,1%) out of 1.078.159 observations 
product_data$gap_ind<-NULL
rm(product_data_temp); gc()

#Create annual product growth dataset with relevant information
product_data <- product_data[, within_economy_rev_share_test :=  rev_bar/ sum(rev_bar, na.rm = T), by = .(year)]
product_data <- product_data[, rev_growth :=  ((rev-rev_l)/rev_bar)*within_economy_rev_share_test]
product_data <- product_data[, rev_reallocation :=  (abs(rev-rev_l)/rev_bar)*within_economy_rev_share_test]
#I reconstruct measures of aggregate revenue growth and reallocation based on the filtering I did before (review?)

product_growth_annual <- product_data %>% group_by(year, 
                                                   prodfra_plus) %>% summarise(age=mean(firm_age, na.rm = T),
                                                                               age_w=sum((firm_age*rev), na.rm = T)/sum(rev, na.rm = T),
                                                                               empl=mean(empl, na.rm = T),
                                                                               empl_w=sum((empl*rev), na.rm = T)/sum(rev, na.rm = T),
                                                                               # empl_w=weighted.mean(empl, rev, na.rm=T),
                                                                               rev_first=sum(rev*first_introduction, na.rm=T),
                                                                               rev_reintroduced=sum(rev*reintroduced, na.rm=T),
                                                                               rev_discontinued=sum(rev*discontinued, na.rm=T),
                                                                               rev_incumbent=sum(rev*incumbent, na.rm=T),
                                                                               rev_gap=sum(rev*gap, na.rm=T),
                                                                               rev=sum(rev, na.rm = T),
                                                                               rev_l=sum(rev_l, na.rm=T),
                                                                               within_economy_rev_share = sum(within_economy_rev_share_test, na.rm = T),
                                                                               rev_growth = sum(rev_growth, na.rm = T),
                                                                               rev_reallocation = sum(rev_reallocation, na.rm = T),
                                                                               first_introduction=sum(first_introduction),
                                                                               reintroduced=sum(reintroduced),
                                                                               discontinued=sum(discontinued),
                                                                               incumbent=sum(incumbent),
                                                                               gap=sum(gap),
                                                                               # rev_bar=sum(rev_bar, na.rm=T),
                                                                               n_obs=n())
test<-product_growth_annual %>% group_by(year) %>% summarise(within_economy_rev_share=sum(within_economy_rev_share),
                                                             rev_growth=sum(rev_growth),
                                                             rev_reallocation=sum(rev_reallocation))

#Growth and industry variables
product_growth_annual <- product_growth_annual %>% mutate(rev_bar=(rev + rev_l)/2,
                                                          rev_growth=ifelse(rev_bar==0, 0, ((rev - rev_l)/rev_bar)) ,
                                                          # rev_reallocation=ifelse(rev_bar==0, 0, (abs(rev - rev_l)/rev_bar)) ,
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
product_growth_annual<- product_growth_annual %>% select(prodfra_plus, rev_growth, rev_reallocation, year, industry, empl_bucket, age_bucket, sector, everything())
setorder(product_growth_annual, rev_growth, age_bucket, empl_bucket, sector, industry)

saveRDS(hhi, "br_industry_HHI.RDS")
saveRDS(product_data, "product_firm_data_regressions.RDS")
saveRDS(product_growth_annual, "product_growth_annual.RDS")
saveRDS(firm_empl_rev, "product_data_aggr_firm_empl_rev.RDS")

#2) Distributions of annual product revenue growth and annual firm employment growth by industry ------------------------------------

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
    
    #Distributions of product level revenue growth
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
# Compared to product_firm_data, product_data_regression has fewer firms, the result of the drop of products with gaps in the previous step
# We keep product_firm_data_regressions for consistency in the analysis of this module
product_firm_data_regressions<-readRDS("product_firm_data_regressions.RDS")
hhi<-readRDS("br_industry_HHI.RDS")
# status_restructuring<-readRDS("BR_status_restructuring.RDS")

# firm_empl_rev<-merge(firm_empl_rev, status_restructuring, by=c("firmid", "year"), all.x=T)
# test<-firm_empl_rev %>% filter(status==1 & died==T)
# test<-firm_empl_rev %>% filter(status==1 & died==T)
# firmids_3_4<-unique(test$firmid)
# test<-firm_empl_rev %>% filter(firmid %in% firmids_3_4) %>% select(firmid, year, status, died, empl_growth, nq_growth, everything()) 
# setorder(test, firmid, year)
# I bring in statistical status of the firm information from FARE
# Code 1 means active, 2 Pr?sum?e inactive, 3	Inactive - Mise en sommeil, 4	Cess?e, 
# 6	Poursuite de l'activit? suite ? d?c?s de l'exploitant ou dissolution, 9	Inactive statistique
# I conclude that "died" is a better approximation of firm exit than codes 3, 4 or 9
# Below is the result of table(firm_empl_rev$status, firm_empl_rev$died)
#   FALSE TRUE
# 1  6906   88
# 2   267   13
# 3    24   22
# 4    41   42
# 6     1    0
# 9     0    1
# Many (88) firms identified as exiting (died==TRUE) are classified as active (1)
# Conversely, many firms (41) classified as closed (4) still show activity in terms of employment and revenue (died==FALSE)

datasets<-c("firm_empl_rev", "product_firm_data_regressions")

if(!final){
  unique_firmids<-unique(firm_empl_rev$firmid)
  sample_size<-round(0.01*length(unique_firmids))
  sampled_firmids<-sample(unique_firmids, sample_size)
  
  for(dataset in datasets){
    data<-get(dataset)
    setDT(data)
    assign(dataset, data[firmid %in% sampled_firmids])
  }
  
  rm(data);gc()
}

product_firm_data_regressions[, `:=`(NACE_product=substr(prodfra_plus, 1,4),
                                   NACE_2d_product=substr(prodfra_plus, 1,2),
                                   NACE_2d_BR=substr(NACE_BR, 1,2))]

#Define filters for the regressions
regression_data_product<-product_growth_annual %>% filter(year>2009, gap==0, rev_growth<2, rev_growth>-2)
regression_data_firm<-firm_empl_rev %>% filter(year>2009, empl_growth<2, empl_growth>-2, nq_growth<2, nq_growth>-2, size!="micro")
# test<-firm_empl_rev %>% filter(year>2009, !born, !died)
regression_data_firm_exiters<-firm_empl_rev %>% filter(year>2009, empl_growth<2,  nq_growth<2) %>% mutate(exit=ifelse(nq_growth==-2 | empl_growth==-2, T, F))
regression_data_firm_product<-product_firm_data_regressions %>% filter(year>2009, rev_growth<2, rev_growth>-2)


# Define vectors with variables, thresholds and descriptions
levels<-c("product", "firm", "firm_exiters", "firm_product")
growth_vars<-c("rev", "empl", "empl", "nq")
unit_vars<-list("prodfra_plus", "firmid", "firmid", c("firmid", "prodfra_plus"))
nace_vars<-c("NACE", "NACE_BR", "NACE_BR", "NACE_product")
high_growth_thresholds<-c(0.25, 0.3, 0.3, 0.25)
descriptions<-c("product revenue growth, entering and exiting products removed",
                "employment growth, entering and exiting firms removed",
                "employment growth, entering firms removed",
                "firm-product revenue growth, entering and exiting products removed")



# Regression of annual product revenue growth on average age, average employment and sector-----------------------





for (i in 2:3){
  # i<-3
  #Define relevant variables
  level<-levels[i]
  var<-growth_vars[i]
  var_l<-paste0(var, "_l")
  var_growth<-paste0(var, "_growth") 
  unit_var<-unit_vars[[i]]
  high_growth_threshold<-high_growth_thresholds[i]
  high_decline_threshold<- high_growth_threshold*(-1)
  nace_var<-nace_vars[i]
  
  #Get the data
  data<-get(paste0("regression_data_", level))
  setDT(data)
  data<-merge(data, hhi, by.x=c(nace_var, "year"), by.y=c("NACE_BR", "year"), all.x=T)
  
  # Create risk-adjusted version of growth
  data<-data[, `:=`(growth_sd= sd(get(var_growth), na.rm=T),
                    growth_adj=get(var_growth)/sd(get(var_growth), na.rm=T)), by=unit_var]
  
  # Get an analogue for high growth threshold in terms of the risk-adjusted version of growth
  ecdf_growth<-ecdf(data[[var_growth]])
  prob_high_growth<-ecdf_growth(high_growth_threshold)
  prob_high_decline<-ecdf_growth(high_decline_threshold)
  
  growth_adj_thresh<-quantile(data$growth_adj, probs=prob_high_growth, na.rm = T)
  decline_adj_thresh<-quantile(data$growth_adj, probs=prob_high_decline, na.rm = T)
  
  print(paste0("The analogue of ", var, " high growth threshold (", high_growth_threshold, ") is ", round(growth_adj_thresh, 2), " in terms of SDs"))
  print(paste0("The analogue of ", var, " high decline threshold (", high_decline_threshold, ") is ", round(decline_adj_thresh, 2), " in terms of SDs"))
  
  #"The analogue of rev high growth threshold (0.25) is 0.91 in terms of SDs"
  #"The analogue of empl high growth threshold (0.3) is 1.45 in terms of SDs"
  #"The analogue of empl high growth threshold (0.3) is 1.37 in terms of SDs"
  #"The analogue of rev high growth threshold (0.25) is 1.15 in terms of SDs"
  
  # Define additional variables for regression
  data<-data[, high_growth:=ifelse(growth_adj>=growth_adj_thresh,1,0)]
  data<-data[, high_decline:=ifelse(growth_adj<=decline_adj_thresh,1,0)]
  data<-data[, delta_var:=get(var)-get(var_l)]
  
  # Especial regressions
  if(level=="firm" | level=="firm_exiters"){
    
    # Regressions to establish the share of products in a sector with high growth or decline
    product_regression_unweighted_positive<-lm(high_growth ~ 0+factor(get(nace_var)) + HHI_industry, data = data[data$delta_var>0, ])
    product_regression_unweighted_negative<-lm(high_decline ~ 0+factor(get(nace_var)) + HHI_industry, data = data[data$delta_var<0, ])
    product_regression_weighted_positive<-lm(high_growth ~ 0+factor(get(nace_var)) + HHI_industry, weight=delta_var, data = data[data$delta_var>0, ])
    product_regression_weighted_negative<-lm(high_decline ~ 0+factor(get(nace_var)) + HHI_industry, weight=abs(delta_var), data = data[data$delta_var<0, ])
    
    
    measures<-c("empl", "capital")

    for(measure in measures){

      # measure<-"capital"
      # Define average factor measure between t and t-1
      measure_bar<-paste0(measure, "_bar")
      data[, (measure_bar):=(get(measure)+get(paste0(measure, "_l")))/2]
      factor_vars<-c(paste0(measure, "_growth"))
      
      # Deine weighting variables
      weights<-list(NULL, paste0(measure, "_bar"), measure)
      
      for(factor_var in factor_vars){
        
        # factor_var<-factor_vars[2]
        for (w in weights) {
          
          # w<-weights[2]
          w<-unlist(w)
          
          # Create a placeholder for storing elasticities through time
          results<-data.frame(
            year=character(),
            point_estimate=numeric(),
            conf_low=numeric(),
            conf_high=numeric(),
            regression=character()
          )
          
          # Create the list of available years and include the complete sample 
          years<-c("All", unique(data$year))
          
          for(yr in years){
            
            # yr<-"All"
            print(paste0("Starting with weight ", w, "factor ", measure, " variable ", factor_var, " and year ", yr))
            
            # Subset the data based on the year
            if(yr=="All"){
              data_subset<-data
            }else{
              data_subset<-data[year==yr]
            }
            
            # Set variables for weights and firm size classification
            if(is.null(w)){
              wt<-NULL
              var<-"empl_bar"
            }else{
              wt<-data_subset[[w]]
              if(measure=="empl"){
                var<-w
              }else{
                var<-"empl_bar"
              }
            }
            
            
            regression_vars<-c("young", "size", "superstar", "factor(year)", "factor(NACE_BR)")
            

            # Set formulas to run
            f_empl_rev<-as.formula(paste0(factor_var, "~ nq_growth"))
            f_empl_rev_young<-as.formula(paste0(factor_var, "~ nq_growth*young"))
            f_empl_rev_size<-as.formula(paste0(factor_var, "~ nq_growth*size"))
            f_empl_rev_superstar<-as.formula(paste0(factor_var, "~ nq_growth*superstar"))
            # f_empl_rev_high_tech<-as.formula(paste0(factor_var, "~ nq_growth*high_tech"))
            f_empl_rev_young_size_superstar<-as.formula(paste0(factor_var, "~ nq_growth*young + nq_growth*size + nq_growth*superstar"))
            
            formulas<-list("f_empl_rev", "f_empl_rev_young", "f_empl_rev_size", "f_empl_rev_superstar",  "f_empl_rev_young_size_superstar")
            for(formula in formulas){
              assign(paste0(formula, "_t"), as.formula(paste0(paste(deparse(get(formula)), collapse=""), " | factor(year)")))
              assign(paste0(formula, "_t_nace"), as.formula(paste0(paste(deparse(get(formula)), collapse=""), " | factor(year) + factor(NACE_BR)")))
            }

            # Base regressions of factor growth on revenue growth
            firm_empl_rev_regression<-lm(f_empl_rev, data=data_subset, weights = wt)
            firm_empl_rev_regression_t<-feols(f_empl_rev_t, data=data_subset, weights = wt)
            firm_empl_rev_regression_t_nace<-feols(f_empl_rev_t_nace , data=data_subset, weights = wt)
            
            # Regressions of factor growth on revenue growth + young/established dummies
            # data_subset[, young:=ifelse(firm_age<=5, 1,0)]
            firm_empl_rev_young_regression_t<-feols(f_empl_rev_young_t  , data=data_subset, weights = wt)
            firm_empl_rev_young_regression_t_nace<-feols(f_empl_rev_young_t_nace, data=data_subset, weights = wt)

            # Create size variables for regressions
            # data_subset[, size:=case_when(get(var)<1 ~ "micro",
            #                               get(var)>=1 & get(var)<50 ~ "small",
            #                               get(var)>=50 & get(var) < 250 ~ "medium",
            #                               get(var)>=250 & get(var) < 30000 ~ "large",
            #                               get(var)>=30000 ~ "extra large")]
            # 
            # data_subset$size<-factor(data_subset$size, levels=c("micro", "small", "medium", "large", "extra large"))
            data_subset$size<-factor(data_subset$size, levels=c("small", "medium", "large", "extra large"))
            # data_subset<-data_subset[size!="micro"]

            # Set variables for weights and firm size classification
            if(is.null(w)){
              wt<-NULL
              var<-"empl_bar"
            }else{
              wt<-data_subset[[w]]
              if(measure=="empl"){
                var<-w
              }else{
                var<-"empl_bar"
              }
            }
          
            # Regressions of factor growth on revenue growth + size dummies
            firm_empl_rev_size_regression_t<-feols(f_empl_rev_size_t, data=data_subset, weights=wt)
            firm_empl_rev_size_regression_t_nace<-feols(f_empl_rev_size_t_nace, data=data_subset, weights=wt)
            

            # Regressions of factor growth on superstar dummies
            firm_empl_rev_superstar_regression_t<-feols(f_empl_rev_superstar_t, data=data_subset, weights=wt)
            firm_empl_rev_superstar_regression_t_nace<-feols(f_empl_rev_superstar_t_nace, data=data_subset, weights=wt)
            
            # Regressions of factor growth on revenue growth + young/established dummies +  size dummies
            firm_empl_rev_young_size_superstar_regression_t<-feols(f_empl_rev_young_size_superstar_t,  data=data_subset, weights=wt)
            firm_empl_rev_young_size_superstar_regression_t_nace<-feols(f_empl_rev_young_size_superstar_t_nace, data=data_subset, weights=wt)
            
            # Compile the models we ran
            models<-list(firm_empl_rev_regression, 
                         firm_empl_rev_regression_t,
                         firm_empl_rev_regression_t_nace,
                         firm_empl_rev_young_regression_t,
                         firm_empl_rev_young_regression_t_nace,
                         firm_empl_rev_size_regression_t,
                         firm_empl_rev_size_regression_t_nace,
                         firm_empl_rev_superstar_regression_t,
                         firm_empl_rev_superstar_regression_t_nace,
                         firm_empl_rev_young_size_superstar_regression_t,
                         firm_empl_rev_young_size_superstar_regression_t_nace)
            
            # Populate the table with the results of the elasticities through time
            for (k in seq_along(models)){
              # k<-9
              if(class(models[[k]])=="lm"){
                fit<-models[[k]]
                tidy_fit<-tidy(fit, conf.int=T)
                estimate_cis<-tidy_fit[tidy_fit$term == "nq_growth", ]
                
                results<-rbind(results, data.table(
                  regression=gsub("factor\\((.*?)\\)",   "FE: \\1",
                                  paste(intersect(unlist(strsplit(deparse(formula(models[[k]])), split=" ")), 
                                                  regression_vars), collapse = ", ")),
                  point_estimate=estimate_cis$estimate,
                  conf_low=estimate_cis$conf.low,
                  conf_high=estimate_cis$conf.high,
                  year=yr
                ))
              }else{
                fit<-models[[k]]
                conf<-confint(fit, parm="nq_growth")
                point_estimate<-coef(fit)["nq_growth"]
                
                results<-rbind(results, data.table(
                  regression=gsub("factor\\((.*?)\\)",   "FE: \\1",
                                  paste(intersect(unlist(strsplit(deparse(formula(models[[k]])), split=" ")), 
                                                  regression_vars), collapse = ", ")),
                  point_estimate=point_estimate,
                  conf_low=conf[1,1],
                  conf_high=conf[1,2],
                  year=yr
                ))
              

              }
            }
            
            if(yr=="All"){
              
                    
              
              #Export regression tables
              print("Exporting results table")
              
              level_dir<-paste0(output_dir, level, "/")
              if(!dir.exists(level_dir)){
                dir.create(level_dir)
              }
              

              measure_dir<-paste0(level_dir, measure, "/")
              if(!dir.exists(measure_dir)){
                dir.create(measure_dir)
              }
              
              factor_dir<-paste0(measure_dir, factor_var, "/")
              if(!dir.exists(factor_dir)){
                dir.create(factor_dir)
              }
              
              if (is_null(w)){
                #Eport summary stats
                print("Exporting summary stats")
                # make_summary_stats(data_subset, c("empl", "nq", "firm_age", "empl_growth", "nq_growth", "capital_growth", "capital_reallocation", "superstar", "high_tech"), 
                #                    "full_sample", paste0("firm_", factor_var, "_", if (is_null(w)) "unweighted" else "weighted_", w, "summary_stats_full_sample"))
                make_summary_stats(data_subset, c("empl", "nq", "firm_age", "empl_growth", "nq_growth", "capital_growth", "superstar", "high_tech"), 
                                   "size", paste0(level, "/", measure, "/", factor_var, "/", "firm_", factor_var, "_", if (is_null(w)) "unweighted" else "weighted_", w, "_summary_stats_size"))
              }
              
              
              modelsummary(models, output=paste0(factor_dir, "/firm_", factor_var, "_rev_regressions_", if (is_null(w)) "unweighted" else "weighted_", w, ".tex"), 
                           stars = T, 
                           gof_omit = "Std.Errors|R2 Within|R2 Within Adj.|AIC|BIC|Log.Lik.|F",
                           title=paste0("Regression of firm ", gsub(paste0("_"), " ", factor_var),  " on revenue growth. ",
                                        "Weight: ", if (is_null(w)) "unweighted" else gsub(paste0("_"), " ", w),
                                        ". Sample: Firms covered by Prodcom")
                           )
              
            }
          }
          
          # Save the table of results
          assign(paste0("results_", if (is.null(w)) "unweighted" else "weighted_", w), results)
          
          ggplot(results[year!="All"] %>% mutate (year=as.numeric(year)), aes(x=year, y=point_estimate, color=regression)) + 
            geom_point() + 
            geom_errorbar(aes(ymin=conf_low, ymax=conf_high), width=0.2) + 
            geom_line()+
            coord_cartesian(ylim=c(min(0, results$conf_low), NA))+
            theme_minimal() + 
            theme()+#legend.position = "bottom",
                  #legend.text = element_text(size=6),
                  #legend.key.size = unit(1, "cm"))+
            labs(title=paste0("Estimate of the effect of revenue growth on ", factor_var , " - Different specifications"),
                 subtitle = paste0(if (is.null(w)) "Unweighted" else "Weighted by: ", w),
                 x="Year",
                 y="Point Estimate with 95% Confidence Intervals")
          ggsave(paste0(factor_dir, "/firm_", factor_var, "_rev_regressions_", if (is_null(w)) "unweighted" else "weighted_", w, ".png"))
          
          
        }
        
      }
      
    }


    
  }else{
    data<-data[, n_products_industry:=n_distinct(prodfra_plus), by=nace_var]
    
    product_regression_unweighted_positive<-lm(high_growth ~ 0+factor(get(nace_var)) + HHI_industry, data = data[data$delta_var>0, ])
    product_regression_unweighted_negative<-lm(high_decline ~ 0+factor(get(nace_var)) + HHI_industry, data = data[data$delta_var<0, ])
    product_regression_weighted_positive<-lm(high_growth ~ 0+factor(get(nace_var)) + HHI_industry, weight=delta_var, data = data[data$delta_var>0, ])
    product_regression_weighted_negative<-lm(high_decline ~ 0+factor(get(nace_var)) + HHI_industry, weight=abs(delta_var), data = data[data$delta_var<0, ])
    
  }
}



# Regression of elasticities of factor growth to revenue growth on positive and negative shocks


datasets<-c("firm_empl_rev", "product_firm_data_regressions")

if(!final){
  unique_firmids<-unique(firm_empl_rev$firmid)
  sample_size<-round(0.01*length(unique_firmids))
  sampled_firmids<-sample(unique_firmids, sample_size)
  
  for(dataset in datasets){
    data<-get(dataset)
    setDT(data)
    assign(dataset, data[firmid %in% sampled_firmids])
  }
  
  rm(data);gc()
}

product_firm_data_regressions[, `:=`(NACE_product=substr(prodfra_plus, 1,4),
                                     NACE_2d_product=substr(prodfra_plus, 1,2),
                                     NACE_2d_BR=substr(NACE_BR, 1,2))]


# Regression of annual product revenue growth on average age, average employment and sector-----------------------




for (i in 2:3){
  # i<-3
  #Define relevant variables
  level<-levels[i]
  var<-growth_vars[i]
  var_l<-paste0(var, "_l")
  var_growth<-paste0(var, "_growth") 
  unit_var<-unit_vars[[i]]
  high_growth_threshold<-high_growth_thresholds[i]
  high_decline_threshold<- high_growth_threshold*(-1)
  nace_var<-nace_vars[i]
  
  #Get the data
  data_subset<-get(paste0("regression_data_", level))
  setDT(data_subset)

    if(level=="firm" | level=="firm_exiters"){

    factor_vars<-c("empl", "capital")
    
      for(factor_var in factor_vars){
        
        
        rev_young_size_superstar<-as.formula(paste0(paste0(factor_var, "_growth"), "~ nq_growth*young + nq_growth*size + nq_growth*superstar | factor(year) + factor(NACE_BR)"))
        
        data_subset$size<-factor(data_subset$size, levels=c("small", "medium", "large", "extra large"))
        
        wt<-data_subset[[paste0(factor_var, "_bar")]]
        
        data_positive<-data_subset %>% filter(nq_growth>0)
        wt_positive<-data_positive[[paste0(factor_var, "_bar")]]
        
        data_negative<-data_subset %>% filter(nq_growth<0)
        wt_negative<-data_negative[[paste0(factor_var, "_bar")]]
        
        
        rev_young_size_superstar_regression_t_nace_weighted_all<-feols(rev_young_size_superstar, data=data_subset, weights=wt)
        rev_young_size_superstar_regression_t_nace_unweighted_all<-feols(rev_young_size_superstar, data=data_subset)
        rev_young_size_superstar_regression_t_nace_weighted_positive<-feols(rev_young_size_superstar, data=data_positive, weights=wt_positive)
        rev_young_size_superstar_regression_t_nace_unweighted_positive<-feols(rev_young_size_superstar, data=data_positive)
        rev_young_size_superstar_regression_t_nace_weighted_negative<-feols(rev_young_size_superstar, data=data_negative, weights=wt_negative)
        rev_young_size_superstar_regression_t_nace_unweighted_negative<-feols(rev_young_size_superstar, data=data_negative)
        
        samples<-c("all", "positive", "negative")
        weight_options<-c("weighted", "unweighted")
        
        for(sample in samples){
          for(weight_option in weight_options){
            assign(paste0(factor_var, "_", weight_option, "_", sample), 
                   get(paste0("rev_young_size_superstar_regression_t_nace_", weight_option, "_", sample)))
            }
          }
      }
          
    # Compile the models we ran
    models<-list(empl_weighted_all,
                 empl_unweighted_all,
                 empl_weighted_positive,
                 empl_unweighted_positive,
                 empl_weighted_negative,
                 empl_unweighted_negative,
                 capital_weighted_all,
                 capital_unweighted_all,
                 capital_weighted_positive,
                 capital_unweighted_positive,
                 capital_weighted_negative,
                 capital_unweighted_negative)
    

    modelsummary(models, output=paste0(output_dir, "/elasticities_positive_negative_shocks_", level, ".tex"), 
                 stars = T, 
                 gof_omit = "Std.Errors|R2 Within|R2 Within Adj.|AIC|BIC|Log.Lik.|F",
                 title=paste0("Elasticities of Factor Growth to Revenue Growth"))
    
    models_weighted<-list(empl_weighted_all,
                 empl_weighted_positive,
                 empl_weighted_negative,
                 capital_weighted_all,
                 capital_weighted_positive,
                 capital_weighted_negative)
    
    
    modelsummary(models_weighted, output=paste0(output_dir, "/elasticities_positive_negative_shocks_", level, "_short.tex"), 
                 stars = T, 
                 gof_omit = "Std.Errors|R2 Within|R2 Within Adj.|AIC|BIC|Log.Lik.|F",
                 title=paste0("Elasticities of Factor Growth to Revenue Growth"))
    
                 
  }
  
  
}
            


#4) Graphs of high growth vs. high decline NACE industries ----------------------------------

library(dplyr)
library(stringr)
library(ggplot2)
library(readxl)
library(plotly)
library(data.table)

# Define input and result folder paths
inputs_folder <- "C:/Users/nb/Dropbox/Reallocation - shared folder/Inputs/" #Adjust these with the folder containing the results of part 3
results_folder <- "C:/Users/nb/Dropbox/Reallocation - shared folder/Exports/05.07 High product employment growth and decline/" #Adjust these with the folder containing the results of part 3

# Read the input Excel files
NACE_2d_list <- read_excel(paste0(inputs_folder, "NACE_2d_list.xlsx"))
NACE_list <- read_excel(paste0(inputs_folder, "NACE_list.xlsx"))

# Custom color palette with 13 distinct colors
custom_colors <- c(
  "#1f78b4", "#33a02c", "#e31a1c", "#ff7f00",
  "#6a3d9a", "#b15928", "#a6cee3", "#b2df8a",
  "#fb9a99", "#fdbf6f", "#cab2d6", "#ffff99",
  "#b2b2b2", "black"
)

# Define the parameters for iteration
filters <- c("firms", "sectors")
levels_text <- c("products", "firms", "firm-product lines")
weights <- c("unweighted", "weighted")

levels_growth_vars<-data.frame(levels=levels, growth_vars=growth_vars, high_growth_thresholds, levels_text)

# Iterate through combinations of filter, level, and weight
for (filter in filters) {
  for (level in levels) {
    for (weight in weights) {
      
      folder_temp<-paste0(results_folder, "Export_prodcom_", filter, "/")
      output_temp<-paste0(results_folder, "plots/")
      
      # Read positive and negative growth data from Excel files
      positive <- read_xlsx(paste0(folder_temp, level, "/Regression_", level, "_sector_n_products_", weight, "_positive.xlsx"))
      negative <- read_xlsx(paste0(folder_temp, level, "/Regression_", level, "_sector_n_products_", weight, "_negative.xlsx"))
      
      # Rename and select relevant columns
      positive <- positive %>% rename(share_high_growth = Estimate) %>% select(variable, share_high_growth)
      negative <- negative %>% rename(share_high_decline = Estimate) %>% select(variable, share_high_decline)
      
      # Merge positive and negative growth data
      growth_decline <- merge(positive, negative, by = "variable", all = TRUE)
      growth_decline$NACE <- as.numeric(gsub("[^0-9]", "", growth_decline$variable))
      growth_decline$NACE <- str_pad(growth_decline$NACE, width = 4, side = "left", pad = "0")
      growth_decline$NACE_2d <- substr(growth_decline$NACE, 1, 2)
      
      setDT(growth_decline)
      HHI_positive<-round(growth_decline[variable=="HHI_industry",]$share_high_growth, 2)
      HHI_negative<-round(growth_decline[variable=="HHI_industry",]$share_high_decline, 2)
      
      # Merge with NACE lists
      growth_decline <- merge(growth_decline, NACE_2d_list, by = "NACE_2d", all = TRUE)
      growth_decline <- merge(growth_decline, NACE_list, by = "NACE", all = TRUE)
      
      # Remove rows with NA values
      growth_decline <- na.omit(growth_decline)
      
      # Get weighting variables and thresholds 
      growth_var<-levels_growth_vars[levels==level,]$growth_vars
      growth_threshold<-levels_growth_vars[levels==level,]$high_growth_thresholds
      level_text<-levels_growth_vars[levels==level,]$levels_text
      
      subtitle_plot<-paste0(weight, if(weight=="weighted"){paste0(" by absolute change in ", growth_var)})
      
      
      title_plot<-paste0("Share of high growth vs. high decline ", level_text, " (", subtitle_plot, ")")
      
      # Create the scatterplot with a custom color palette
      p <- ggplot(data = growth_decline, aes(x = share_high_growth, y = share_high_decline, color = category,
                                             text = paste("Industry:", industry, "<br>",
                                                          "Share High Growth:", round(share_high_growth, 2), "<br>",
                                                          "Share High Decline:", round(share_high_decline, 2)))) +
        geom_point() +  # Draw the points first so lines don't cover them
        geom_smooth(method = "lm", se = FALSE, aes(group = category)) +  # Fit lines per category
        scale_color_manual(values = custom_colors) +
        labs(x = "Share High Growth", y = "Share High Decline", color = "Category", 
             title=title_plot,
             subtitle = subtitle_plot,
             caption=paste0("HHI coefficient in high growth regression: ", HHI_positive, ". HHI coefficient in high decline regression: ", HHI_negative)) +
        # annotate("text", x = 50, y = 80, label = "Note: This is a custom annotation", color = "grey", size = 5)+
        theme_minimal()
      
      # Print static plot to check geom_smooth
      print(p)
      
      # Add interactivity using plotly
      p_plotly <- ggplotly(p, tooltip = "text") %>%
        plotly::highlight(on = "plotly_hover", off = "plotly_doubleclick") %>%
        plotly::highlight_key(~category, ~industry)  %>% 
        plotly::layout(legend="Hello") 
      
      # Show the interactive plotly plot
      print(p_plotly)      
      
      htmlwidgets::saveWidget(p_plotly, paste0(output_temp, filter, "_", level, "_", weight, ".html"))
    }
  }
}

