"
This file uses firm (FARE/FICUS) and product data (EAP) to analyze the effect of product entry and exit on firm growth



Author: Julián Díaz-Acosta
Last update: 10/10/2024
"



# 0) setup -------------------------------------------------------------------
source("C:/Users/NEWPROD_J_DIAZ-AC/Documents/Reallocation/6 Publish/Dummy infrastructure/1 Code/Main - fake.R")
output_dir<-paste0(output_dir, "Product reallocation and firm dynamics/Export 09.11/")
output_dir_creator(output_dir)

#Bring in necessary firm and product information
firm_data_select<-readRDS("sbs_br_data_prodcom_firms.RDS") %>% filter(year>2009)
nace_DEFind <- fread("C:/Users/Public/1. Microprod/1. Microprod-H2020/NEW_INFRASTRUCTURE/Infra/MetaData/nace_DEFind.conc", colClasses = c('character'))
product_data<-readRDS(paste0("product_level_growth_", filter_indicator,  "_.RDS"))


# Check only prodcom firms are present in the firm_data_select sample
if(length(setdiff(unique(firm_data_select$firmid), unique(product_data$firmid)))!=0){
  stop("Firm data does not contain only prodcom firms. Check modules a and c and come back")
}

# 1) Create measures of products introduced and products destroyed in a year --------------------


# Check that product categories status variables are exhaustive and mutually exclusive
if(sum(product_data$first_introduction, product_data$reintroduced, 
       product_data$discontinued, product_data$incumbent, product_data$paused) != nrow(product_data)){
  stop("product categories are not exhaustive and/or mutually exlcusive")
}


# Identify firm's core products and industries at the different digits aggregation levels
product_data[, `:=`(prodcom=substr(prodfra_plus, 1,8),
                    cpa=substr(prodfra_plus, 1,6),
                    NACE_2d_pf=substr(prodfra_plus, 1,2))]

core_switch_product<-function(data, n_digits){
  
  #Set names of variables
  pf<-paste0("pf_", n_digits)
  core<-paste0("core_", pf)
  switch<-paste0("switch_", pf)
  n_core<-paste0("n_core_", pf)
  
  setDT(data)
  
  data[[pf]]<-substr(data$prodfra_plus, 1, n_digits) # Create the category variable (sector, industry, cpa, pcc8, prodfra)
  if(n_digits==1){
    data[[pf]]<-substr(data$DEFind, 1, n_digits) # Create the category variable (sector, industry, cpa, pcc8, prodfra)
  }else{
    data[[pf]]<-substr(data$prodfra_plus, 1, n_digits) # Create the category variable (sector, industry, cpa, pcc8, prodfra)
  }
  
  data<-data[, .(rev=sum(rev, na.rm = T)), by=.(firmid, year, get(pf))] # Aggregate revenue at the category level
  data<-data[, core:=(rev==max(rev, na.rm = T)), by=.(firmid, year)] # Flag the category with the highest revenue
  data<-data[core==T & rev!=0] # Keep only categories with the highest revenue and with revenue
  data<-data[, .(n_core=n_distinct(get), # Flag the number of tied categories in number 1
                 get=paste(unique(get), collapse=", ")), by=.(firmid, year)] # Define the set of top categories
  setorder(data, firmid, year)
  data<-data[, switch:=!(ifelse(is.na(dplyr::lag(get, 1)), NA,  str_detect(dplyr::lag(get, 1), get))), by=.(firmid)] # Flag if there has been a switch in category
  
  setnames(data, c("get", "switch", "n_core"), c(core, switch, n_core)) # Adjust names
  
  return(data)
}

levels_agg<-c(1, 2, 4, 6, 8, 10)

for(i in seq_along(levels_agg)){
  print(levels_agg[i])
  data<-core_switch_product(product_data, levels_agg[i])

  if(i==1){
    product_core<-data
  }else{
    if(nrow(product_core)!=nrow(data)){
      stop("Differing numbers in levels of aggregation. Check this.")
    }
    
    product_core<-merge(product_core, data, by=c("firmid", "year"), all.x = T, allow.cartesian=F)
    
  }
}

setorder(product_core, firmid, year)
cores<-names(product_core)[grepl("^core", names(product_core))]
for(i in cores){
  product_core<-product_core[, (paste0("lag_", i)):=dplyr::lag(get(i), 1), by=firmid]
}


product_data<-merge(product_data, product_core, by=c("firmid", "year"))
# rm(product_prodfra_core, product_prodcom_core, product_cpa_core, product_NACE_core, product_NACE_2d_pf_core, product_core); gc()

setorder(product_data, firmid, year, -rev)


product_data<-product_data[, `:=`(new_10=first_introduction & prodfra_plus==lag_core_pf_10)]
product_data<-product_data[, `:=`(new_8=first_introduction & prodcom==lag_core_pf_8 & new_10!=T)]
product_data<-product_data[, `:=`(new_6=first_introduction & cpa==lag_core_pf_6 & new_10!=T & new_8!=T)]
product_data<-product_data[, `:=`(new_4=first_introduction & NACE==lag_core_pf_4 & new_10!=T & new_8!=T & new_6!=T)]
product_data<-product_data[, `:=`(new_2=first_introduction & NACE_2d_pf==lag_core_pf_2 & new_10!=T & new_8!=T & new_6!=T & new_4!=T)]
product_data<-product_data[, `:=`(new_1=first_introduction & substr(DEFind,1,1)==lag_core_pf_1 & new_10!=T & new_8!=T & new_6!=T & new_4!=T & new_2!=T)]
product_data<-product_data[, `:=`(new_0=first_introduction & substr(DEFind,1,1)!=lag_core_pf_1 & new_10!=T & new_8!=T & new_6!=T & new_4!=T & new_2!=T & new_1!=T )]

product_data<-product_data[, `:=`(exit_10=discontinued & prodfra_plus==lag_core_pf_10)]
product_data<-product_data[, `:=`(exit_8=discontinued & prodcom==lag_core_pf_8 & exit_10!=T)]
product_data<-product_data[, `:=`(exit_6=discontinued & cpa==lag_core_pf_6 & exit_10!=T & exit_8!=T)]
product_data<-product_data[, `:=`(exit_4=discontinued & NACE==lag_core_pf_4 & exit_10!=T & exit_8!=T & exit_6!=T)]
product_data<-product_data[, `:=`(exit_2=discontinued & NACE_2d_pf==lag_core_pf_2 & exit_10!=T & exit_8!=T & exit_6!=T & exit_4!=T)]
product_data<-product_data[, `:=`(exit_1=discontinued & substr(DEFind,1,1)==lag_core_pf_1 & exit_10!=T & exit_8!=T & exit_6!=T & exit_4!=T & exit_2!=T)]
product_data<-product_data[, `:=`(exit_0=discontinued & substr(DEFind,1,1)!=lag_core_pf_1 & exit_10!=T & exit_8!=T & exit_6!=T & exit_4!=T & exit_2!=T & exit_1!=T )]


product_data<-product_data %>% select(firmid, year, prodfra_plus, within_firm_rev_share, new_10, new_8, new_6, new_4, new_2, new_1, new_0,
                                      switch_pf_10, switch_pf_8, switch_pf_6,switch_pf_4, switch_pf_2,  everything())
  
write_rds(product_data, paste0("product_level_growth_", filter_indicator,  "_new_core_analysis.RDS"))
write_rds(product_core, paste0("product_core.RDS"))

# 1*) Create measures of products introduced and products destroyed in a year --------------------

product_data<-readRDS(paste0("product_level_growth_", filter_indicator,  "_new_core_analysis.RDS"))

product_core<-readRDS(paste0("product_core.RDS"))


vars<-c("first_introduction", "new_10", "new_8", "new_6", "new_4", "new_2", "new_1", "new_0", 
        # "switch_pf_10", "switch_pf_8", "switch_pf_6", "switch_pf_4", "switch_pf_2",
        "exit_10", "exit_8", "exit_6", "exit_4", "exit_2", "exit_1", "exit_0", 
        "reintroduced", "discontinued", "incumbent", "paused", "rev")
unique_vars<-c("first_year", "last_year")

product_summary_sums<-product_data[, lapply(.SD, sum, na.rm=T), .SDcols=vars, by=.(firmid, year)]
product_summary_years<-product_data[, lapply(.SD, unique, na.rm=T), .SDcols=unique_vars, by=.(firmid, year)]
product_summary<-merge(product_summary_sums, product_summary_years, by=c("firmid", "year"), all.x=T)
rm(product_summary_sums, product_summary_years); gc()

product_summary<- merge(product_summary, product_core, by=c("firmid", "year"), all.x = T)

setDT(product_summary)

product_summary[, `:=`(number_of_products=first_introduction+reintroduced+incumbent)]

# product_summary<-product_summary[, number_of_years:=n_distinct(year), by=firmid]

# Set firm's first year observations to NAs, since we don't know actual product status that year (everything is spuriously first_introduction)
# We still leave the observations, because the information on number of products is still useful
new<-grep("^new", names(product_summary), value=T)
switch<-grep("^switch", names(product_summary), value=T)
product_summary<-product_summary[year==first_year,`:=`(number_of_products=first_introduction+reintroduced+incumbent,
                                                       first_introduction=NA,
                                                       paused=NA,
                                                       reintroduced=NA,
                                                       incumbent=NA,
                                                       discontinued=NA)  ]

product_summary<-product_summary[year==first_year, (new):=NA  ]
product_summary<-product_summary[year==first_year, (switch):=NA  ]


# We drop last years observations by taking off all observations that do not have revenue in that year
# since everything is spuriously discontinued and we don't actually know the product composition that year
product_summary<-product_summary[rev!=0]


product_summary[, `:=`(new_products=first_introduction,
                       paused_products=paused,
                       reintroduced_products=reintroduced,
                       destroyed_products=discontinued,
                       number_of_products=ifelse(year!=first_year, first_introduction+reintroduced+incumbent, number_of_products ))]

product_summary <- product_summary %>% group_by(firmid) %>% mutate(entry_year=ifelse(new_products!=0, year, NA),
                                                                   exit_year=ifelse(destroyed_products!=0, year, NA)) %>% 
  mutate(exit_post_entry=case_when(is.na(entry_year) ~ NA_real_,
                                    # destroyed_products!=0 ~ 1,
                                    dplyr::lead(destroyed_products, 1)!=0 ~ 1,
                                    dplyr::lead(destroyed_products, 2)!=0 ~ 1,
                                    # dplyr::lead(destroyed_products, 3)!=0 ~ 1,
                                    # dplyr::lead(destroyed_products, 4)!=0 ~ 1,
                                    TRUE ~ 0)) %>%
  mutate(entry_post_exit=case_when(is.na(exit_year) ~ NA_real_,
                                   # new_products!=0 ~ 1,
                                   dplyr::lead(new_products, 1)!=0 ~ 1,
                                   dplyr::lead(new_products, 2)!=0 ~ 1,
                                   # dplyr::lead(new_products, 3)!=0 ~ 1,
                                   # dplyr::lead(new_products, 4)!=0 ~ 1,
                                   TRUE ~ 0)) %>% 
  mutate(exit_pre_entry=case_when(is.na(entry_year) ~ NA_real_,
                                  # destroyed_products!=0 ~ 1,
                                  dplyr::lag(destroyed_products, 1)!=0 ~ 1,
                                  dplyr::lag(destroyed_products, 2)!=0 ~ 1,
                                  # dplyr::lag(destroyed_products, 3)!=0 ~ 1,
                                  # dplyr::lag(destroyed_products, 4)!=0 ~ 1,
                                  TRUE ~ 0)) %>%
  mutate(entry_pre_exit=case_when(is.na(exit_year) ~ NA_real_,
                                   # new_products!=0 ~ 1,
                                   dplyr::lag(new_products, 1)!=0 ~ 1,
                                   dplyr::lag(new_products, 2)!=0 ~ 1,
                                   # dplyr::lag(new_products, 3)!=0 ~ 1,
                                   # dplyr::lag(new_products, 4)!=0 ~ 1,
                                   TRUE ~ 0)) %>%
  mutate(exit_with_entry=case_when(is.na(entry_year) ~ NA_real_,
                                   destroyed_products!=0 ~ 1,
                                   TRUE ~ 0)) %>%
  mutate(entry_with_exit=case_when(is.na(exit_year) ~ NA_real_,
                                   new_products!=0 ~ 1,
                                   TRUE ~ 0)) %>%
  
  ungroup()

# Delete false negatives
product_summary <- product_summary %>% group_by(firmid) %>% 
  mutate(exit_post_entry=ifelse(last_year-year>2 | exit_post_entry==1, exit_post_entry, NA),
         entry_post_exit=ifelse(last_year-year>2| entry_post_exit==1, entry_post_exit, NA), # Here the inequality is not strict because we have a "dummy" last year
         exit_pre_entry=ifelse(year-first_year>=2 | exit_pre_entry==1, exit_pre_entry, NA), 
         entry_pre_exit=ifelse(year-first_year>=2| entry_pre_exit==1, entry_pre_exit, NA))

product_summary <- product_summary %>% select(firmid, year, number_of_products, new_products, destroyed_products, paused_products, reintroduced_products,
                                              exit_post_entry, exit_pre_entry, entry_post_exit, entry_pre_exit, everything())

product_summary <- product_summary %>% mutate(prod_creat=case_when( new_products>0 ~ 1, 
                                                                    is.na(new_products) ~ NA, 
                                                                    TRUE ~ 0),
                                              prod_destr=case_when( destroyed_products>0 ~ 1, 
                                                                    is.na(destroyed_products) ~ NA, 
                                                                    TRUE ~ 0))

product_summary <- product_summary %>% arrange(firmid, year) %>% group_by(firmid) %>% mutate(
  prod_creat_lag4=dplyr::lag(prod_creat, 4),
  prod_creat_lag3=dplyr::lag(prod_creat, 3),
  prod_creat_lag2=dplyr::lag(prod_creat, 2),
  prod_creat_lag1=dplyr::lag(prod_creat, 1),
  prod_creat_lead1=dplyr::lead(prod_creat, 1),
  prod_creat_lead2=dplyr::lead(prod_creat, 2),
  prod_creat_lead3=dplyr::lead(prod_creat, 3),
  prod_creat_lead4=dplyr::lead(prod_creat, 4),
  prod_destr_lag4=dplyr::lag(prod_destr, 4),
  prod_destr_lag3=dplyr::lag(prod_destr, 3),
  prod_destr_lag2=dplyr::lag(prod_destr, 2),
  prod_destr_lag1=dplyr::lag(prod_destr, 1),
  prod_destr_lead1=dplyr::lead(prod_destr, 1),
  prod_destr_lead2=dplyr::lead(prod_destr, 2),
  prod_destr_lead3=dplyr::lead(prod_destr, 3),
  prod_destr_lead4=dplyr::lead(prod_destr, 4),
  
)

saveRDS(product_summary, paste0("product_creation_destruction.RDS"))

# 2) Create measures of product diversification within years --------------------

HHI_firm<-product_data[!is.na(HHI), .(HHI=unique(HHI)), by=.(firmid, year)]
saveRDS(HHI_firm, "HHI_firm_year.RDS")

# 3) Patent data  ------------------------
# 3.1) Patent data creation-------------------
patent_data<-fread("patent_data.csv")
patent_data$firmid<-as.character(patent_data$siren)
patent_data$firmid<-str_pad(patent_data$firmid, 9, "left", "0")

patent_data<-patent_data[, .(min_publication_year=min(publication_year, na.rm=T),
                             max_publication_year=max(publication_year, na.rm=T),
                             collection=paste(collection, collapse = ", ")), by=.(application_year, title, inventor_name, firmid)] 

test_applications<- patent_data %>% group_by(firmid, application_year) %>% summarise(n_applications=n()) %>% rename(year=application_year)
test_granted <- patent_data %>% group_by(firmid, min_publication_year) %>% summarise(n_publications=n()) %>% rename(year=min_publication_year)

patent_apps_published<-merge(test_applications, test_granted, by=c("firmid", "year"), all=T)

saveRDS(patent_apps_published, "patent_apps_published.RDS")

# 3.2) Patent trend graph -------------------

patent_apps_published<-readRDS("patent_apps_published.RDS")


trends<-patent_data %>% select(application_year, application_number, collection, type, title, publication_year, publication_number, inventor_name, ipcr_list)
trends<-unique(trends)

publications_by_year<- trends %>%  select(publication_year, publication_number, collection)
publications_by_year<- unique(publications_by_year)
publications_by_year<- publications_by_year %>% group_by(publication_year, collection) %>% summarise(n_publications=n()) %>% 
  rename(year=publication_year) %>% pivot_wider(id_cols = "year", names_from = "collection", values_from = "n_publications", names_prefix = "pubs_")


ggplot(publications_by_year %>% filter(year %in% 1995:2022), aes(x=year)) + 
  geom_line(aes(y=pubs_FR, color="FR")) + 
  geom_line(aes(y=pubs_WO, color="WO")) +
  geom_line(aes(y=pubs_EP, color="EP")) + 
  scale_x_continuous(breaks = (seq(1995, 2022, by=1)))+
  labs(title="Patent Grants for French Firms by Geographic Scope", y="Count", x="Year", color="Type") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle=45, hjust=1))
ggsave(paste0(output_dir, "patents_trends.png"), width=6, height=4, dpi=300)




# 1**) ----------------------

product_data<-readRDS(paste0("product_level_growth_", filter_indicator,  "_new_core_analysis.RDS"))
product_summary<-readRDS(paste0("product_creation_destruction.RDS"))
product_core<-readRDS(paste0("product_core.RDS"))

patent_apps_published<-readRDS("patent_apps_published.RDS")

product_summary<-merge(product_summary, patent_apps_published, by=c("firmid", "year"), all.x = T)
# product_summary[, n_publications:=ifelse(is.na(n_publications),0, n_publications)]

rev_limit<-0
threshold_firms_per_category<-50
n_bins<-50
# innovation_vars<-c("first_introduction", "new_10", "new_8", "new_6", "new_4", "new_2", "new_1", "new_0",
#                    "discontinued", "exit_10", "exit_8", "exit_6", "exit_4", "exit_2", "exit_1", "exit_0")
#                            # "switch_pf_10", "switch_pf_8", "switch_pf_6", "switch_pf_4", "switch_pf_2")

digits<-c(0, 1, 2, 4, 6, 8, 10)
innovation_vars<-c("all", digits)
labels_codes<-c("No Common Root", "1 Letter", "2 Digits", "4 Digits", "6 Digits", "8 Digits", "10 Digits")

category_vars<-c("DEFind", "NACE_2d", "NACE", "cpa", "prodcom", "prodfra_plus")

product_data<-product_data[year!=first_year & year!=last_year]

output_dir_og<-output_dir

for(j in category_vars){
  
  all_results<-data.table(bin=NA, weighted_avg_in_rate=NA, weighted_avg_ex_rate=NA,  innovation_var=NA)
  
  
  for(i in innovation_vars){
    # i<-"first_introduction"
    # j<-"cpa"
    
    if(!dir.exists(paste0(output_dir_og, j, "/"))){
      output_dir<-paste0(output_dir_og, j, "/")
      output_dir_creator(output_dir_og)
    }else{
      output_dir<-paste0(output_dir_og, j, "/")
    }
    
    
    if(i =="all"){
      new_var<-"first_introduction"
      exit_var<-"discontinued"
    }else{
      new_var<-paste0("new_", i)
      exit_var<-paste0("exit_", i)
    }
    
    # Argente et al.: "For each firm x product catergory, we compute average sales, the average product
    # innovation rate (new products/number of products sold)... 
    test<-product_data[, .(n_new_products=n_distinct(prodfra_plus[get(new_var) & year!=first_year & rev>rev_limit], na.rm = T),
                           new_codes=paste(unique(prodfra_plus[get(new_var) & year!=first_year & rev>rev_limit], na.rm = T), collapse=", "),
                           n_exit_products=n_distinct(prodfra_plus[get(exit_var) & year!=last_year], na.rm = T),
                           exit_codes=paste(unique(prodfra_plus[get(exit_var) & year!=last_year], na.rm = T), collapse=", "),
                           n_products=n_distinct(prodfra_plus),
                           codes=paste(unique(prodfra_plus, na.rm = T), collapse=", "),
                           rev=mean(rev, na.rm=T)), by=.(firmid, (get(j)))]
    
    names(test)[names(test)=="get"]<-j
    
    test<-test[, `:=`(in_rate=n_new_products/n_products,
                      ex_rate=n_exit_products/n_products) ]
  
    test<-test[, `:=`(n_firms_in_cpa=.N), by=.(get(j)) ]
    
    test<-test[n_firms_in_cpa>threshold_firms_per_category]
    
    # Argente et al.: Whithin each product category, we assign firms to 50 bins of average sales ...
    # and plot the average product innovation rate for each bin.
    restult<-test[, `:=`(bin=frank(rev, ties.method = "first") %/% (max(frank(rev, ties.method = "first"))/n_bins) +1), by=get(j)]
    
    
    # restult<-test[, `:=`(bin=cut(rev, breaks=quantile(rev, probs=seq(0,1, length.out=51), na.rm=T), include.lowest=T, labels=F),
    #                      total_rev=sum(rev, na.rm=T)), by=cpa]
    
    # Argente et al.: Each dot plots the averages after weighting each product category by its 
    # importance in the whole sector, as measured by the share of sales accounted for by the category
    
    restult<-restult[, `:=`(total_rev=sum(rev, na.rm=T))]
    restult<-restult[, `:=`(rev_share=rev/total_rev)]
    
    restult<-restult[, .(weighted_avg_in_rate=sum(in_rate*rev_share, na.rm=T)/sum(rev_share, na.rm = T),
                         weighted_avg_ex_rate=sum(ex_rate*rev_share, na.rm=T)/sum(rev_share, na.rm = T)), by=.(bin)]
    
    restult$innovation_var<-i
    restult[, bin:=(bin*(100/n_bins))]
  
    
    all_results<-rbind(all_results, restult)
    
    ggplot(restult, aes(x=bin, y=weighted_avg_in_rate)) + 
      geom_point() +
      labs(x=paste0("Firm-", j, " Size Percentile"), y="Weighted Average Product Entry Rate") +
      theme_minimal() + 
      scale_x_continuous(breaks=seq(0,100, by=25)) + 
      ylim(0, 0.4)+
      ggtitle(paste0("Product Entry Rates by Firm-", j, " Size "),
              subtitle=paste0("Product Entry Variable: ", new_var))
    ggsave(paste0(output_dir, new_var, "_entry_size.png"))
    
    ggplot(restult, aes(x=bin, y=weighted_avg_ex_rate)) + 
      geom_point() +
      labs(x=paste0("Firm-", j, " Size Percentile"), y="Weighted Average Product Exit Rate") +
      theme_minimal() + 
      scale_x_continuous(breaks=seq(0,100, by=25)) + 
      ylim(0, 0.4)+
      ggtitle(paste0("Product Exit Rates by Firm-", j, " Size "),
              subtitle=paste0("Product Exit Variable: ", exit_var))
    ggsave(paste0(output_dir, exit_var, "_exit_size.png"))
    
    
  }
  
  #Checks
  all_results<-all_results[!is.na(bin)]
  all_results<-unique(all_results)
  all_results_wide<-pivot_wider(all_results, names_from = innovation_var, values_from = c(weighted_avg_in_rate, weighted_avg_ex_rate))
  setDT(all_results_wide)
  all_results_wide[, all_in:= rowSums(.SD, na.rm=T), .SDcols = (grep(c("in_rate_[0-9]"), names(all_results_wide)))]
  all_results_wide[, all_ex:= rowSums(.SD, na.rm=T), .SDcols = (grep(c("ex_rate_[0-9]"), names(all_results_wide)))]
  all_results_wide[, check_in:=(abs(all_in-weighted_avg_in_rate_all)<0.0000000000000001) ]
  all_results_wide[, check_ex:=(abs(all_ex-weighted_avg_ex_rate_all)<0.0000000000000001) ]
  
  
  all_results<- all_results[innovation_var %in% digits,]
  all_results$innovation_var<-factor(all_results$innovation_var, levels=digits)
  
  
  ggplot(all_results, aes(x=bin, y=weighted_avg_in_rate, fill=innovation_var)) + 
    geom_bar(stat = "identity") + 
    scale_fill_discrete(labels=labels_codes, name="Common Root with Core")+
    ylim(0, 0.4)+
    ggtitle(paste0("Product Entry Rate by Firm-",j, " Size "))+
    labs(x=paste0("Size Percentile"), y="Weighted Average Product Entry Rate") +
    theme_minimal()
  ggsave(paste0(output_dir, "entry_size.png"))
  
  
  ggplot(all_results, aes(x=bin, y=weighted_avg_ex_rate, fill=innovation_var)) + 
    geom_bar(stat = "identity") + 
    scale_fill_discrete(labels=labels_codes, name="Common Root with Core")+
    ylim(0, 0.4)+
    ggtitle(paste0("Product Exit Rate by Firm-",j, " Size "))+
    labs(x=paste0("Size Percentile"), y="Weighted Average Product Entry Rate") +
    theme_minimal()
  ggsave(paste0(output_dir, "exit_size.png"))
  
  
  all_results<-all_results[, `:=`(norm_weighted_avg_ex_rate=weighted_avg_ex_rate/sum(weighted_avg_ex_rate, na.rm=T)), by=bin]
  all_results<-all_results[, `:=`(norm_weighted_avg_in_rate=weighted_avg_in_rate/sum(weighted_avg_in_rate, na.rm=T)), by=bin]
  
  
  ggplot(all_results, aes(x=bin, y=norm_weighted_avg_ex_rate, fill=innovation_var)) + 
    geom_bar(stat = "identity") + 
    scale_fill_discrete(labels=labels_codes, name="Common Root with Core")+
    ggtitle(paste0("Share of Product Types in Exit Rates by Firm-",j, " Size "))+
    labs(x=paste0("Size Percentile"), y="Weighted Average Product Entry Rate") +
    theme_minimal()
  ggsave(paste0(output_dir, "share_entry_size.png"))
  
  
  
  ggplot(all_results, aes(x=bin, y=norm_weighted_avg_in_rate, fill=innovation_var)) + 
    geom_bar(stat = "identity") + 
    scale_fill_discrete(labels=labels_codes, name="Common Root with Core")+
    ggtitle(paste0("Share of Product Types in Entry Rates by Firm-",j, " Size "))+
    labs(x=paste0("Size Percentile"), y="Weighted Average Product Entry Rate") +
    theme_minimal()
  ggsave(paste0(output_dir, "share_exit_size.png"))
  
}

# product_data[, new:=rowSums(.SD, na.rm=T), .SDcols = (grep("^new_", names(all_results_wide)))]
product_data[, new:=new_0+new_1+new_2+new_4+new_6+new_8+new_10]
product_data[, exit:=exit_0+exit_1+exit_2+exit_4+exit_6+exit_8+exit_10]

test<-product_data %>% select(firmid, year, prodfra_plus, first_introduction, grep("^new", names(product_data)), 
                              discontinued, grep("^exit", names(product_data))) %>% 
  mutate(first_introduction=as.numeric(first_introduction, na.rm=T),
         discontinued=as.numeric(discontinued, na.rm=T),
         check_in=first_introduction==new,
         check_ex=discontinued==exit)
test<-test %>% filter(is.na(exit) | is.na(new))
if(nrow(test)>0){
  stop("New and exit categories do not comprise the number of first_introduction or discontinued. Check this")
}
rm(test); gc()



setDT(product_summary)
test_patent<-product_summary[, .(new_products=sum(new_products, na.rm=T),
                                 patents=sum(n_publications, na.rm=T),
                                 rev=sum(rev, na.rm=T)), by=.(firmid)]
test_patent<-test_patent[, `:=`(total_rev=sum(rev, na.rm=T))]
test_patent<-test_patent[, `:=`(rev_share=rev/total_rev,
                                pat_per_prod=ifelse(new_products==0, NA_real_, patents/new_products))]

test_patent<-test_patent[, `:=`(bin=frank(rev, ties.method = "first") %/% (max(frank(rev, ties.method = "first"))/n_bins) +1)]
test_patent<-test_patent[, .(weighted_avg_pat_per_prod=sum(pat_per_prod*rev_share, na.rm=T)/sum(rev_share, na.rm = T)), by=.(bin)]

ggplot(test_patent, aes(x=bin, y=weighted_avg_pat_per_prod)) + 
  geom_point() +
  labs(x=paste0("Firm Size Percentile"), y="Weighted Average Sahre of Patents Per New Products") +
  theme_minimal() + 
  scale_x_continuous(breaks=seq(0,100, by=25)) + 
  ggtitle(paste0("Product Entry Rates by Firm Size "))









table(substr(product_data[new_0==T,]$DEFind, 1,1), product_data[new_0==T,]$core_pf_1)

abcd<-as.data.table(table(substr(product_data[new_0==T,]$DEFind, 1,1), product_data[new_0==T,]$core_pf_1))








# 4) Merge measures of products introduced and products destroyed in a year to br data ------------------------

product_summary<-readRDS("product_creation_destruction.RDS")
HHI_firm<-readRDS("HHI_firm_year.RDS")
patent_apps_published<-readRDS("patent_apps_published.RDS")
cis_data<-readRDS("cis_data.RDS") %>% select(-NACE_BR)

firmids_in_firm_data_select_og<-unique(firm_data_select$firmid)


firm_data_select <- merge(firm_data_select, product_summary, by=c("firmid", "year"), all.x = T)
firm_data_select <- merge(firm_data_select, HHI_firm, by=c("firmid", "year"), all.x = T)
firm_data_select <- merge(firm_data_select, patent_apps_published, by=c("firmid", "year"), all = T)
firm_data_select <- merge(firm_data_select, cis_data, by=c("firmid", "year"), all.x = T)

setDT(firm_data_select)

# Additional measures of patenting
firm_data_select[, year_d_pat:=ifelse(is.na(n_publications), NA, year)]
firm_data_select[, year_d_pat:=min(year_d_pat, na.rm = T), by=firmid]
firm_data_select[, d_pat:=ifelse(!is.na(year_d_pat) & year>=year_d_pat, 1,0)]

setorder(firm_data_select, firmid, year)

firm_data_select <- firm_data_select %>% group_by(firmid) %>%   
  mutate(patent_window=case_when(!is.na(n_publications) & n_publications!=0 ~ 1,
                                 !is.na(dplyr::lag(n_publications, 1)) & (year-1)==(dplyr::lag(year, 1)) & dplyr::lag(n_publications, 1)!=0 ~ 1,
                                 !is.na(dplyr::lag(n_publications, 2)) & (year-2)==(dplyr::lag(year, 2)) & dplyr::lag(n_publications, 2)!=0 ~ 1,
                                 !is.na(dplyr::lead(n_publications, 1)) & (year+1)==(dplyr::lead(year, 1)) & dplyr::lead(n_publications, 1)!=0 ~ 1,
                                 !is.na(dplyr::lead(n_publications, 2)) & (year+2)==(dplyr::lead(year, 2)) & dplyr::lead(n_publications, 2)!=0 ~ 1,
                                 TRUE ~ 0))

firm_data_select<-firm_data_select %>% filter(year>2009)
setDT(firm_data_select)
firm_data_select<-firm_data_select[firmid %in% unique(product_data$firmid)]



firm_data_select[, IHI:=(1-HHI)]

firm_data_select_og<-firm_data_select


firm_data_select_og<- firm_data_select_og %>% filter(size!="micro" & size!="extra large" & !is.na(size))

firm_data_select_og$size<-factor(firm_data_select_og$size, levels=c("small", "medium", "large"))

# Measures and outliers in labor productivity
firm_data_select_og[, labor_prod:=ifelse(empl>0, nq/empl, NA)]
firm_data_select_og[, k_to_l:=ifelse(empl>0, capital/empl, NA)]

  

summary_labor_productivity<-firm_data_select_og %>% arrange(desc(labor_prod)) %>%
  group_by(size) %>% summarise(mean=mean(labor_prod, na.rm=T), 
                               median=median(labor_prod, na.rm=T),
                               p75=quantile(labor_prod, 1-0.25, na.rm = T),
                               p90=quantile(labor_prod, 1-0.1, na.rm = T),
                               p99=quantile(labor_prod, 1-0.01, na.rm = T),
                               p99_9=quantile(labor_prod, 1-0.001, na.rm = T),
                               p99_99=quantile(labor_prod, 1-0.0001, na.rm = T)
  )


summary_labor_productivity<-xtable(summary_labor_productivity)
print.xtable(summary_labor_productivity, file=paste0(output_dir, "summary_stats_labor_productivity", type="latex", include.rownames=F))

table(firm_data_select_og$year)
  
make_summary_stats(firm_data_select_og, c("empl", "nq", "capital", "firm_age",
                                          "empl_growth", "nq_growth", "capital_growth", 
                                          "t_l", "t_k", "t_m",
                                          "labor_prod", 
                                          "superstar", "young"),
                   "size", paste0("firm_data_select_summary_stats_full_sample"))

make_summary_stats(firm_data_select_og, c("number_of_products", "new_products", "destroyed_products", "paused_products", "reintroduced_products",
                                          "entry_post_exit", "entry_pre_exit", "entry_with_exit",
                                          "exit_post_entry", "exit_pre_entry", "exit_with_entry",
                                          "n_applications", "n_publications",
                                          "HHI"),
                   "size", paste0("product_creation_destruction_summary_stats_full_sample"))

write_rds(firm_data_select_og, paste0("firm_data_select_og.rds"))

# test<-firm_data_select_og[, c("firmid", "year", "nq_bar", "empl_bar")]
# Diversification regressions ----------------------------

firm_data_select_og[, `:=`(log_n_products=log(number_of_products),
                           log_new_products=log(new_products))]

diversification_vars<-c("IHI", "log_n_products", "log_new_products")

# table(firm_data_select_og$number_of_products)
# table(firm_data_select_og$new_products)
# 
# 
# test<-firm_data_select_og %>% filter(number_of_products>=23) %>% select(firmid, year, number_of_products, everything())
# 
# View(product_data[firmid=="302305529"] %>% select(firmid, year, NACE_BR, prodfra_plus, rev, rev_l, first_introduction, reintroduced, discontinued, incumbent, everything()))

for(divf_var in diversification_vars){
  
  f_baseline_size<-as.formula(paste0("nq_growth ~ size | NACE_BR^year"))
  f_baseline_divf_var<-as.formula(paste0("nq_growth ~ ", divf_var, " | NACE_BR^year"))
  f_baseline_divf_var_sq<-as.formula(paste0("nq_growth ~ ", divf_var, " + ", divf_var, "^2 | NACE_BR^year"))
  f_size_divf_var<-as.formula(paste0("nq_growth ~ size + ", divf_var, " | NACE_BR^year"))
  f_size_divf_var_sq<-as.formula(paste0("nq_growth ~ size + ", divf_var, " + ", divf_var, "^2 | NACE_BR^year"))
  f_size_divf_var_interaction<-as.formula(paste0("nq_growth ~ size*", divf_var, " | NACE_BR^year"))
  f_size_divf_var_sq_interaction<-as.formula(paste0("nq_growth ~ size*", divf_var, " + size*", divf_var, "^2 | NACE_BR^year"))
  
  formulas<-c("f_baseline_size", "f_baseline_divf_var", "f_baseline_divf_var_sq", 
              "f_size_divf_var", "f_size_divf_var_sq", 
              "f_size_divf_var_interaction", "f_size_divf_var_sq_interaction")
  
  for(i in 1:2){
    
    if(i==1){
      data<-firm_data_select_og
      weight<-data$nq_bar
      label<-"all_firms"
    }else{
      data<-firm_data_select_og[number_of_products>1]
      weight<-data$nq_bar
      label<-"multiproduct_obs"
    }
    
    for (formula in formulas){
      diversification_reg_unweighted<-feols(get(formula), data)
      diversification_reg_weighted<-feols(get(formula), data, weights=weight)
      
      assign(paste0("reg_", formula, "_unweighted"), diversification_reg_unweighted)
      assign(paste0("reg_", formula, "_weighted"), diversification_reg_weighted)
      
    }
    
    models_weighted<-list(reg_f_baseline_size_weighted,
                 reg_f_baseline_divf_var_weighted,
                 reg_f_baseline_divf_var_sq_weighted,
                 reg_f_size_divf_var_weighted,
                 reg_f_size_divf_var_sq_weighted,
                 reg_f_size_divf_var_interaction_weighted,
                 reg_f_size_divf_var_sq_interaction_weighted)
    
    modelsummary(models_weighted, output=paste0(output_dir, "diversification_", divf_var, "_weighted_", label, ".tex"), 
                 stars = T, 
                 gof_omit = "Std.Errors|R2 Within|R2 Within Adj.|AIC|BIC|Log.Lik.",
                 title=paste0("Diversification and Revenue Growth. Weighted Regressions. Diversification Measure: ", divf_var))
    
    
    models_unweighted<-list(reg_f_baseline_size_unweighted,
                          reg_f_baseline_divf_var_unweighted,
                          reg_f_baseline_divf_var_sq_unweighted,
                          reg_f_size_divf_var_unweighted,
                          reg_f_size_divf_var_sq_unweighted,
                          reg_f_size_divf_var_interaction_unweighted,
                          reg_f_size_divf_var_sq_interaction_unweighted)
    
    modelsummary(models_weighted, output=paste0(output_dir, "diversification_", divf_var, "_unweighted_", label, ".tex"), 
                 stars = T, 
                 gof_omit = "Std.Errors|R2 Within|R2 Within Adj.|AIC|BIC|Log.Lik.",
                 title=paste0("Diversification and Revenue Growth. Unweighted Regressions. Diversification Measure: ", divf_var))
    
    models<-list(reg_f_baseline_size_weighted,
                 reg_f_baseline_divf_var_weighted,
                 reg_f_baseline_divf_var_sq_weighted,
                 reg_f_size_divf_var_weighted,
                 reg_f_size_divf_var_sq_weighted,
                 reg_f_size_divf_var_interaction_weighted,
                 reg_f_size_divf_var_sq_interaction_weighted,
                 reg_f_baseline_size_unweighted,
                 reg_f_baseline_divf_var_unweighted,
                 reg_f_baseline_divf_var_sq_unweighted,
                 reg_f_size_divf_var_unweighted,
                 reg_f_size_divf_var_sq_unweighted,
                 reg_f_size_divf_var_interaction_unweighted,
                 reg_f_size_divf_var_sq_interaction_unweighted)
    
    modelsummary(models, output=paste0(output_dir, "diversification_", divf_var, "_", label, ".tex"), 
                 stars = T, 
                 gof_omit = "Std.Errors|R2 Within|R2 Within Adj.|AIC|BIC|Log.Lik.",
                 title=paste0("Diversification and Revenue Growth. Diversification Measure: ", divf_var))
    
    
  }
}




## Effect of product introduction/destruction on firm growth ----------------------

firm_data_select_og<-readRDS(paste0("firm_data_select_og.rds"))


results<-data.frame(
  k=numeric(),
  point_estimate_creat=numeric(),
  conf_low_creat=numeric(),
  conf_high_creat=numeric(),
  point_estimate_destr=numeric(),
  conf_low_destr=numeric(),
  conf_high_destr=numeric(),
  
  point_estimate_creat_interact=numeric(),
  conf_low_creat_interact=numeric(),
  conf_high_creat_interact=numeric(),
  point_estimate_destr_interact=numeric(),
  conf_low_destr_interact=numeric(),
  conf_high_destr_interact=numeric(),
  
  point_estimate_interact=numeric(),
  conf_low_interact=numeric(),
  conf_high_interact=numeric(),

  
  factor=character(),
  n_obs=numeric()
)

# firm_data_select<-firm_data_select %>% filter(firm_age<=5)

# categories<-list("young", c("small", "medium", "large", "extra large"), "superstar")


regression_reallocation_growth<-function(data, weights, category, filter, interaction=NULL, firm_fe=T){
  creat_measure<-paste0("prod_creat", lead_or_lag)
  destr_measure<-paste0("prod_destr", lead_or_lag)
  
  if(!is.null(interaction)){
    formula_growth_p_reall<-paste0(var_growth, " ~ ", creat_measure,"*", interaction, " + ", destr_measure,"*", interaction)
  }else{
    formula_growth_p_reall<-paste0(var_growth, " ~ ", creat_measure, " + ", destr_measure)
  }

  if(firm_fe){
    f_var_growth_p_reall<-as.formula(paste0(formula_growth_p_reall,  "| firmid+ NACE_BR^year"))
  }else{
    f_var_growth_p_reall<-as.formula(paste0(formula_growth_p_reall,  "| NACE_BR^year"))
  }

  print(f_var_growth_p_reall)
  
  regression<-tryCatch(feols(f_var_growth_p_reall, data=data, weights = weights), error=function(e) NA)
  assign(paste0("regression_", var, "_growth_p_reall"), regression)
  print(get(paste0("regression_", var, "_growth_p_reall")))
  
  
  conf1<-tryCatch(confint(regression, parm=creat_measure), error=function(e) data.table(NA, NA))
  point_estimate1<-tryCatch(coef(regression)[creat_measure], error=function(e) NA)
  conf2<-tryCatch(confint(regression, parm=destr_measure), error=function(e) data.table(NA, NA))
  point_estimate2<-tryCatch(coef(regression)[destr_measure], error=function(e) NA)
  
  if(!is.null(interaction)){
    
    conf3<-tryCatch(confint(regression, parm=paste0(creat_measure, ":", interaction)), error=function(e) data.table(NA, NA))
    point_estimate3<-tryCatch(coef(regression)[paste0(creat_measure, ":", interaction)], error=function(e) NA)
    
    conf4<-tryCatch(confint(regression, parm=paste0(destr_measure, ":", interaction)), error=function(e) data.table(NA, NA))
    point_estimate4<-tryCatch(coef(regression)[paste0(destr_measure, ":", interaction)], error=function(e) NA)
    
    conf5<-tryCatch(confint(regression, parm=interaction), error=function(e) data.table(NA, NA))
    point_estimate5<-tryCatch(coef(regression)[interaction], error=function(e) NA)
    
  }
  
  results_temp<-data.table(
    k=k,
    point_estimate_creat=point_estimate1,
    conf_low_creat=conf1[1,1],
    conf_high_creat=conf1[1,2],
    point_estimate_destr=point_estimate2,
    conf_low_destr=conf2[1,1],
    conf_high_destr=conf2[1,2],
    factor=var,
    variable=category,
    filter=filter)
  
  
  if(!is.null(interaction)){
    interactions_table<-data.table(
      point_estimate_creat_interact=point_estimate3,
      conf_low_creat_interact=as.numeric(conf3[1,1]),
      conf_high_creat_interact=as.numeric(conf3[1,2]),
      point_estimate_destr_interact=point_estimate4,
      conf_low_destr_interact=conf4[1,1],
      conf_high_destr_interact=conf4[1,2],
      point_estimate_interact=point_estimate5,
      conf_low_interact=conf5[1,1],
      conf_high_interact=conf5[1,1]
    )
    
    results_temp<-cbind(results_temp, interactions_table)
  }
  
  results<-rbind(results, results_temp, fill=T)
  
  
  
  return(results)
}


firm_data_select_og[, size_alt:=ifelse(size=="medium" | size=="large", "large", "small")]
firm_data_select_og[, size_age:=ifelse(young, paste0(size_alt, "_young"), paste0(size_alt, "_established"))]
firm_data_select_og[, young_alt:=ifelse(firm_age>=10, F, T)]
firm_data_select_og[, size_age_alt:=ifelse(young_alt, paste0(size_alt, "_young"), paste0(size_alt, "_established"))]

firm_data_select_og[, size_young:=ifelse(young, paste0(size, "_young"), NA)]
firm_data_select_og[, size_established:=ifelse(young, NA, paste0(size, "_established"))]


firm_data_select_og[, size_young_alt:=ifelse(young_alt, paste0(size, "_young"), NA)]
firm_data_select_og[, size_established_alt:=ifelse(young_alt, NA, paste0(size, "_established"))]



baseline_vars<-c("empl", "capital", "nq")
cost_share_vars<-c("t_l", "t_m", "t_k")

vars<-c(baseline_vars, cost_share_vars)


variables<-list("young", "size", "size_age",  "size_young", "size_established",
                "young_alt", "size_age_alt", "size_young_alt", "size_established_alt")


for(variable in variables){
  # variable<-"young"
  
  filters=unique(firm_data_select_og[[variable]])
  filters<-filters[!is.na(filters)]
  
  if(!is.logical(filter)){
    filters<-filters[filters!="extra large" & filters!="micro"]
  }
  
  firm_data_select_og$share<-NA
  
  for(filter in filters){
    firm_data_select<-firm_data_select_og[get(variable)==filter]
    n_filter=nrow(firm_data_select)
    firm_data_select_og[, share:=ifelse(get(variable)==filter, n_filter/.N, share)]
    # stop("Here")
  }
  
  
  
  make_summary_stats(firm_data_select_og, c(#"empl", "nq", "capital",
                                            "share",
                                            "firm_age",
                                            "nq", "empl",
                                            "empl_growth", "nq_growth", "capital_growth", 
                                            "t_l", "t_k", "t_m"),
                                            #"labor_prod", 
                                            #"superstar", "young"),
                     variable, paste0(variable, "_firm_data_select_summary_stats_full_sample"))
  
}


for(var in vars){
  # var<-"empl"
  print(var)
  
  if(var %in% baseline_vars){
    var_growth<-paste0(var, "_growth")
    print(var_growth)
  }else{
    var_growth<-var
  }
  

  for(k in -4:4){
    # k<--1
    if(k<0){
      lead_or_lag<-paste0("_lag", abs(k))
    }else{
      if(k==0){
        lead_or_lag<-paste0("")
      }else{
        if(k>0){
          lead_or_lag<-paste0("_lead", abs(k))
        }
      }
    }
  
  
  weights<-firm_data_select_og[[paste0(var, "_bar")]]
  results<-regression_reallocation_growth(firm_data_select_og, weights = weights, "all", "all")
  
  for(variable in variables){
    
    filters=unique(firm_data_select_og[[variable]])
    filters<-filters[!is.na(filters)]
    
    if(!is.logical(filter)){
      filters<-filters[filters!="extra large" & filters!="micro"]
    }

    
    for(filter in filters){
      firm_data_select<-firm_data_select_og[get(variable)==filter]
      
      weights<-firm_data_select[[paste0(var, "_bar")]]
      # stop("Here")
      results<-regression_reallocation_growth(data=firm_data_select, weights = weights, category=variable, filter=filter, firm_fe=T)
    }
    
  }
  
  }

}

results<-results %>% filter(factor!=TRUE)

for (z in variables) {
  
  results_temp<- results %>% filter(variable==z) %>% filter(factor %in% baseline_vars)
  
  
  y_lim_baseline=c(min(0, results_temp$conf_low_creat, results_temp$conf_low_destr, na.rm = T),
                   max(results_temp$conf_high_creat, results_temp$conf_high_destr, na.rm = T))
  
  results_temp<- results %>% filter(variable==z) %>% filter(factor %in% cost_share_vars)
  
  y_lim_cost_share=c(min(0, results_temp$conf_low_creat, results_temp$conf_low_destr, na.rm = T),
                   max(results_temp$conf_high_creat, results_temp$conf_high_destr, na.rm = T))
  
  
for(var in vars){
  
  if(var %in% baseline_vars){
    y_lim<-y_lim_baseline
  }else{
    y_lim<-y_lim_cost_share
  }
  
  results_temp<- results %>% filter(factor==var)
  results_temp<- results_temp %>% filter(variable==z | variable=="all")
  
  
  temp_creation<-ggplot(results_temp, aes(x=k, y=point_estimate_creat, color=filter, fill=filter)) +
    geom_point() +
    geom_ribbon(aes(ymin=conf_low_creat, ymax=conf_high_creat), colour=NA, alpha=0.2) +
    geom_line()+
    geom_hline(yintercept = 0, color="darkred", linetype="solid", size=1)+
    geom_vline(xintercept = 0, color="black", linetype="dashed") + 
    scale_x_continuous(breaks=seq(-4, 4, by=1))+
    coord_cartesian(ylim=y_lim)+
    theme_minimal() +
    theme(plot.title = element_text(hjust=0.5))+
    labs(title=paste0(if (var=="nq") "revenue" else var),
         fill=z,
         color=z,
         x="k",
         y="Relation to Product Introduction")
  # ggsave(paste0(output_dir, "prod_creation_", var, "_growth_effects_", z, ".png"), height = 4, width = 6)
  
  
  
  
  
  temp_destruction<-ggplot(results_temp , aes(x=k, y=point_estimate_destr, color=filter, fill=filter)) +
    geom_point() +
    geom_ribbon(aes(ymin=conf_low_destr, ymax=conf_high_destr), colour=NA, alpha=0.2) +
    geom_line()+
    geom_hline(yintercept = 0, color="darkred", linetype="solid", size=1)+
    geom_vline(xintercept = 0, color="black", linetype="dashed") + 
    scale_x_continuous(breaks=seq(-4, 4, by=1))+
    coord_cartesian(ylim=y_lim)+
    theme_minimal() +
    theme(plot.title = element_text(hjust=0.5))+
    labs(x="k",
         fill=z,
         color=z,
         y="Relation to Product Exit")
  # ggsave(paste0(output_dir, "prod_destruction_", var, "_growth_effects_", z, ".png"), height = 4, width = 6)
  
  
  assign(paste0(var, "_", z, "_creation"), temp_creation)
  assign(paste0(var, "_", z, "_destruction"), temp_destruction)
}
  
  final_plot<-((get(paste0("empl_", z, "_creation"))  +  theme(axis.title.x = element_blank(), axis.ticks.x = element_blank()))+
                 (get(paste0("nq_", z, "_creation")) + theme(axis.title.y=element_blank(), axis.title.x = element_blank(), axis.ticks.x = element_blank())) + 
                 (get(paste0("capital_", z, "_creation"))  + theme(axis.title.y=element_blank(), axis.title.x = element_blank(), axis.ticks.x = element_blank()))) / 
    (get(paste0("empl_", z, "_destruction")) +  
       (get(paste0("nq_", z, "_destruction")) + theme(axis.title.y=element_blank())) + 
       (get(paste0("capital_", z, "_destruction")) + theme(axis.title.y=element_blank())))+
    plot_layout(guides="collect")+
    theme(legend.position = "right")
  ggsave(paste0(output_dir, "effect of creation and destruction by ", z, " empl nq capital.png"), height = 8, width=11)
  
  
  final_plot<-((get(paste0("t_l_", z, "_creation"))  +  theme(axis.title.x = element_blank(), axis.ticks.x = element_blank()))+
                 (get(paste0("t_m_", z, "_creation")) + theme(axis.title.y=element_blank(), axis.title.x = element_blank(), axis.ticks.x = element_blank())) + 
                 (get(paste0("t_k_", z, "_creation"))  + theme(axis.title.y=element_blank(), axis.title.x = element_blank(), axis.ticks.x = element_blank()))) / 
    (get(paste0("t_l_", z, "_destruction")) +  
       (get(paste0("t_m_", z, "_destruction")) + theme(axis.title.y=element_blank())) + 
       (get(paste0("t_k_", z, "_destruction")) + theme(axis.title.y=element_blank())))+
    plot_layout(guides="collect")+
    theme(legend.position = "right")
  ggsave(paste0(output_dir, "effect of creation and destruction by ", z, " t_l t_m t_k.png"), height = 8, width=11)
  
  
}

## Product determinants of high growth young firms ----------------------

firm_data_select_og<-readRDS(paste0("firm_data_select_og.rds"))

threshold_young<-10
threshold_growth<-0.2

setDT(firm_data_select_og)
firm_data_select_og<-firm_data_select_og[firm_age<=threshold_young & abs(empl_growth)!=2]
firm_data_select_og[, top_growth:=fifelse(frank(empl_growth, ties.method="min")/.N>(1-threshold_growth) & 
                                            frank(nq_growth, ties.method="min")/.N>(1-threshold_growth),1,0), by=year]
names(firm_data_select_og)

formula<-"top_growth ~ new_0 +  new_1 + new_2 + new_4 + new_6 + new_8 + exit_0 + exit_1 + exit_2 + exit_4 + exit_6 + exit_8 + exit_10 + patent_window"
formula_baseline<-as.formula(formula)
formula_NACE_fe<-as.formula(paste0(formula, "| NACE_BR^year"))
formula_firmid_fe<-as.formula(paste0(formula, "| firmid"))
formula_firmid_NACE_fe<-as.formula(paste0(formula, "| firmid + NACE_BR^year"))

feols(formula_firmid_NACE_fe, data=firm_data_select_og)

## Effect of product introduction/destruction on firm growth ----------------------

firms_patenting <- firm_data_select[, .(d_pat=sum(d_pat, na.rm = T)), by=firmid]
firms_patenting <- firms_patenting[d_pat>=1,]
firms_patenting <- unique(firms_patenting$firmid)

firms_patenting <- firm_data_select[firmid %in% firms_patenting, c("new_products", "firmid", "year", "d_pat", "year_d_pat")]
firms_patenting <- firms_patenting %>% group_by(firmid) %>% mutate(unique_d_pat=paste(unique(d_pat), collapse= ","),
                                                                   d_pat=ifelse(unique_d_pat=="0,1", d_pat, 0)) %>% select(-unique_d_pat)


feols(asin(new_products) ~ d_pat | firmid + year , data=firms_patenting)




