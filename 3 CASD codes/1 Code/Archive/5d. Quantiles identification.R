
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
output_dir<-"C:/Users/NEWPROD_J_DIAZ-AC/Documents/Reallocation/3 Output/graphs/product breakdown/12.04.24/"

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

product_firm_data<-readRDS(paste0("product_firm_data_fpqi_", filter,  "_.RDS"))


start<-2009
end<-2020

#4) Superstar product estimation ------------------------------------


superstar <- product_firm_data
superstar <- superstar %>% group_by(year, prodfra_plus) %>% summarise(rev=sum(rev, na.rm = T), 
                                                                       rev_l=sum(rev_l, na.rm=T), 
                                                                       rev_bar=sum(rev_bar, na.rm=T))
superstar <- superstar %>% mutate(rev_growth=ifelse(rev_bar != 0, (rev - rev_l)/rev_bar, 0)) %>% select(year, prodfra_plus, rev_growth)


# superstar$NACE_2d_codes<-substr(superstar$prodfra_plus,1,2)
# NACE_2d_codes<-unique(superstar$NACE_2d_codes)


# variables <- c("rev_growth_weighted")
# for (var in variables){
#   for(yr in start:end){
#     superstar_temp<-superstar %>% filter(year==yr)
#     counter<-0
#     
#     for(nace in NACE_2d_codes){
#         if(nace %in% unique(superstar_temp$NACE_2d_codes)){
#         superstar_temp<-superstar_temp %>% filter(NACE_2d_codes==nace)
#         
#         # superstar_temp[paste0(var,"_percentile")]<-NA
#         # superstar_temp[paste0(var,"_decile")]<-NA
#         superstar_temp[paste0(var,"_quintile")]<-NA
#         
#         # percentiles <- quantile(superstar_temp[[var]], probs=seq(0, 1, by=0.01), na.rm=T)
#         # deciles <- quantile(superstar_temp[[var]], probs=seq(0, 1, by=0.1), na.rm=T)
#         quintiles <- quantile(superstar_temp[[var]], probs=seq(0, 1, by=0.2), na.rm=T)
#         
#         # superstar_temp[paste0(var,"_percentile")]<-cut(superstar_temp[[var]], breaks=percentiles, labels=F)
#         # superstar_temp[paste0(var,"_decile")]<-cut(superstar_temp[[var]], breaks=deciles, labels=F)
#         superstar_temp[paste0(var,"_quintile")]<-cut(superstar_temp[[var]], breaks=quintiles, labels=F)
#         
#         superstar_yr_nace<-paste0("superstar_", yr, "_", nace)
#         assign(superstar_yr_nace, superstar_temp)
#         
#         if(counter==0){
#           superstar_quantiles_nace=superstar_temp
#         }else{
#           superstar_quantiles_nace<-rbind(superstar_quantiles_nace, superstar_yr_nace)
#         }
#         rm(list=superstar_yr_nace)
#         counter<-counter+1
#         
#       }
#     }
#     
#     if(yr==start){
#       superstar_quantiles=superstar_quantiles_nace
#     }else{
#       superstar_quantiles<-rbind(superstar_quantiles, get(paste0("superstar_", yr)))
#     }
#     rm(list=superstar_yr)
#     
#   }
# }

start<-2010
variables <- c("rev_growth")
for (var in variables){
  for(yr in start:end){
    superstar_temp<-superstar %>% filter(year==yr)

    superstar_temp[paste0(var,"_percentile")]<-NA
    superstar_temp[paste0(var,"_decile")]<-NA
    superstar_temp[paste0(var,"_quintile")]<-NA

    percentiles <- quantile(superstar_temp[[var]], probs=seq(0, 1, by=0.01), na.rm=T)
    deciles <- quantile(superstar_temp[[var]], probs=seq(0, 1, by=0.1), na.rm=T)
    quintiles <- quantile(superstar_temp[[var]], probs=seq(0, 1, by=0.2), na.rm=T)

      superstar_temp[paste0(var,"_percentile")]<-cut(superstar_temp[[var]], breaks=unique(percentiles), labels=F, include.lowest = T)
      superstar_temp[paste0(var,"_decile")]<-cut(superstar_temp[[var]], breaks=unique(deciles), labels=F, include.lowest = T)
      superstar_temp[paste0(var,"_quintile")]<-cut(superstar_temp[[var]], breaks=unique(quintiles), labels=F, include.lowest = T)

      superstar_yr<-paste0("superstar_", yr)
    assign(superstar_yr, superstar_temp)
    
    if(sum(is.na(superstar_temp[[paste0(var,"_percentile")]]!=0))){
      stop('decomp not correct')
    }
    

    if(yr==start){
      superstar_quantiles=superstar_temp
    }else{
      superstar_quantiles<-rbind(superstar_quantiles, get(paste0("superstar_", yr)))
    }
    rm(list=superstar_yr)
  }
}



superstar_quantiles <- superstar_quantiles %>% group_by(year) %>% mutate(agg = sum(rev_growth_weighted))

for(q in c("percentile", "decile", "quintile")){
  var<-paste0("rev_growth_", q)
  superstar_q<-superstar_quantiles %>% select(-rev_growth)
  superstar_q<-superstar_q %>% select(year, prodfra_plus, var)
  superstar_q<-pivot_wider(superstar_q, names_from = year, values_from = var)
  superstar_q$score<- rowSums(superstar_q[, -1]^2)
  
  write.xlsx(superstar_q, paste0(output_dir, q, "_switching_", filter, ".xlsx"))
  description(paste0(q, "_switching_", filter, ".xlsx"), paste0("This table divides product codes by ", q, "s based on their growth rate in the EAP panel per year. \n"))
  assign(paste0("superstar_", q), superstar_q)
  
  
  
  # superstar_quantiles_temp<-superstar_quantiles %>% group_by(year, get(var)) %>% summarise(growth_rate=sum(rev_growth_weighted, na.rm =T)) 
  # colnames(superstar_quantiles_temp)[colnames(superstar_quantiles_temp)=="get(var)"]<-q
  # superstar_quantiles_temp[[q]]<-superstar_quantiles_temp[[q]]-1
  # # superstar_quantiles_temp[[q]]<-as.character(superstar_quantiles_temp[[q]])
  # superstar_quantiles_temp_2<-superstar_quantiles %>% group_by(year) %>% summarise(growth_rate=sum(rev_growth_weighted, na.rm =T)) %>% mutate(var="agg")
  # colnames(superstar_quantiles_temp_2)[colnames(superstar_quantiles_temp_2)=="var"]<-q
  # # superstar_quantiles_temp<-rbind(superstar_quantiles_temp, superstar_quantiles_temp_2) 
  # 
  # 
  # ggplot(superstar_quantiles_temp %>% filter(year>start, year<=end) %>% arrange(growth_rate), aes(x = year, y = growth_rate*100, fill = as.factor(get(q)))) +
  #   geom_bar(stat="identity", position="stack") +
  #   geom_line(data=superstar_quantiles_temp_2 %>% filter(year>start), aes(x=year, y=growth_rate*100, colour="agg"), colour="black") +
  #   scale_fill_brewer(palette = 'Paired') +
  #   scale_y_continuous(labels = scales::label_percent(scale = 1), limits=c(-15, 30), breaks =  seq(-20, 70, by = 10)) +
  #   scale_x_continuous(breaks =  seq(2010, end, by = 1)) +
  #   labs(title = paste0("Decomposition of economy-level revenue growth rate by ", q, "s"), 
  #        subtitle =paste0("Deflated (base year 2009), ", description_energy, ", ", description_PF_PC, ", ", description_digits),  x = element_blank(),  y = "Revenue growth rate", fill = "Product status", colour="agg") +
  #   theme_classic() +theme(legend.position = 'bottom')
  # ggsave(paste0(output_dir, 'rev_growth_', q, "_", filter, "_", description_energy, '.png'),  width=7, height = 5)
  # description(paste0('rev_growth_', q, "_", filter, "_", description_energy, '.png'), paste0("Graph with decomposition of economy-level revenue growth by ", q, ", using invoice level aggregation from EAP, ", description_PF_PC, ", ", description_digits,  " and ", description_energy, ". Source: FARE/FICUS + Prodcom. \n"))
  # 
  # 
  # rm(superstar_q)
  
}


superstar_growth<-superstar_quantiles %>% select(year, prodfra_plus, rev_growth)
superstar_growth<-superstar_growth %>% select(year, prodfra_plus, rev_growth)
superstar_growth$rev_growth<-superstar_growth$rev_growth*100
superstar_growth<-pivot_wider(superstar_growth, names_from = year, values_from = rev_growth)

write.xlsx(superstar_growth, paste0(output_dir, "growth", "_switching_", filter, ".xlsx"))
description(paste0("growth_switching_", filter, ".xlsx"), paste0("This table divides product codes by gwoth rate in the EAP panel per year. \n"))
assign(paste0("superstar_", q), superstar_q)


for(q in c("percentile", "decile", "quintile")){
}


      description(paste0('economy_rev_growth_', filter, "_", description_energy, '.png'), paste0("Graph with decomposition of economy-level revenue growth, using invoice level aggregation from EAP, ", description_PF_PC, ", ", description_digits,  " and ", description_energy, ". Source: FARE/FICUS + Prodcom. \n"))
<-pivot_wider(superstar_quantiles_clean, names_from = year, values_from = c("rev_growth_weighted_percentile", "rev_growth_weighted_decile", "rev_growth_weighted_quintile"))



superstar <- product_firm_data %>% group_by(prodfra_plus) %>% filter(ifelse(type=="incumbent",incumbent==T,T)) %>%
  summarise(rev_growth_weighted_superstar=sum(rev_growth_weighted, na.rm=T)*100,
            av_rev_growth_superstar=mean(rev_growth, na.rm = T)*100)

product_firm_data_incumbent <- product_firm_data %>% filter(incumbent==T)
variables <- c("rev_growth", "rev_growth_weighted")
for (var in variables){
  percentiles <- quantile(product_firm_data[[var]], probs=seq(0, 1, by=0.01), na.rm=T)
  deciles <- quantile(product_firm_data[[var]], probs=seq(0, 1, by=0.1), na.rm=T)
  quintiles <- quantile(product_firm_data[[var]], probs=seq(0, 1, by=0.2), na.rm=T)
  
  #  product_firm_data_incumbent[paste0(var,"_percentile")]<-cut(product_firm_data_incumbent[[var]], breaks=percentiles, labels=F)
  #  product_firm_data_incumbent[paste0(var,"_decile")]<-cut(product_firm_data_incumbent[[var]], breaks=deciles, labels=F)
  #  product_firm_data_incumbent[paste0(var,"_quintile")]<-cut(product_firm_data_incumbent[[var]], breaks=quintiles, labels=F)
  assign(paste0(var, "_top_quintile_cutoff"), quintiles[5])
}

variables <- c("rev_growth_weighted_superstar", "av_rev_growth_superstar")
for (var in variables){
  percentiles <- quantile(superstar[[var]], probs=seq(0, 1, by=0.01), na.rm=T)
  deciles <- quantile(superstar[[var]], probs=seq(0, 1, by=0.1), na.rm=T)
  quintiles <- quantile(superstar[[var]], probs=seq(0, 1, by=0.2), na.rm=T)
  
  superstar[[paste0(var,"_percentile")]]<-cut(superstar[[var]], breaks=percentiles, labels=F)
  superstar[[paste0(var,"_decile")]]<-cut(superstar[[var]], breaks=deciles, labels=F)
  superstar[[paste0(var,"_quintile")]]<-cut(superstar[[var]], breaks=quintiles, labels=F)
  assign(paste0(var, "_top_quintile_cutoff"), quintiles[5])
}

superstar<-superstar %>% mutate(superstar=ifelse(av_rev_growth_superstar>av_rev_growth_superstar_top_quintile_cutoff, "high-growth product", "not-high-growth product"))
product_firm_data<-merge(product_firm_data, superstar, by="prodfra_plus", all.x = T)


fpqi <- product_firm_data %>% group_by(prodfra_plus, firmid) %>% summarise(rev_growth_weighted_fpqi=sum(rev_growth_weighted, na.rm=T)*100,
                                                                           av_rev_growth_fpqi=mean(rev_growth, na.rm = T)*100, firm_product_quality_index=mean(firm_product_quality_index, na.rm=T))

percentiles <- quantile(fpqi$firm_product_quality_index, probs=seq(0, 1, by=0.01), na.rm=T)
deciles <- quantile(fpqi$firm_product_quality_index, probs=seq(0, 1, by=0.1), na.rm=T)
quintiles <- quantile(fpqi$firm_product_quality_index, probs=seq(0, 1, by=0.2), na.rm=T)
quartiles <- quantile(fpqi$firm_product_quality_index, probs=seq(0, 1, by=0.25), na.rm=T)

var<-"firm_product_quality_index"
fpqi$fpqi_percentile<-cut(fpqi[[var]], breaks=percentiles, labels=F)
fpqi$fpqi_decile<-cut(fpqi[[var]], breaks=deciles, labels=F)
fpqi$fpqi_quintile<-cut(fpqi[[var]], breaks=quintiles, labels=F)
fpqi$fpqi_quartile<-cut(fpqi[[var]], breaks=quartiles, labels=F)
fpqi$firm_product_quality_index_single<-fpqi$firm_product_quality_index
fpqi$firm_product_quality_index<-NULL
product_firm_data<-merge(product_firm_data, fpqi, by=c("prodfra_plus", "firmid"), all.x = T)