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
output_dir<-"C:/Users/NEWPROD_J_DIAZ-AC/Documents/Reallocation/3 Output/graphs/product breakdown/19.04.24/"

# Set parameters for prodfra-pcc8 and excluded industries
prodfra_or_pcc8<-"prodfra"
only_prodfra_in_prodcom<-FALSE
filter<-paste0(if(prodfra_or_pcc8=="prodfra") "10_digit" else if(prodfra_or_pcc8=="pcc8") "8_digit" else prodfra_or_pcc8, 
               "_",
               if(only_prodfra_in_prodcom) "only_prodfra_in_prodcom" else "all_prodfra")
label<-paste0(if(prodfra_or_pcc8=="prodfra") "using 10-digit codes" else if(prodfra_or_pcc8=="pcc8") "using 8-digit codes" else prodfra_or_pcc8, 
               ",",
               if(only_prodfra_in_prodcom) " only prodfra codes in prodcom " else " all prodfra codes ")


exclude_industries<-TRUE
filter<-paste0(filter, if(exclude_industries) "_exclude_industries" else "_not_exclude_industries")
label<-paste0(label, if(exclude_industries) "and excluding utilities and wholesale trade" else "and all industries")


##import supplementary data
product_data<-readRDS(paste0("product_level_growth_", filter, "_.RDS"))
birth_death = readRDS('firm_birth_death.rds') 
deflators <- fread("C:/Users/Public/1. Microprod/1. Microprod-H2020/NEW_INFRASTRUCTURE/Infra/AuxData/deflators.csv")
deflators <- deflators %>% filter(cc=="FR")
nace_DEFind <- fread("C:/Users/Public/1. Microprod/1. Microprod-H2020/NEW_INFRASTRUCTURE/Infra/MetaData/nace_DEFind.conc", colClasses = c('character'))
tools_dir <- 'C:/Users/NEWPROD_J_DIAZ-AC/Documents/Reallocation/1 Code/Product decomposition/Tools/'
source(paste0(tools_dir, "description.R"))
# source(paste0(tools_dir, "summary_stats_helper.R"))


#Bring in sbs, br and bs information
sbs_data<-readRDS('sbs_br_combined.rds')

#Set start and end years
start=2009
end=2021


#0) Data merge  ------------------------------------

#Merge prodcom, sbs, br and bs data
product_firm_data<-merge(product_data, sbs_data, by=c("firmid", "year"), all.x = T)
rm(product_data)
gc()

product_firm_data[, `:=`(product_birth_year = min(year),
                         product_death_year = max(year)), by = c("firmid", "prodfra_plus")]

product_firm_data<-product_firm_data %>% mutate(age_product=ifelse((year-code_entry_year)>4, "established", "new"),
                                                age_product_years=year-code_entry_year,
                                                product_tenure=year-product_birth_year,
                                                age_firm=year-firm_birth_year)

product_firm_data<- product_firm_data %>% group_by(year) %>% mutate(within_economy_rev_share_test=rev_bar/sum(rev_bar, na.rm = T), 
                                                                    ind=ifelse(within_economy_rev_share_test==within_economy_rev_share, 1, 0)) %>% 
                                                            select(within_economy_rev_share, within_economy_rev_share_test, ind, everything())

product_firm_data<- product_firm_data %>% select(firmid, prodfra_plus, year, rev, rev_l, rev_bar, rev_growth, first_introduction, reintroduced, discontinued, incumbent, gap, everything())
product_firm_data<- as.data.table(product_firm_data)
product_firm_data[, rev_growth_weighted := rev_growth * within_economy_rev_share_test]
product_firm_data[, rev_reallocation_weighted := rev_reallocation * within_economy_rev_share_test]
setorder(product_firm_data, firmid, prodfra_plus, year)

saveRDS(product_firm_data, "product_firm_data.RDS")

#1) Test of economy-level revenue growth rate ------------------------------------

## generate weighted growth metric 



product_firm_data<-readRDS("product_firm_data.RDS")

if(exclude_industries){
  ind_to_exclude <- c(35,36,37,38,39,46) 
  product_firm_data<-product_firm_data %>% filter(!(substr(NACE, 1, 2) %in% ind_to_exclude))
}

growth_or_realloc<-"growth"
var<-"rev"; var_est<-paste0(var, "_", growth_or_realloc); var_est_weighted<-paste0(var_est, "_weighted")
est_rate<-paste0(growth_or_realloc, "_rate")



product_firm_data[, agg := sum(get(var_est_weighted)), by = year]

# product_firm_data_first_introduction <- product_firm_data %>% filter(first_introduction & year==2010)

## carry out decomposition 
sub_groups = c('first_introduction', 'reintroduced', 'discontinued', 'incumbent')
sub_groups_g = paste0(sub_groups, '_', growth_or_realloc)
for (i in seq_along(sub_groups)){
  product_firm_data[, sub_groups_g[i]:= sum(get(var_est_weighted) * get(sub_groups[i])), by = year]
}
est_decomposition = unique(product_firm_data %>% select(year,agg,sub_groups_g))

## check decomposition results 
est_decomposition[, check := agg- rowSums(.SD, na.rm = T), .SDcols = sub_groups_g]
if(max(abs(est_decomposition$check))>1e-12){
  stop('decomp not correct')
}


## output decomposition results 
colnames(est_decomposition)[3:6] = sub_groups
est_decomp_long = melt(est_decomposition, id.vars = 'year', measure.vars = c('agg',sub_groups))
colnames(est_decomp_long) = c('year', 'subset', est_rate)

max_y<-max(est_decomp_long %>% filter(get(est_rate)>0, year>2009, subset!="agg") %>% group_by(year) %>% summarise(sum=sum(get(est_rate))) %>% select(sum))*100+1
min_y<-min(est_decomp_long %>% filter(get(est_rate)<0, year>2009, subset!="agg") %>% group_by(year) %>% summarise(sum=sum(get(est_rate))) %>% select(sum))*100


growth<-ggplot(est_decomp_long %>% filter(year>start, year<=end, subset!="agg"), aes(x = year, y = get(est_rate)*100, fill = subset)) +
  geom_bar(stat="identity", position="stack") + 
  geom_line(data=est_decomp_long %>% filter(year>start, subset=="agg"), aes(x=year, y=get(est_rate)*100, colour="agg" ), colour="black") +
  scale_fill_manual(values=c("black", brewer.pal(length(sub_groups), name="Paired")[1:4]))+
  scale_y_continuous(labels = scales::label_percent(scale = 1), limits=c(min(0, min_y), max_y), breaks =  seq(-20, 70, by = 10)) +
  scale_x_continuous(breaks =  seq(2010, end, by = 1)) +  
  labs(title = paste0("Decomposition of economy-level revenue ", growth_or_realloc, " rate"), subtitle =paste0("Deflated (base year 2009), ", label),  x = element_blank(),  y = paste0("Revenue ", growth_or_realloc, " rate"), fill = "Product status", colour="agg") +
  theme_classic() +theme(legend.position = 'bottom')

reallocation<-ggplot(est_decomp_long %>% filter(year>start, year<=end, subset!="agg"), aes(x = year, y = get(est_rate)*100, fill = subset)) +
  geom_bar(stat="identity", position="stack") + 
  geom_line(data=est_decomp_long %>% filter(year>start, subset=="agg"), aes(x=year, y=get(est_rate)*100, colour="agg" ), colour="black") +
  scale_fill_manual(values=c("black", brewer.pal(length(sub_groups), name="Paired")[1:4]))+
  scale_y_continuous(labels = scales::label_percent(scale = 1), limits=c(min(0, min_y), max_y), breaks =  seq(-20, 70, by = 10)) +
  scale_x_continuous(breaks =  seq(2010, end, by = 1)) +  
  labs(title = paste0("Decomposition of economy-level revenue ", growth_or_realloc, " rate"), subtitle =paste0("Deflated (base year 2009), ", label),  x = element_blank(),  y = paste0("Revenue ", growth_or_realloc, " rate"), fill = "Product status", colour="agg") +
  theme_classic() +theme(legend.position = 'bottom')

graph<-growth | reallocation


((manufacturing_g +theme(axis.title.x  = element_blank(), legend.position = 'none')) |
    (services_g  + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = 'none')) |
    (`other sector_g` + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = 'right'))) +
  plot_annotation(title = plot_titles[i], subtitle = subtitle)

name<-paste0('economy_rev_', growth_or_realloc, '_rate_decomposition_product_status.png')
ggsave(paste0(output_dir, name),  width=7, height = 5)
description(name, paste0("Graph with decomposition of economy-level ", growth_or_realloc, " growth, using ", label, ". Source: FARE/FICUS + Prodcom. \n"))


#Deflated using industry value added price indices from Eurostat (base year 2009)
ggsave(paste0(output_dir, 'economy_rev_growth_rate_decomposition_product_status_deflated_exc_energy.png'),  width=7, height = 5)

# description(economy_rev_growth_rate_decomposition_product_status_nondeflated.png, "Graph with decomposition of economy-level revenue growth, non-deflated. Source: FARE/FICUS + Prodcom. \n")
description(economy_rev_growth_rate_decomposition_product_status_deflated_energy.png, "Graph with decomposition of economy-level revenue growth, deflated using added PF codes and value price index for industries, including firms in the energy sector. Source: FARE/FICUS + Prodcom. \n")

# revenue <- product_firm_data %>% group_by(year) %>% summarise(rev_growth_weighted=sum(rev_growth_weighted, na.rm = T))

#Deflate turnover and nq. Caution: Here I'm using product NACE. It might be better to use industry NACE, but this creates NAs.
product_firm_data <- product_firm_data %>% mutate(turnover=turnover/pnv_start_NACE,
                                                   nq=nq/pnv_start_NACE)

revenue <- product_firm_data[, rev := ifelse(gap==1,rev_l, rev)]

revenue <- revenue %>% group_by(firmid, year) %>% summarise(turnover=mean(turnover, na.rm = T), 
                                                                      nq=mean(nq, na.rm = T), 
                                                                      rev_og=sum(rev_og, na.rm = T),
                                                                      rev=sum(rev, na.rm = T),
                                                                      # rev_growth_weighted=sum(rev_growth_weighted, na.rm = T),
                                                                      # within_economy_rev_share=sum(within_economy_rev_share, na.rm = T),
                                                                      # rev_growth=sum(rev_growth, na.rm = T),
                                                                      born=mean(born),
                                                                      died=mean(died))

# revenue <- revenue %>% group_by(year) %>% mutate(within_economy_rev_share_nq=rev_bar/sum(rev_bar, na.rm = T))

sbs_revenue <- sbs_data %>%  mutate(turnover_economy=turnover, nq_economy=nq,
                                    NACE_BR=as.character(NACE_BR)) %>% select(firmid, year, turnover_economy, nq_economy, NACE_BR)
sbs_revenue <- merge(sbs_revenue, nace_DEFind, by.x="NACE_BR", by.y="nace", all.x = T)
deflators <- deflators %>% group_by(DEFind) %>% mutate(pnv= pnv/pnv[year==2009])
sbs_revenue <- merge(sbs_revenue, deflators %>% select(DEFind, year, pnv) , by=c("DEFind", "year"), all.x = T)
sbs_revenue <- sbs_revenue %>% mutate(turnover_economy=turnover_economy/pnv, nq_economy=nq_economy/pnv)
sbs_revenue <- sbs_revenue %>% select(firmid, year, turnover_economy, nq_economy)

revenue <- merge(sbs_revenue, revenue, by=c("firmid", "year"), all.x=T, all.y=T)


#Caution: This code is order sensitive in the names of the "normal_cols"
# normal_cols = c("turnover", "nq", "rev_og", "rev")
normal_cols = c("turnover_economy", "nq_economy", "turnover", "nq", "rev_og", "rev")
lag_cols = paste0(normal_cols,'_l')
revenue_l = revenue %>% filter(year<end) %>% mutate(year = year + 1) %>% select(-born, -died)#, -rev_growth_weighted, -within_economy_rev_share, -rev_growth)
colnames(revenue_l)[names(revenue_l) %in% normal_cols] = lag_cols
revenue = merge(revenue, revenue_l ,by=c("year", "firmid"), all = T)
revenue <- revenue %>% filter(!is.na(born) & !is.na(died))
setorder(revenue, firmid, year)

# revenue = merge(revenue,birth_death, by=c('firmid'))
revenue<-as.data.table(revenue)
# revenue[, `:=`(born = !is.na(birth_year) & birth_year == year,
#                     died = !is.na(death_year) & death_year < year)]
# revenue[, status:= ifelse(born, 'born', ifelse(died, 'died', 'survived'))]

for (i in seq_along(normal_cols)){
  revenue[is.na(get(normal_cols[i])), normal_cols[i]:=0]
  revenue[is.na(get(lag_cols[i])), lag_cols[i]:=0]
}

## fix first / last year of data 
revenue <- revenue %>% mutate(across(all_of(lag_cols), ~ifelse(born==1, 0, .)))
revenue <- revenue %>% mutate(across(all_of(normal_cols), ~ifelse(died==1, 0, .)))


for (var in normal_cols){
  var_l<-paste0(var, "_l")
  var_bar<-paste0(var, "_bar")
  var_growth<-paste0(var, "_growth")
  var_growth_weighted<-paste0(var, "_growth_weighted")
  within_economy_rev_share<-paste0("within_economy_rev_share_", var)
    
  revenue<- revenue %>% mutate( !!var_bar := 0.5*(.data[[var]] + .data[[var_l]]))
  revenue<- revenue %>% mutate( !!var_growth := ifelse(.data[[var_bar]] != 0, ((.data[[var]] - (.data[[var_l]]))/.data[[var_bar]]), 0))
  revenue<- revenue %>% group_by(year) %>% mutate( !!within_economy_rev_share := .data[[var_bar]]/sum(.data[[var_bar]], na.rm = T))
  revenue<- revenue %>% mutate( !!var_growth_weighted := (.data[[var_growth]])*(.data[[within_economy_rev_share]]))
  }




revenue_plot <- revenue %>% group_by(year) %>% summarise(rev=sum(rev, na.rm = T),
                                                         rev_l=sum(rev_l, na.rm = T),
                                                         rev_bar=sum(rev_bar, na.rm = T),
                                                         rev_og_growth=sum(rev_og_growth_weighted, na.rm = T),
                                                         rev_growth=sum(rev_growth_weighted, na.rm = T),
                                                         nq_growth=sum(nq_growth_weighted, na.rm = T),
                                                         turnover_growth=sum(turnover_growth_weighted, na.rm = T),
                                                         nq_economy_growth=sum(nq_economy_growth_weighted, na.rm = T),
                                                         turnover_economy_growth=sum(turnover_economy_growth_weighted, na.rm = T),
                                                         )

revenue_plot <- merge(revenue_plot, growth_decomp_long %>% filter(subset=="agg") %>% select(-subset), by="year")
revenue_plot <- revenue_plot %>% select(-rev, -rev_l, -rev_bar, -rev_og_growth, -rev_growth)
france_gdp_growth<-data.frame(year=2009:2020, variable="gdp growth", value=c(-0.029, 0.019, 0.022, 0.003, 0.006, 0.01, 0.011, 0.011, 0.023, 0.019, 0.018, -0.075))
subgroups<-colnames(revenue_plot)[-1]

revenue_plot_long = melt(revenue_plot, id.vars = 'year', measure.vars = c(subgroups))
revenue_plot_long = revenue_plot_long %>% filter(variable=="turnover_growth"| variable=="turnover_economy_growth" |variable=="growth_rate")
revenue_plot_long <- revenue_plot_long %>% mutate(variable=ifelse(variable=="turnover_growth", "BS - only prodcom firms", 
                                                                  ifelse(variable=="turnover_economy_growth", "BS - all",
                                                                         "prodcom")))
revenue_plot_long<-rbind(revenue_plot_long, france_gdp_growth)

max_y<-37
min_y<--10
ggplot(revenue_plot_long %>% filter(year>2009), aes(x = year, y = value*100, color = variable)) +
  geom_line() + scale_y_continuous(labels = scales::label_percent(scale = 1), limits = c(min_y, max_y)) +
  scale_x_continuous(breaks =  seq(2010, 2021, by = 1)) +
  labs(title = "Aggregate revenue growth rate", subtitle ="Comparing revenue numbers from Prodcom, BS and GDP growth - including energy",  x = element_blank(),  y = "Revenue growth rate", color = "Sample") +
  theme_classic() +theme(legend.position = 'bottom')

ggsave(paste0(output_dir, 'rev_growth_bs_prodcom_energy.png'),  width=7, height = 5)
description(rev_growth_bs_prodcom_energy.png, "Graph comparing revenue growth rates computed from prodcom and FARE/FICUS - including energy. Source: FARE/FICUS + Prodcom. \n")



#2) Test of economy-level revenue reallocation rate ------------------------------------

## generate weighted reallocation metric 
product_firm_data[, rev_reallocation_weighted := rev_reallocation * within_economy_rev_share_test]
product_firm_data[, agg := sum(rev_reallocation_weighted), by = year]

## carry out decomposition 
sub_groups = c('first_introduction', 'reintroduced', 'discontinued', 'incumbent')
sub_groups_g = paste0(sub_groups, '_reallocation')
for (i in seq_along(sub_groups)){
  product_firm_data[, sub_groups_g[i]:= sum(rev_reallocation_weighted * get(sub_groups[i])), by = year]
}
reallocation_decomposition = unique(product_firm_data %>% select(year,agg,sub_groups_g))

## check decomposition results 
reallocation_decomposition[, check := agg- rowSums(.SD, na.rm = T), .SDcols = sub_groups_g]
if(max(abs(reallocation_decomposition$check))>1e-12){
  stop('decomp not correct')
}
reallocation_decomposition[, check:=NULL]

## output decomposition results 
colnames(reallocation_decomposition)[3:6] = sub_groups
reallocation_decomp_long = melt(reallocation_decomposition, id.vars = 'year', measure.vars = c('agg',sub_groups))
colnames(reallocation_decomp_long) = c('year', 'subset', 'reallocation_rate')
# saveRDS(reallocation_decomp_long, 'decompositions/reallocation_rate_decomp.rds')

# ggplot(reallocation_decomp_long %>% filter(year>2009), aes(x = year, y = reallocation_rate*100, color = subset)) +
#   geom_line() + scale_y_continuous(labels = scales::label_percent(scale = 1)) +
#   scale_x_continuous(breaks =  seq(2010, 2021, by = 1)) +  
#   labs(title = "Decomposition of economy-level revenue reallocation rate", subtitle ="Deflated using industry value added price indices from Eurostat (base year 2009)",  x = element_blank(),  y = "Revenue reallocation rate", color = "Product status") +
#   theme_classic() +theme(legend.position = 'bottom')

ggplot(reallocation_decomp_long %>% filter(year>2009, subset!="agg"), aes(x = year, y = reallocation_rate*100, fill = subset)) +
  geom_bar(stat="identity", position="stack") + 
  geom_line(data=reallocation_decomp_long %>% filter(year>2009, subset=="agg"), aes(x=year, y=reallocation_rate*100), colour="black") +
  scale_fill_brewer(palette = 'Paired') +
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  scale_x_continuous(breaks =  seq(2010, 2021, by = 1)) +  
  labs(title = "Decomposition of economy-level revenue reallocation rate", subtitle ="Deflated using industry value added price indices from Eurostat (base year 2009)",  x = element_blank(),  y = "Revenue reallocation rate", fill = "Product status", colour="agg") +
  theme_classic() +theme(legend.position = 'bottom') 

ggsave(paste0(output_dir, 'rev_growth_bs_prodcom'),  width=7, height = 5)
description(economy_rev_reallocation_rate_decomposition_product_status_deflated.png, "Graph with decomposition of economy-level reallocation rate, deflated using added value price index for industries. Source: FARE/FICUS + Prodcom. \n")

# description(economy_rev_growth_rate_decomposition_product_status_nondeflated.png, "Graph with decomposition of economy-level revenue growth, non-deflated. Source: FARE/FICUS + Prodcom. \n")
description(economy_rev_growth_rate_decomposition_product_status_deflated.png, "Graph with decomposition of economy-level revenue growth, deflated using added PF codes and value price index for industries. Source: FARE/FICUS + Prodcom. \n")


#3) Test of economy-level revenue reallocation rate ------------------------------------


vars<-c("rev_og", "rev")
subtitle<-c("Non-deflated", "Deflated using industry value added price indices from Eurostat (base year 2009)")

for(i in 1:2){
  # i<-1
  var<-vars[i]
  empl_rev_thresholds<-product_firm_data %>% mutate(empl_ind=ifelse(empl>=20, ">=20", "<20"))
  empl_rev_thresholds<-empl_rev_thresholds %>% group_by(year, empl_ind) %>% summarise(agg_rev=sum(get(var), na.rm=T))
  
  ggplot(empl_rev_thresholds %>% filter(!is.na(empl_ind)), aes(x = year, y = agg_rev, color = empl_ind)) +
    geom_line() + scale_y_continuous() +
    scale_x_continuous(breaks =  seq(2001, 2021, by = 1)) +
    labs(title = "Decomposition of economy-level revenue reallocation rate", subtitle =subtitle[i],  x = element_blank(),  y = "Revenue", color = "Number of employees") +
    theme_classic() +theme(legend.position = 'bottom')
  
  # ggsave(paste0(output_dir, var, '_empl_disaggregated.png'),  width=7, height = 5)
  # description(paste0(output_dir, var, '_empl_disaggregated.png'), paste0("Graph with aggregated revenue per year for firms with 20 employees or more and firms with less than 20 employees.", subtitle[i], "Source: FARE/FICUS + Prodcom. \n"))
  
}



#3o) Trashcan ------------------------------------

end=2020

# product_firm_data_test= product_firm_data %>% group_by(year) %>% summarise(rev=sum(rev, na.rm=T), rev_l=sum(rev_l, na.rm=T))


#1. Using aggregate revenue measures
#Aggregate revenue and average revenue over years
product_firm_data_og<-product_firm_data
test_data <- product_firm_data
# test_data$rev<-test_data$rev_og
test_data <- test_data[, rev := ifelse(gap==1,rev_l, rev)]
test_data <- test_data[, .(rev = sum(rev, na.rm=T),
                           rev_bar=sum(rev_bar, na.rm=T),
                           rev_l=sum(rev_l, na.rm=T)),
                       by = .(year)]



# test_data2<-product_firm_data %>% group_by(year) %>% summarise(rev = sum(rev, na.rm=T),
#                                                                rev_l=sum(rev_l, na.rm=T))
# 
# #Create aggregate revenue lags and bring it to the original dataset
# normal_cols = c('rev')
# lag_cols = paste0(normal_cols,'_l')
# test_data_l = test_data[year<end] %>% mutate(year = year + 1) %>% select(-rev_bar)
# colnames(test_data_l)[names(test_data_l) %in% normal_cols] = lag_cols
# test_data = merge(test_data, test_data_l,by="year", all = T)
test_data <- test_data %>% mutate(rev_growth=((rev-rev_l)/rev_bar)*100)
test_data <- test_data %>% select(year, rev, rev_l, everything())

ggplot(test_data %>% filter(year>2009), aes(x = year, y = rev_growth)) +
  geom_line() + scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  scale_x_continuous(breaks =  seq(2010, 2021, by = 1)) +
  labs(title = "Economy-level revenue growth rate", subtitle = "Using aggregate revenue measures", x = element_blank(),  y = "Revenue growth rate", color = "blue") +
  theme_classic() +theme(legend.position = 'bottom')
ggsave(paste0(output_dir, 'economy_rev_growth_rate_aggregate.png'),  width=6, height = 5)
description(economy_rev_growth_rate_aggregate.png, "Graph with economy-level revenue growth rate, using firm-product level revenue measures. Source: FARE/FICUS + Prodcom. \n")



#2. Using product-firm level revenue measures
#Create revenue shares
test_data_weights <- product_firm_data [, within_economy_rev_share_test :=  rev_bar/ sum(rev_bar, na.rm = T), by = .(year)]

#Create weighted revenue growth rate at the firm-product level
test_data_weights <- test_data_weights [, av_rev_growth :=  ((rev-rev_l)/rev_bar)*within_economy_rev_share_test]

#Aggregate over years
test_data_weights <- test_data_weights %>% group_by(year) %>% summarise( rev=sum(rev, na.rm = T), 
                                                                         rev_l=sum(rev_l, na.rm=T), 
                                                                         rev_bar=sum(rev_bar, na.rm=T), 
                                                                         within_economy_rev_share_test=sum(within_economy_rev_share_test, na.rm=T),
                                                                         av_rev_growth=sum(av_rev_growth, na.rm=T))



# #Create yearly weighted growth rates
# test_data_weights <- test_data_weights %>% mutate(av_rev_growth=((rev-rev_l)/rev_bar)*100)
# 
# test_data_weights <- test_data_weights %>% group_by(year) %>% summarise(av_rev_growth=sum(av_rev_growth, na.rm=T))




ggplot(test_data_weights %>% filter(year>2009), aes(x = year, y = av_rev_growth*100)) +
  geom_line() + scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  scale_x_continuous(breaks =  seq(2010, 2021, by = 1)) +
  labs(title = "Economy-level revenue growth rate", subtitle = "Using firm-product-level (i.e. weighting) revenue measures", x = element_blank(),  y = "Revenue growth rate", color = "blue") +
  theme_classic() +theme(legend.position = 'bottom')
ggsave(paste0(output_dir, 'economy_rev_growth_rate_firm_product.png'),  width=6, height = 5)
description(economy_rev_growth_rate_firm_product.png, "Graph with economy-level revenue growth rate, using aggregate level revenue measures. Source: FARE/FICUS + Prodcom. \n")

saveRDS(product_firm_data, 'product_firm.rds')


rm(test_data_l, normal_cols, lag_cols)
gc()
