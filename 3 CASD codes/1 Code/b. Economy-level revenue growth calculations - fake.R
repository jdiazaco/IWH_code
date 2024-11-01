"
This file calculates measures of aggregate revenue growth and reallocation from prodcom product revenue information, 
decomposed by product status within the firm (first introduction, continuing products, discontinued and reintroduced products).
It creates plots for these results.

Sections 2 and 3 are validations of the aggregate revenue growth measures using aggregate and revenue weighted measures of growth.
Section 4 aims to compare revenue growth in prodcom against growth from revenue as reported in the BR and against France's GDP growth.
Section 4 has not been cleaned as of 26/06/24.


Author: Julián Díaz (using some snippets from Alex Magnuson's '2 reallocation dataset construction.R')
Edits: Julián Díaz-Acosta
Last update: 26/06/2024
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
setwd('C:/Users/NEWPROD_J_DIAZ-AC/Documents/Reallocation/6 Publish/2 Data/')
output_dir<-"C:/Users/NEWPROD_J_DIAZ-AC/Documents/Reallocation/6 Publish/3 Output/Graphs/"

#Bring tools
tools_dir <- 'C:/Users/NEWPROD_J_DIAZ-AC/Documents/Reallocation/6 Publish/1 Code/Tools/'
source(paste0(tools_dir, "description.R"))
source(paste0(tools_dir, "deflate.R"))
source(paste0(tools_dir, "summary stats helper.R"))
source(paste0(tools_dir, "parameters.R"))

# Set parameters for prodfra-pcc8 and excluded industries
exclude_industries<-TRUE
prodfra_or_pcc8<-"prodfra"
only_prodfra_in_prodcom<-FALSE

parameters(prodfra_or_pcc8, only_prodfra_in_prodcom, exclude_industries)


#Set start and end years
start=2009
end=2021


#0) Data merge  ------------------------------------

##import supplementary data
product_data<-readRDS(paste0("product_level_growth_", filter_indicator, "_.RDS"))
birth_death = readRDS('firm_birth_death.rds') 
sbs_data<-readRDS('sbs_br_combined.rds')


#Merge prodcom, sbs, br and bs data
product_firm_data<-merge(product_data, sbs_data, by=c("firmid", "year"), all.x = T)
rm(product_data)
gc()

#Create product age and tenure and firm age based on BR year creation (firm_birth_year)
product_firm_data[, `:=`(product_birth_year = min(year),
                         product_death_year = max(year)), by = c("firmid", "prodfra_plus")]
product_firm_data<-product_firm_data %>% mutate(age_product=ifelse((year-code_entry_year)>4, "established", "new"),
                                                age_product_years=year-code_entry_year,
                                                product_tenure=year-product_birth_year,
                                                age_firm=year-firm_birth_year)

#Manually create measure of aggregate growth and reallocation
product_firm_data<- product_firm_data %>% group_by(year) %>% mutate(within_economy_rev_share_test=rev_bar/sum(rev_bar, na.rm = T), 
                                                                    ind=ifelse(within_economy_rev_share_test==within_economy_rev_share, 1, 0)) %>% 
                                                            select(within_economy_rev_share, within_economy_rev_share_test, ind, everything())
product_firm_data<- product_firm_data %>% select(firmid, prodfra_plus, year, rev, rev_l, rev_bar, rev_growth, first_introduction, reintroduced, discontinued, incumbent, gap, everything())
product_firm_data<- as.data.table(product_firm_data)
product_firm_data[, rev_growth_weighted := rev_growth * within_economy_rev_share_test]
product_firm_data[, rev_reallocation_weighted := rev_reallocation * within_economy_rev_share_test]
setorder(product_firm_data, firmid, prodfra_plus, year)

#Save dataset
saveRDS(product_firm_data, "product_firm_data.RDS")

#1) Plot of economy-level revenue reallocation/growth rate ------------------------------------

#Bring in product firm data
product_firm_data<-readRDS("product_firm_data.RDS")

#Choose growth_or_ralloc to analyze agg. growth or reallocation. The following lines create the rest of the needed variables based on this 
growth_or_realloc<-"growth"
var<-"rev"; var_est<-paste0(var, "_", growth_or_realloc); var_est_weighted<-paste0(var_est, "_weighted")
est_rate<-paste0(growth_or_realloc, "_rate")

#Create yearly measures of growth/reallocation
product_firm_data[, agg := sum(get(var_est_weighted)), by = year]

## Decompose growth/reallocation by product status (contribution of each product status to aggregate growth/reallocation)
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

#Set min and maximum axis values
max_y<-max(est_decomp_long %>% filter(get(est_rate)>0, year>2009, subset!="agg") %>% group_by(year) %>% summarise(sum=sum(get(est_rate))) %>% select(sum))*100+1
min_y<-min(est_decomp_long %>% filter(get(est_rate)<0, year>2009, subset!="agg") %>% group_by(year) %>% summarise(sum=sum(get(est_rate))) %>% select(sum))*100

#Create plot
growth<-ggplot(est_decomp_long %>% filter(year>start, year<=end, subset!="agg"), aes(x = year, y = get(est_rate)*100, fill = subset)) +
  geom_bar(stat="identity", position="stack") + 
  geom_line(data=est_decomp_long %>% filter(year>start, subset=="agg"), aes(x=year, y=get(est_rate)*100, colour="agg" ), colour="black") +
  scale_fill_manual(values=c("black", brewer.pal(length(sub_groups), name="Paired")[1:4]))+
  scale_y_continuous(labels = scales::label_percent(scale = 1), limits=c(min(0, min_y), max_y), breaks =  seq(-20, 70, by = 10)) +
  scale_x_continuous(breaks =  seq(2010, end, by = 1)) +  
  labs(title = paste0("Decomposition of economy-level revenue ", growth_or_realloc, " rate"), subtitle =paste0("Deflated, ", label),  x = element_blank(),  y = paste0("Revenue ", growth_or_realloc, " rate"), fill = "Product status", colour="agg") +
  theme_classic() +theme(legend.position = 'bottom')

#Set plot attributes
name<-paste0('economy_rev_', growth_or_realloc, '_rate_decomposition_product_status.png')
ggsave(paste0(output_dir, name),  width=7, height = 5)
description(name, paste0("Graph with decomposition of economy-level ", growth_or_realloc, " growth, using ", label, ". Source: FARE/FICUS + Prodcom. \n"))

# Save est_decomposition for further validation
saveRDS(est_decomposition, "agg_growth_decomposition.RDS")


#2) Validation of economy-level revenue growth rate using aggregate revenue measures----------------------

#Bring in product-firm data and create a test dataset
product_firm_data<-readRDS("product_firm_data.RDS")
est_decomposition<-readRDS("agg_growth_decomposition.RDS")
test_data <- product_firm_data

# We decided we would set g=0 and rev_bar=0 for the years of entry and exit for products with discontinuous prodcom coverage
# To address this, we set rev_l=rev so that g=0 for gap years. rev_bar is already set to 0 for gap years in the previous script
test_data <- test_data[, rev := ifelse(gap==1,rev_l, rev)]

# Aggregate revenue, rev_bar and rev_l over years
test_data <- test_data[, .(rev = sum(rev, na.rm=T),
                           rev_bar=sum(rev_bar, na.rm=T),
                           rev_l=sum(rev_l, na.rm=T)),
                       by = .(year)]

# Calculate yearly measures of revenue growth
test_data <- test_data %>% mutate(rev_growth=((rev-rev_l)/rev_bar)*100)
test_data <- test_data %>% select(year, rev, rev_l, everything())

# Validate results
test_data<-merge(test_data, est_decomposition, by="year")
test_data[, check := (rev_growth/100)- agg]
if(max(abs(test_data$check))>1e-12){
  stop('validation not correct')
}


# Plot the results
ggplot(test_data %>% filter(year>2009), aes(x = year, y = rev_growth)) +
  geom_line() + scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  scale_x_continuous(breaks =  seq(2010, 2021, by = 1)) +
  labs(title = "Economy-level revenue growth rate", subtitle = "Using aggregate revenue measures", x = element_blank(),  y = "Revenue growth rate", color = "blue") +
  theme_classic() +theme(legend.position = 'bottom')

# Save plot
ggsave(paste0(output_dir, 'economy_rev_growth_rate_aggregate.png'),  width=6, height = 5)
description("economy_rev_growth_rate_aggregate.png", "Graph with economy-level revenue growth rate, using firm-product level revenue measures. Source: FARE/FICUS + Prodcom. \n")

#3) Validation of economy-level revenue growth rate using product-firm level revenue measures -----------------------

#Bring in product-firm data and create a test dataset
product_firm_data<-readRDS("product_firm_data.RDS")
est_decomposition<-readRDS("agg_growth_decomposition.RDS")
test_data_weights <- product_firm_data [, within_economy_rev_share_test :=  rev_bar/ sum(rev_bar, na.rm = T), by = .(year)]

#Create weighted revenue growth rate at the firm-product level
test_data_weights <- test_data_weights [, av_rev_growth :=  ((rev-rev_l)/rev_bar)*within_economy_rev_share_test]

#Aggregate over years
test_data_weights <- test_data_weights %>% group_by(year) %>% summarise( rev=sum(rev, na.rm = T), 
                                                                         rev_l=sum(rev_l, na.rm=T), 
                                                                         rev_bar=sum(rev_bar, na.rm=T), 
                                                                         within_economy_rev_share_test=sum(within_economy_rev_share_test, na.rm=T),
                                                                         av_rev_growth=sum(av_rev_growth, na.rm=T))

# Validate results
test_data_weights<-merge(test_data_weights, est_decomposition, by="year")
setDT(test_data_weights)
test_data_weights[, check := av_rev_growth- agg]
if(max(abs(test_data_weights$check))>1e-12){
  stop('validation not correct')
}

# Plot results
ggplot(test_data_weights %>% filter(year>2009), aes(x = year, y = av_rev_growth*100)) +
  geom_line() + scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  scale_x_continuous(breaks =  seq(2010, 2021, by = 1)) +
  labs(title = "Economy-level revenue growth rate", subtitle = "Using firm-product-level (i.e. weighting) revenue measures", x = element_blank(),  y = "Revenue growth rate", color = "blue") +
  theme_classic() +theme(legend.position = 'bottom')

# Save plot
ggsave(paste0(output_dir, 'economy_rev_growth_rate_firm_product.png'),  width=6, height = 5)
description("economy_rev_growth_rate_firm_product.png", "Graph with economy-level revenue growth rate, using aggregate level revenue measures. Source: FARE/FICUS + Prodcom. \n")


#4) Plot of prodcom revenue growth, France's GDP growth and BR turnover growth ------------------------------------

product_firm_data<-readRDS("product_firm_data.RDS")
sbs_data<-readRDS('sbs_br_combined.rds')

product_firm_data<-deflate(product_data, "NACE", c("turnover", ""), start)

test<-product_firm_data[, c("firmid", "year", "prodfra_plus", "NACE", "NACE_BR", "rev", "nq", "turnover")]

# revenue <- product_firm_data %>% group_by(year) %>% summarise(rev_growth_weighted=sum(rev_growth_weighted, na.rm = T))

#Deflate turnover and nq. Caution: Here I'm using product NACE to deflate revenue. It might be better to use firm NACE, but this creates NAs.
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



