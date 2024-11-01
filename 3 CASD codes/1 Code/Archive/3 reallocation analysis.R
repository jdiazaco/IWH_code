# libraries
rm(list = ls())
gc()
libraries = c('data.table', 'haven', 'stringr', 'readr', 'dplyr',
              'ggplot2', 'tidyverse', 'rstudioapi', 'zoo', 'reshape2',
              'patchwork', 'latex2exp', "RColorBrewer", "gridExtra")
lapply(libraries, library, character.only = T)

# set up directories 
raw_dir = "C:/Users/Public/1. Microprod/0. Raw data processing/Data/"
output_dir<-"C:/Users/NEWPROD_J_DIAZ-AC/Documents/reallocation/3 Output/graphs/product breakdown/31.05.24/"
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd('C:/Users/NEWPROD_J_DIAZ-AC/Documents/Reallocation/2 Data/reallocation_construction_output/Product breakdown/')
start<-2009
end<-2021

source('C:/Users/Public/1. Microprod/Reallocation_work/1 Code/3.1 summary stats helper.R')


#1) compare PRODCOM and BR samples  ---------------------------------------------
## import data and neaten var names 
  comparison_dataset = readRDS('combined_sbs_br_prodcom_data.rds')
  # product_data = readRDS('product_data_10_digit_all_prodfra_.rds')
  # firm_data<-product_data %>% group_by(firmid, year) %>% summarise(rev=sum(rev))

  comparison_dataset= comparison_dataset %>%
    rename(revenue = nq, employees = empl, 'high tech' = high_tech,
           'full sample' = full_sample, 'prodcom only' = in_prodcom, 
           'other sectors' = other_sector, 'low tech' = low_tech)

  ## generate summary stats
  interest_vars =  c('employees', 'revenue','age', 'young','high tech',
                   'manufacturing', 'services')

# import function to generate summary stat csv and latex files 
source('C:/Users/NEWPROD_J_DIAZ-AC/Documents/Reallocation/1 Code/3.1 summary stats helper.R')
comparison_dataset <-comparison_dataset %>% filter(year>=2009)
  
# compare full sample and prodcom sample
sub_groups = c('full sample', 'prodcom only')
make_summary_stats(comparison_dataset, interest_vars, sub_groups, paste0(output_dir, 'prodcom_comparison'))

# compare manufacturing vs. services vs. other 
sub_groups_manufac = c('manufacturing', 'services', 'other sectors')
interest_vars_manufac = interest_vars[! interest_vars %in% c('manufacturing', 'services')]
make_summary_stats(comparison_dataset, interest_vars_manufac, sub_groups_manufac,
                   paste0(output_dir, 'manufac_vs_services'))

# compare high tech vs. low tech 
sub_groups_manufac = c('low tech', 'high tech')
interest_vars_manufac = interest_vars[! interest_vars %in% 'high tech']
make_summary_stats(comparison_dataset, interest_vars_manufac, sub_groups_manufac,
                   paste0(output_dir, 'high_vs_low_tech'))

## generate industry coverage rate
comparison_dataset[, `:=`(matched_rev = revenue*`prodcom only`, industry_rev = sum(revenue)), by = industry]
industry_comps = comparison_dataset[,.(obs_matched = round(sum(`prodcom only`,na.rm = T)/.N, 3),
                                       rev_matched =round(sum(revenue*`prodcom only`, na.rm = T)/sum(revenue, na.rm = T),3)), by = industry]
industry_comps= industry_comps[!is.na(industry)]
setorder(industry_comps, -rev_matched)
saveRDS(industry_comps, 'prodcom_industry_coverage.rds')

## generate change in sectoral composition over time 
  comparison_dataset = readRDS('combined_sbs_br_prodcom_data.rds')
  comparison_dataset[, `:=`(sector_name = ifelse(is.na(NACE_BR), NA,
                                                 ifelse(manufacturing, 'manufacturing',
                                                        ifelse(services, 'services', 'other sectors'))))]
  sectoral_decomp = comparison_dataset[!is.na(NACE_BR)]
  sectoral_decomp =  sectoral_decomp[, .(firms = .N,
                                           rev_share = sum(nq, na.rm =T),
                                           empl_share = sum(empl, na.rm =T)), by = .(year,sector_name)]
  
  sectoral_decomp =  sectoral_decomp[, `:=`(firm_share = firms/sum(firms),
                                         rev_share = rev_share/sum(rev_share),
                                         empl_share = empl_share/ sum(empl_share)), by = .(year)] %>%select(-firms)
  sectoral_decomp[,3:5] = round(sectoral_decomp[,3:5],2)
  ylim = c(0, max(sectoral_decomp[,3:5]))
  
  variables = c('firm_share', 'rev_share', 'empl_share')
  titles = c('Firm Share', 'Revenue Share', 'Labor Share')
  sector_graphs = lapply(seq_along(variables),function(i){
  ouput = ggplot(sectoral_decomp, aes(x = year, y = get(variables[i]), color = sector_name)) + geom_line() +
    scale_y_continuous(labels = scales::label_percent(scale = 100), limits = ylim) +
    labs(color = element_blank(), title = titles[i]) +theme(axis.title = element_blank())
  })
  output = ((sector_graphs[[1]] +theme(legend.position = 'none')) +
    (sector_graphs[[2]] + theme(legend.position = 'none', axis.text.y = element_blank(), axis.ticks.y = element_blank())) +
      (sector_graphs[[3]] + theme(legend.position = 'right',axis.text.y = element_blank(), axis.ticks.y = element_blank()))) +
    plot_annotation(title = 'Trends in French Sectoral Composition') 
  ggsave(paste0(output_dir, 'trends in french sector composition.png'))
  
  
#2)firm level decompositions -------------------------------------------------------------
  # 2.1 calculate decompositions  -----------------------------------------------
  ## import data 
  
    growth_or_realloc<-"growth"
    extensive_margin_data  = readRDS('firm_level_ex_margin.rds')
    intensive_margin_data   = readRDS(paste0('firm_level_',growth_or_realloc, '_in_margin.rds'))
    sbs_data = readRDS(paste0('sbs_br_combined_cleaned.rds'))
    # sbs_data = readRDS(paste0('sbs_br_combined_cleaned', if (growth_or_realloc=="growth") "_growth" else "",'.rds'))
    
  ## carry out the decompositions  
    vars = c('rev', 'entrance', 'exit', 'empl')
    # vars = c('rev', 'empl') 
    
    data_frames = c('intensive_margin_data', rep('extensive_margin_data',2), 'sbs_data')
    end = 2021
    for (i in 1:4){
    # i<-1
    ## define vars / import data 
    g = paste0(vars[i], '_', growth_or_realloc); gweighted = paste0(g, '_weighted'); share= paste0(vars[i], '_share')
    g_l = paste0(g, '_l'); gweighted_l = paste0(gweighted, '_l');share_l =  paste0(share, '_l')
    shift_vars = c(g, share, gweighted)
    lag_vars = paste0(shift_vars, '_l')
    
    firm_data = get(data_frames[i])
    setorder(firm_data,firmid, year)
    firm_data = firm_data %>% select(firmid, year, birth_year, death_year, all_of(shift_vars))
    
    #define aggregates /net entry portion of decomp 
    aggregates = as.data.table(firm_data[, .(agg = sum(get(gweighted), na.rm = T)), by = year] %>% arrange(year))
    aggregates[, `:=`(agg_l = shift(agg), agg_delta = agg- shift(agg))]
    
    ## generate lagged vars 
    firm_data_l = firm_data[year<end] %>% mutate(year = year + 1) %>% select(shift_vars, everything())
    colnames(firm_data_l)[names(firm_data_l) %in% shift_vars] = lag_vars
    # firm_data = merge(firm_data, firm_data_l, all = T)
    firm_data = merge(firm_data, firm_data_l, by=c("firmid", "year", "birth_year", "death_year"),all = T)
    
    ## redefine birth/ death/ survivor 
    firm_data[, `:=`(born = !is.na(birth_year) & birth_year == year,
                     died = !is.na(death_year) & death_year < year)]
    firm_data[, survivor := !born & ! died]
    
    ## set missing data to zero 
    for (j in 1:length(shift_vars)){
      firm_data[is.na(get(shift_vars[j])), shift_vars[j] := 0]
      firm_data[is.na(get(lag_vars[j])), lag_vars[j] := 0]
    } 
    
    decompositions = firm_data[,.(
      survivor = sum(survivor*(get(gweighted)- get(gweighted_l))),
      within = sum(survivor*get(share_l)*(get(g)-get(g_l))),
      between = sum(survivor*(get(share)- get(share_l))*get(g_l)),
      cross = sum(survivor*(get(share) - get(share_l))*(get(g)-get(g_l))),
      entrance = sum(born*(get(gweighted)- get(gweighted_l))),
      exit = sum(died*(get(gweighted)- get(gweighted_l))),
      'entry/exit' = sum(born*(get(gweighted)- get(gweighted_l))) +
                    sum(died*(get(gweighted)- get(gweighted_l)))),
      by =year]
    decompositions= merge(aggregates,decompositions)
    
    ## check that the decompositions were carried out correctly 
    decompositions[,`:=`(check_1 = survivor-within-between-cross,
                         check_2 = agg_delta -(survivor + entrance + exit))]
    if (max(abs(decompositions$check_2), na.rm = T) > 1E-6|
        max(abs(decompositions$check_1), na.rm = T) > 1E-6){
      break(paste0(vars[i], " not correct"))
    }
    print(paste0(vars[i], " correct"))
    decompositions[,c('check_1', 'check_2','agg_l') := NULL]
    
    ## for extensive margin add together entrance and exit; reshape and export  
    sub_groups = colnames(decompositions)[2:ncol(decompositions)]
    if (vars[i]=='entrance'){
      entrance = decompositions 
    }else if(vars[i]== 'exit'){
      exit = decompositions
      products = merge(entrance, exit, by = 'year')
      for (j in seq_along(sub_groups)){
        x = paste0(sub_groups[j], '.x')
        y = paste0(sub_groups[j], '.y')
        products[, sub_groups[j] := get(x)+ get(y)]
        products[,c(x,y):=NULL]
      }
      products_long = melt(products, id.vars = 'year', measure.vars = sub_groups) %>%
        rename(sub_group = variable)
      saveRDS(products_long,paste0('decompositions/firm_level_prod_', growth_or_realloc, '.rds') )
    } else{
      decompositions_long = melt(decompositions, id.vars = 'year', measure.vars = sub_groups) %>%
        rename(sub_group = variable)
      saveRDS(decompositions_long, paste0('decompositions/firm_level_',vars[i],'_', growth_or_realloc, '.rds'))
    }
    }
  
  # 2.2 graph decompositions ----------------------------------------------------
    #combine output and additional cleaning 
      vars = c('empl', 'rev','prod')
      results = rbindlist(lapply(seq_along(vars),function(i){
        as.data.table(readRDS(paste0('decompositions/firm_level_', vars[i],'_', growth_or_realloc, '.rds'))) %>%
          mutate(var= vars[i])
      }))
     results[var != 'empl' & (year == 2010 & sub_group !='agg' | year == 2009), value:= NA]
     results[var == 'empl' & (year == 1995 & sub_group !='agg' | year == 1994), value:= NA]
  
     titles = c(paste0('Decomposing $\\Delta$ labor ', growth_or_realloc,' rate'),
                paste0('Decomposing $\\Delta$ product ',growth_or_realloc,' rate'), 
                paste0('Decomposing $\\Delta$ product ',growth_or_realloc,' rate'))
     subtitles= c('', '(intensive margin)', '(extensive margin)')
   
    #make each graph 
      for (i in seq_along(vars)){
       data = results[var == vars[i]]
       ylim = c(min(data$value,na.rm = T)-.01, max(data$value, na.rm = T)+.01)*100
       ybreaks = seq(floor(ylim[1]), ceiling(ylim[2]), length.out = 4)
       ybreaks = sort(c(ybreaks[abs(ybreaks)>5],0))
       
       assign(paste0(vars[i],'_delta_decomp'), ggplot(data[sub_group %in% c('agg', 'within', 'between', 'cross','entry/exit')],
               aes(x =year, y = value*100, color = sub_group, size = sub_group)) +
         geom_line() +
         scale_size_manual(values = c(1,rep(.5,4)))+
         scale_x_continuous(limits = c(min(data$year)+1,max(data$year)))+
          scale_y_continuous(labels = scales::label_percent(scale = 1), limits = ylim, breaks= ybreaks) +
         theme_classic() +theme(legend.position = 'bottom', axis.title.x = element_blank(),
          axis.ticks.x = element_blank(), axis.title.y = element_blank(),
          axis.line.x = element_blank(), legend.title = element_blank()) + 
         labs(title =  TeX(titles[i]), subtitle = subtitles[i])+ guides(size = 'none') +
         geom_hline(yintercept = 0, linetype = 'dashed') +
         scale_color_hue(labels = c('agg' = paste0(growth_or_realloc, ' rate         Decompostion:'),
                                    'within'= 'within', 'between' = 'between', 
                                       'cross' = 'cross', 'entry/exit' = 'entry/exit'))
       )
      ggsave(paste0(output_dir ,vars[i],"_", growth_or_realloc, '_delta_decomp.png'), get(paste0(vars[i],'_delta_decomp')), height = 5.33, width = 9)
  }
   
#3) decomposition of growth by product status (new, existing, etc) -------------------------
## import product data 

prodfra_or_pcc8<-"prodfra"
only_prodfra_in_prodcom<-FALSE
filter<-paste0(if(prodfra_or_pcc8=="prodfra") "10_digit" else if(prodfra_or_pcc8=="pcc8") "8_digit" else prodfra_or_pcc8, 
              "_",
              if(only_prodfra_in_prodcom) "only_prodfra_in_prodcom" else "all_prodfra")
exclude_industries<-TRUE
filter<-paste0(filter, if(exclude_industries) "_exclude_industries" else "_not_exclude_industries")


## import data and generate potential firm-year level observations
product_data<-readRDS(paste0("product_level_growth_", filter,  "_.RDS"))
     
     
product_data = readRDS('product_level_reallocation.rds')

## generate weighted growth metric 
product_data[, rev_growth_weighted := rev_growth * within_economy_rev_share]
product_data[, agg := sum(rev_growth_weighted), by = year]

## carry out decomposition 
sub_groups = c('first_introduction', 'reintroduced', 'discontinued', 'incumbent')
sub_groups_g = paste0(sub_groups, '_growth')
for (i in seq_along(sub_groups)){
  product_data[, sub_groups_g[i]:= sum(rev_growth_weighted * get(sub_groups[i])), by = year]
}
growth_decomposition = unique(product_data %>% select(year,agg,sub_groups_g))

## check decomposition results 
growth_decomposition[, check := agg- rowSums(.SD, na.rm = T), .SDcols = sub_groups_g]
if(max(abs(growth_decomposition$check))>1e-12){
  stop('decomp not correct')
}
growth_decomposition[, check:=NULL]

## output decomposition results 
colnames(growth_decomposition)[3:6] = sub_groups
growth_decomp_long = melt(growth_decomposition, id.vars = 'year', measure.vars = c('agg',sub_groups))
colnames(growth_decomp_long) = c('year', 'subset', 'growth_rate')
saveRDS(growth_decomp_long, 'decompositions/growth_rate_decomp.rds')


#4) product-HHI analysis ----------------------------------------------------------

## import product data 

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


HHI_graph = product_data[, .(HHI = mean(HHI, na.rm = T), rev = sum(rev, na.rm = T)), by =.(firmid, year)]
HHI_graph = HHI_graph[, .('unweighted' = mean(HHI, na.rm = T), 'rev. weighted' = sum(HHI*rev/sum(rev,na.rm = T),na.rm= T)), by = year]
HHI_graph = melt(HHI_graph, id.vars = 'year', measure.vars = c('unweighted',  'rev. weighted')) %>% rename(type = variable) 

ggplot(HHI_graph, aes(x = year, y = value*100, color = type)) +
  geom_line() + scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  scale_x_continuous(breaks =  seq(2009, 2021, by = 3)) +  
  labs(title = "Within Firm Product HHI", x = element_blank(),  y = "HHI value", color = 'type') +
  theme_classic() +theme(legend.position = 'bottom')
ggsave(paste0(output_dir, 'HHI_over_time.png'), height = 5.33, width = 9)

#5) emergence of superstar firms  -------------------------------------------
end = 2021
age_cutoff = 3

## identify number and share of firms hat reach supserstar cutoff within age cutoff 
sbs_data = readRDS('sbs_br_combined_cleaned.rds') %>% select(firmid, year, birth_year, nq, NACE_BR)
# sbs_data = readRDS('sbs_br_combined_cleaned.rds', select = c('firmid', 'year', 'birth_year', 'nq', 'NACE_BR'))

sbs_data[, market_share := nq/sum(nq, na.rm = T), by = .(year, NACE_BR)]

superstar_cutoffs = c(.001, .005, .01)
temp = sbs_data[year - birth_year <= age_cutoff & birth_year <= end-age_cutoff]
superstar_analysis=rbindlist(lapply(seq_along(superstar_cutoffs), function(i){
  temp = temp[, .(superstar = max(market_share)>superstar_cutoffs[i]), by = .(firmid, birth_year)]
  temp = temp[, .(count = sum(superstar, na.rm = T), share = sum(superstar, na.rm = T)/.N), by = birth_year]
  temp = temp %>% mutate('Market Share Req' = paste0(superstar_cutoffs[i]*100,'%')) %>% rename(cohort = birth_year)
}))
superstar_analysis = superstar_analysis[cohort>1995]

## graph results 
number= ggplot(superstar_analysis, aes(x =cohort, y= count, color = `Market Share Req`)) + geom_line() +
  scale_y_continuous(limits = c(0, max(superstar_analysis$count))) +
  theme(legend.position = 'bottom',axis.title.y = element_blank()) + labs(title = 'Number of young superstar firms')

share = ggplot(superstar_analysis, aes(x =cohort, y= share, color = `Market Share Req`)) + geom_line() +
  scale_y_continuous(limits = c(0, max(superstar_analysis$share))) +
  theme(legend.position = 'none',axis.title.y = element_blank()) + labs(title = 'Share of Cohort')

superstar_combined = number + share
superstar_combined+ plot_annotation(title = 'Declines in Young Superstar Firms')
ggsave(paste0(output_dir, 'superstar_growth_overtime.png'), height = 5.33, width = 9)



#6) plot reallocation rate comparisons across types of firms  ------------
## import the data 
nace_DEFind <- fread("C:/Users/Public/1. Microprod/1. Microprod-H2020/NEW_INFRASTRUCTURE/Infra/MetaData/nace_DEFind.conc", colClasses = c('character'))
growth_or_realloc<-"reallocation"
combined_data = readRDS(paste0('combined_sbs_br_prodcom_data', if (growth_or_realloc=="growth") "_growth" else "",'.rds'))

combined_data<-merge(combined_data, nace_DEFind, by.x = "NACE_BR", by.y="nace", all.x=T)
combined_data<-combined_data %>% mutate(DEFind=ifelse(is.na(DEFind), "NA", DEFind))

# combined_data =  readRDS('combined_sbs_br_prodcom_data.rds')

## prepare data for graphing -----------------------------------------------
    division_vars = c('full_sample','in_prodcom', 'sector_labor_bucket','high_tech', 'superstar','young')
    division_vars = c("full_sample")
    interest_vars = c('empl', "rev", "entrance", "exit")
    
    # allow for faster testing
      test = F
      if(test){
        set.seed(123)
        combined_data_test= combined_data[sample(nrow(combined_data), ceiling(.01*nrow(combined_data))),]
      }


    ## prepare data for graphing 
    setorder(combined_data,firmid, year)
    reall_or_growth_output = rbindlist(lapply(seq_along(division_vars), function(i){
      division = division_vars[i]
      if (test){temp_og = combined_data_test[!is.na(get(division))]
      }else{ temp_og = combined_data[!is.na(get(division))]}
      output= rbindlist(lapply(seq_along(interest_vars), function(j){
        ##define vars for this iteration
        var= interest_vars[j]; reall_or_growth = paste0(var,'_', growth_or_realloc); share = paste0(var,'_share')
        temp = temp_og[!is.na(get(reall_or_growth)) & !is.na(get(share))]
        print(paste0('division: ', division, '; var: ', var))
        ## carry out the decomposition --> what portion of reallocation comes from each category
        temp[, share := get(share)/sum(get(share), na.rm = T), by = year]
        temp[, reall_or_growth_weighted := share*get(reall_or_growth)]
        decomposition = temp[, .(sum(reall_or_growth_weighted, na.rm = T),
                                 version = 'decomp'), by = .(get(division), year)]
        
        ## carry out comparison --> how do the reall_or_growth rates compare between categories 
        temp[, share := NULL]
        temp[, share := get(share)/sum(get(share), na.rm = T), by = .(get(division), year)]
        temp[, reall_or_growth_weighted := share*get(reall_or_growth)]
        
        comparison  = temp[, .(sum(reall_or_growth_weighted, na.rm = T),
                               version = 'comparison'), by = .(get(division), year)]
        #reset share variable and combine two results 
        temp[,share := NULL]
        output = rbind(comparison, decomposition) %>% mutate(division = division, var = var) %>%
          rename(reall_or_growth_rate = V1, division_cat = get)
      }))
    })) 
    
    ### cleanup output 
    reall_or_growth_output = reall_or_growth_output[!(division == 'full_sample' & version =='decomp')]
    ### only include analysis of in prodcom for employment variable, and when we have prodcom data 
    reall_or_growth_output = reall_or_growth_output[division != 'in_prodcom' | (var == 'empl' & year >2008)]
    
    ### restrict prodcom reall_or_growth metrics to 2010 +, sbs metrics to '95 + 
    reall_or_growth_output = reall_or_growth_output[(var=='empl' & year>=1995) | (var!='empl' & year >= 2010)]
    
    ### restrict young analysis to after 1998
    reall_or_growth_output = reall_or_growth_output[division != 'young' | year > 1998]
    
    ### handle extensive margin 
    extensive<-dcast(reall_or_growth_output[var=='entrance' | var == 'exit'],
                     division_cat + year + version + division ~ var,
                     value.var = paste0('reall_or_growth_rate')) %>%
      mutate(reall_or_growth_rate = entrance + exit, var = 'product') 
    
    reall_or_growth_output= rbind(reall_or_growth_output[var!='entrance' & var!='exit'],
                               dcast(reall_or_growth_output[var=='entrance' | var == 'exit'],
                                     division_cat + year + version + division ~ var,
                                     value.var = paste0('reall_or_growth_rate')) %>%
                                 mutate(reall_or_growth_rate = entrance + exit, var = 'product') %>% 
                                 select(-c(entrance, exit)))


  ## plot graphs  -------------------------------------------------
    ## baseline ----------------------------------------------------------------
     # labor = reall_or_growth_output[var == 'empl' & (division =='full_sample'|
     #                            (division=='in_prodcom' & division_cat ==T & version =='comparison'))]
    labor = reall_or_growth_output[var == 'empl' & (division =='full_sample')]
    
     product= reall_or_growth_output[(var =='rev' | var =='product') & division == 'full_sample']
    
     ylim = c(0, max(labor$reall_or_growth_rate, product$reall_or_growth_rate, na.rm= T))
     ylim = c(0,0.42)
     

    labor_graph =ggplot(labor, aes(x =year , y = reall_or_growth_rate, color = division)) + geom_line(color="chartreuse4") + 
        theme(legend.position = 'bottom', axis.title.x = element_blank()) + labs(color = "Sample", y= paste0(growth_or_realloc, ' rate'), title ='Employee-Level Reallocation') + 
        scale_color_hue(labels = c('full_sample'='FARE/FICUS','in_prodcom' = 'Prodcom')) +  
        scale_y_continuous(labels = scales::label_percent(scale = 100), limits = ylim) 
    ggsave(paste0(output_dir, paste0('baseline_labor_', growth_or_realloc ,'.png')), labor_graph,  height = 5.33, width = 7)
    

        
    product_graph = ggplot( if(growth_or_realloc=="growth") {product %>% filter(var=="rev")} else {product} , aes(x =year , y = reall_or_growth_rate, color = var)) + geom_line() + 
        theme(legend.position = 'bottom', axis.title.x = element_blank()) + labs(color = "Margin", y= paste0(growth_or_realloc, ' rate'), title ='Product-Level Reallocation') + 
        scale_color_hue(labels = c('product'='extensive','rev' = 'intensive')) +  
        scale_y_continuous(labels = scales::label_percent(scale = 100), limits = ylim) 
    ggsave(paste0(output_dir, paste0('baseline_product_', growth_or_realloc ,'.png')), product_graph,  height = 5.33, width = 7)
    
      
      # labor_graph + product_graph + plot_annotation(title = 'French Reallocation Rates (1995-2021)')
    labor_graph + product_graph +plot_annotation(title = paste0('French ',growth_or_realloc, ' rates (1995-2021)')) 
      ggsave(paste0(output_dir, paste0('baseline_', growth_or_realloc ,'.png')), height = 5.33, width = 9)
      rm(labor_graph, product_graph)
    
    ## plot sector x labor_bucket -------------------------------------------------
    manufacturing_graph = reall_or_growth_output[division =='sector_labor_bucket'] %>%
      separate(division_cat, into= c('sector', 'employees'), sep =':') %>% filter(employees!="NA") %>%
        mutate(employees = factor(employees, levels = c("<10","10-50","50-250","250+" )))
    setorder(manufacturing_graph,version,sector, employees,var, year)
   
        sectors = c('manufacturing', 'services', 'other sector')
        graph_vars = c('empl', 'rev', if(growth_or_realloc=="reallocation") 'product')
        versions = c('comparison', 'decomp'); k = 2
        plot_titles = c(paste0('Labor ', growth_or_realloc, ' rate'), paste0('Product ', growth_or_realloc, ' rate (intensive margin)'),
                        paste0('Product ', growth_or_realloc, ' rate (extensive margin)'))
        subtitle = 'Decomposing By Sector and Firm Size'
        breaks= c(4,3,3)
  
    for(i in seq_along(graph_vars)){
      y_lim = c(min(manufacturing_graph %>% filter(version==versions[k] & var == graph_vars[i]) %>% pull(reall_or_growth_rate) ), max(manufacturing_graph %>% filter(version==versions[k] & var == graph_vars[i]) %>% pull(reall_or_growth_rate) ))
      # y_lim = c(0, max(manufacturing_graph[ version == versions[k] & var == graph_vars[i]] %>% pull(reallocation_rate) ))
    for (j in seq_along(sectors)){
    
   # assign(paste0(sectors[j],'_g'),ggplot(manufacturing_graph[version == versions[k] & sector == sectors[j]  & var == graph_vars[i]],
   assign(paste0(sectors[j],'_g'),ggplot(manufacturing_graph %>% filter(version == versions[k] & sector == sectors[j]  & var == graph_vars[i]),
                                 aes(x = year, y = reall_or_growth_rate, color = employees)) + geom_line() +
      scale_color_brewer(palette = 'RdYlGn') +labs(title = sectors[j], y = paste0(growth_or_realloc, ' rate')) +
        theme(title = element_text(size =  9)) + scale_x_continuous(breaks = scales::pretty_breaks(n =breaks[i]))+
        scale_y_continuous(labels = scales::label_percent(scale = 100), limits = y_lim)
   )
    }
      assign(paste0('graph_', graph_vars[i]),
             ((manufacturing_g +theme(axis.title.x  = element_blank(), legend.position = 'none')) |
                (services_g  + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = 'none')) |
                (`other sector_g` + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = 'right'))) +
               plot_annotation(title = plot_titles[i], subtitle = subtitle))
      # ggsave(paste0('C:/Users/NEWPROD_J_DIAZ-AC/Documents/Reallocation/3 output/graphs/manufac_decomp_',graph_vars[i],'.png'),
      print(paste0(output_dir,graph_vars[i],'.png'))
      ggsave(paste0(output_dir,graph_vars[i],'.png'),
           get(paste0('graph_', graph_vars[i])), width=10, height = 5)
      }


    ## make young, high tech, superstar graphs --------------------------------------------------
    division_titles = c(paste0('Changes in ', growth_or_realloc, ' by technology level'),
                        paste0('Changes in ', growth_or_realloc, ' by superstar status'),
                        paste0('Changes in ', growth_or_realloc, ' by firm age'))
      legend_title = c('', 'market share', '')
    division_vars = c('high_tech', 'superstar','young')
    
    graph_vars = c('empl', 'rev', if(growth_or_realloc=="reallocation") 'product')
    titles = c('Employment', 'Product (intensive)', "Product (extensive)")
    
    versions = c('comparison', 'decomp')
    axis_titles = c(paste0(growth_or_realloc, ' rate'), paste0('weighted ', growth_or_realloc, ' rate'))
    x_lim_list = list(ceiling(seq(1995,2020, length.out = 4))
                      ,ceiling(seq(2010,2020, length.out = 4)), ceiling(seq(2010,2020, length.out = 4)))
    
    reall_or_growth_output[division== 'high_tech', division_cat_new :=ifelse(division_cat== "TRUE", "high tech", 'low tech')]
    reall_or_growth_output[division== 'superstar', division_cat_new :=ifelse(division_cat== "TRUE", "> 1%", '< 1 %')]
    reall_or_growth_output[division== 'young', division_cat_new :=ifelse(division_cat== "TRUE", "young (< 5 years)", 'old')]
    
    for( h in seq_along(division_vars)){
      for(j in seq_along(versions)){
        y_lim = c(min(reall_or_growth_output[version == versions[j] & division == division_vars[h]] %>%pull(reall_or_growth_rate)), max(reall_or_growth_output[version == versions[j] & division == division_vars[h]] %>%pull(reall_or_growth_rate) ))
      
        for( i in seq_along(graph_vars)){
          assign(paste0(graph_vars[i], '_g'),
                 ggplot(reall_or_growth_output[division == division_vars[h] & var == graph_vars[i] & version == versions[j]],
                        aes(x =year, y = reall_or_growth_rate, color = division_cat_new))
                 + geom_line() +  scale_y_continuous(labels = scales::label_percent(scale = 100), limits = y_lim) +
                   labs(title = titles[i], color = legend_title[h], y = axis_titles[j]) + scale_x_continuous(breaks = x_lim_list[[i]]))
          }
            ggsave(paste0(output_dir,division_vars[h],'_', versions[j],'.png'),
               ((empl_g +theme(axis.title.x  = element_blank(), legend.position = 'bottom')) |
                  (rev_g  + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = 'none')) |
                 if(growth_or_realloc=="reallocation") {(product_g + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = 'none'))}) +
                plot_annotation(title = division_titles[h]), height = 5.3, width = 9) 
            print(paste0(output_dir,division_vars[h],'_', versions[j],'.png'))
               
      }
    }
    
  
    

    ## extensive margin reallocation by entrance and exit---------------------------

    product= extensive %>% filter(division == 'full_sample') %>% select(year, entrance, exit, reall_or_growth_rate) %>%
      pivot_longer(cols = c("entrance", "exit", "reall_or_growth_rate"))
    
    
    ggplot(product %>% filter(name!="reall_or_growth_rate"), aes(x = year, y = value*100, fill = name)) +
      geom_bar(stat="identity", position="stack") + 
      geom_line(data=product %>% filter(name=="reall_or_growth_rate"), aes(x=year, y=value*100, colour="reall_or_growth_rate" ), colour="black") +
      scale_fill_manual(values=c(brewer.pal(2, name="Dark2")[1:2], "black"), labels=c("Entry", "Exit", "Aggregate"))+
      scale_y_continuous(labels = scales::label_percent(scale = 1),  breaks =  seq(0, 40, by = 10)) +
      scale_x_continuous(breaks =  seq(2010, end, by = 1)) +  
      labs(title = paste0("Decomposition of extensive margin product reallocation rate"),  x = element_blank(),  y = paste0("Reallocation rate"), fill = "Component", colour="agg") +
      theme_classic() +theme(legend.position = 'bottom')
    ggsave(paste0(output_dir, "extensive_margin_decomposition.png"), width=7, height=5)
    
    ## industry decomposition---------------------------
    
    # industry<-reall_or_growth_output %>% filter(division=="DEFind", version=="decomp")
    industry<-reall_or_growth_output %>% filter( version=="decomp" | division=="full_sample")
    
    
    top_industries<-industry %>% group_by(division_cat, var, division) %>% filter(var!="empl" & division=="DEFind") %>% summarise(reall_or_growth_rate=sum(reall_or_growth_rate, na.rm = T))
    top_industries<- top_industries %>% group_by(var, division) %>% top_n(8, reall_or_growth_rate) 
    top_industries<-unlist(top_industries["division_cat"])
    
    industry<-industry %>% mutate(DEFind=ifelse(division_cat %in% top_industries, division_cat, "Other")) %>% select(-division_cat)
    industry<-industry %>% group_by(year, DEFind, var, division) %>% summarise(reall_or_growth_rate=sum(reall_or_growth_rate, na.rm = T))
    
    # industry_DEFind<-industry %>% filter(var=="rev", division=="DEFind", DEFind!=1)
    # industry_full<-industry %>% filter(division=="full_sample", var=="rev") 
    # industry_full$DEFind<-NULL
    
    vars<-c("empl", "rev", "product")
    description<-c("Labor Reallocation", "Intensive Margin Product Reallocation", "Extensive Margin Product Reallocation")
    
    for(i in seq_along(vars)){
      assign(paste0("graph_industry_decomp_", vars[i]),
             ggplot(industry %>% filter(division=="DEFind", var==vars[i]), aes(x = year, y = reall_or_growth_rate*100, fill = DEFind)) +
               geom_bar(stat="identity", position="stack") + 
               geom_line(industry %>% filter(division=="full_sample", var==vars[i]), mapping=aes(x=year, y=reall_or_growth_rate*100), stat="identity")+
               # geom_line(data=product %>% filter(name=="reall_or_growth_rate"), aes(x=year, y=value*100, colour="reall_or_growth_rate" ), colour="black") +
               # scale_fill_brewer(palette = "Set1")+
               scale_fill_manual(values=c(brewer.pal(8, name="Set1")[1:8], "cyan3", "chartreuse3", "darkgoldenrod1", "grey", "darkgrey"),
                                 labels=c("C13-C15"="M. Clothing",
                                          "C24" = "M. B. Metals",
                                        "C18"="P. R. Media",
                                        "C19"="M. R. Petroleum",
                                        "C20"="M. Chemicals",
                                        "C25"="M. F. Metals",
                                        "C26"="M. Electronics",
                                        "C28"="M. Machinery (nec)",
                                        "C30"="M. Transport",
                                        "C31_C32"="M. Furniture+",
                                        "C33"="Repair",
                                        "NA"="NA",
                                        "Other"="Other"))+#, labels=c("Entry", "Exit", "Aggregate"))+
               # scale_fill_manual(values=c(brewer.pal(2, name="Dark2")[1:2], "black"), labels=c("Entry", "Exit", "Aggregate"))+
               scale_y_continuous(labels = scales::label_percent(scale = 1),  breaks =  seq(0, 40, by = 10), limits = c(0, max(industry$reall_or_growth_rate)*100)) +
               scale_x_continuous(breaks =  seq(1995, end, by=5)) +
               labs(title = paste0(description[i]),  x = element_blank(),  y = paste0("Reallocation rate"), fill = "Component", colour="agg") +
               theme_classic() +theme(legend.position = 'bottom')
      )
    }
    
    layout_matrix<-rbind(c(1,2), c(3,3))
    industry_decom<-grid.arrange(graph_industry_decomp_product+theme(legend.position = "none"), 
                                 graph_industry_decomp_rev+theme(legend.position = "none") + labs(y=element_blank()), 
                                 graph_industry_decomp_empl,
                                 layout_matrix=layout_matrix)
    ggsave(paste0(output_dir, "reallocation_industry_decomp8.png"), industry_decom)
    
    print(graph_industry_decomp_empl+ graph_industry_decomp_product+ graph_industry_decomp_rev)
    
    
    labs(title = paste0("Decomposition of extensive margin product reallocation rate"),  x = element_blank(),  y = paste0("Reallocation rate"), fill = "Component", colour="agg") +
      
    b<-ggplot(industry_full, aes(x = year, y = reall_or_growth_rate*100))+
    geom_line(colour="black") 
    
    graph<-a+    geom_line(data=industry_full, aes(x=year, y=reall_or_growth_rate), stat="identity") 

    print(graph)

  
# #6) plot reallocation rate comparisons across types of firms  ------------
# ## import the data 
# combined_data =  readRDS('combined_sbs_br_prodcom_data.rds')
# ## prepare data for graphing -----------------------------------------------
#     division_vars = c('full_sample','in_prodcom', 'sector_labor_bucket','high_tech', 'superstar','young')
#     #division_vars = c('sector_labor_bucket')
#     interest_vars = c('empl','rev', 'exit', 'entrance')
#     
#     # allow for faster testing
#       test = F
#       if(test){
#         set.seed(123)
#         combined_data_test= combined_data[sample(nrow(combined_data), ceiling(.01*nrow(combined_data))),]
#       }
# 
# 
#     ## prepare data for graphing 
#     setorder(combined_data,firmid, year)
#     reallocation_output = rbindlist(lapply(seq_along(division_vars), function(i){
#       division = division_vars[i]
#       if (test){temp_og = combined_data_test[!is.na(get(division))]
#       }else{ temp_og = combined_data[!is.na(get(division))]}
#       output= rbindlist(lapply(seq_along(interest_vars), function(j){
#         ##define vars for this iteration
#         var= interest_vars[j]; growth = paste0(var,'_growth'); share = paste0(var,'_share')
#         temp = temp_og[!is.na(get(growth)) & !is.na(get(share))]
#         print(paste0('division: ', division, '; var: ', var))
#         ## carry out the decomposition --> what portion of reallocation comes from each category
#         temp[, share := get(share)/sum(get(share), na.rm = T), by = year]
#         temp[, growth_weighted := share*get(growth)]
#         decomposition = temp[, .(sum(growth_weighted, na.rm = T),
#                                  version = 'decomp'), by = .(get(division), year)]
#         
#         ## carry out comparison --> how do the reallocation rates compare between categories 
#         temp[, share := NULL]
#         temp[, share := get(share)/sum(get(share), na.rm = T), by = .(get(division), year)]
#         temp[, growth_weighted := share*get(growth)]
#         
#         comparison  = temp[, .(sum(growth_weighted, na.rm = T),
#                                version = 'comparison'), by = .(get(division), year)]
#         #reset share variable and combine two results 
#         temp[,share := NULL]
#         output = rbind(comparison, decomposition) %>% mutate(division = division, var = var) %>%
#           rename(reallocation_rate = V1, division_cat = get)
#       }))
#     })) 
#     
#     ### cleanup output 
#     reallocation_output = reallocation_output[!(division == 'full_sample' & version =='decomp')]
#     ### only include analysis of in prodcom for employment variable, and when we have prodcom data 
#     reallocation_output = reallocation_output[division != 'in_prodcom' | (var == 'empl' & year >2008)]
#     
#     ### restrict prodcom reallocation metrics to 2010 +, sbs metrics to '95 + 
#     reallocation_output = reallocation_output[(var=='empl' & year>=1995) | (var!='empl' & year >= 2010)]
#     
#     ### restrict young analysis to after 1998
#     reallocation_output = reallocation_output[division != 'young' | year > 1998]
#     
#     ### handle extensive margin 
#     reallocation_output= rbind(reallocation_output[var!='entrance' & var!='exit'],
#                                dcast(reallocation_output[var=='entrance' | var == 'exit'],
#                                      division_cat + year + version + division ~ var,
#                                      value.var = 'reallocation_rate') %>%
#                                  mutate(reallocation_rate = entrance + exit, var = 'product') %>% 
#                                  select(-c(entrance, exit)))
# 
# 
#   ## plot graphs  -------------------------------------------------
#     ### baseline ----------------------------------------------------------------
#      labor = reall_or_growth_output[var == 'empl' & (division =='full_sample'|
#                                 (division=='in_prodcom' & division_cat ==T & version =='comparison'))]
#      product= reall_or_growth_output[(var =='rev' | var =='product') & division == 'full_sample']
#     
#      lim = c(0, max(labor$reall_or_growth_rate, product$reall_or_growth_rate, na.rm= T))
#     labor_graph =ggplot(labor, aes(x =year , y = reallocation_rate, color = division)) + geom_line() + 
#         theme(legend.position = 'bottom', axis.title.x = element_blank()) + labs(color = "Sample", y= 'reallocation rate', title ='Employee-Level') + 
#         scale_color_hue(labels = c('full_sample'='FARE/FICUS','in_prodcom' = 'Prodcom')) +  
#         scale_y_continuous(labels = scales::label_percent(scale = 100), limits = ylim) 
#         
#     product_graph = ggplot(product, aes(x =year , y = reallocation_rate, color = var)) + geom_line() + 
#         theme(legend.position = 'bottom', axis.title.x = element_blank()) + labs(color = "Margin", y= element_blank(), title ='Product-Level') + 
#         scale_color_hue(labels = c('product'='extensive','rev' = 'intensive')) +  
#         scale_y_continuous(labels = scales::label_percent(scale = 100), limits = ylim) 
#       
#       graph=labor_graph + product_graph +plot_annotation(title = 'French Reallocation Rates (1995-2021)')
#       ggsave(paste0(output_dir, 'baseline_reallocation.png'), graph,height = 5.33, width = 9)
#       rm(labor_graph, product_graph)
#     
#     ## plot sector x labor_bucket -------------------------------------------------
#     manufacturing_graph = reallocation_output[division =='sector_labor_bucket'] %>%
#       separate(division_cat, into= c('sector', 'employees'), sep =':') %>% filter(employees!="NA") %>%
#         mutate(employees = factor(employees, levels = c("<10","10-50","50-250","250+" )))
#     setorder(manufacturing_graph,version,sector, employees,var, year)
#    
#         sectors = c('manufacturing', 'services', 'other sector')
#         graph_vars = c('empl', 'rev', 'product')
#         versions = c('comparison', 'decomp'); k = 2
#         plot_titles = c('Labor Reallocation Rate', 'Product Reallocation Rate (Intensive Margin)',
#                         'Product Reallocation Rate (Extensive Margin)')
#         subtitle = 'Decomposing By Sector and Firm Size'
#         breaks= c(4,3,3)
#   
#     for(i in seq_along(graph_vars)){
#       y_lim = c(0, max(manufacturing_graph[version == versions[k] & var == graph_vars[i]] %>%pull(reallocation_rate) ))
#     for (j in seq_along(sectors)){
#     
#    assign(paste0(sectors[j],'_g'),ggplot(manufacturing_graph[version == versions[k] & sector == sectors[j]  & var == graph_vars[i]],
#                      aes(x = year, y = reallocation_rate, color = employees)) + geom_line() +
#       scale_color_brewer(palette = 'RdYlGn') +labs(title = sectors[j], y = 'reallocation rate') +
#         theme(title = element_text(size =  9)) + scale_x_continuous(breaks = scales::pretty_breaks(n =breaks[i]))+
#         scale_y_continuous(labels = scales::label_percent(scale = 100), limits = y_lim)
#    )
#     }
#       assign(paste0('graph_', graph_vars[i]),
#              ((manufacturing_g +theme(axis.title.x  = element_blank(), legend.position = 'none')) |
#                 (services_g  + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = 'none')) |
#                 (`other sector_g` + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = 'right'))) +
#                plot_annotation(title = plot_titles[i], subtitle = subtitle))
#     ggsave(paste0('C:/Users/NEWPROD_J_DIAZ-AC/Documents/Reallocation/3 output/graphs/manufac_decomp_',graph_vars[i],'.png'),
#            get(paste0('graph_', graph_vars[i])), width=10, height = 5)
#       }
# 
# 
#     ## make young, high tech, superstar graphs --------------------------------------------------
#     division_titles = c('Changes in Reallocation by Technology Level',
#                         'Changes in Reallocation by Superstar status',
#                         'Changes in Reallocation by Firm Age')
#       legend_title = c('', 'market share', '')
#     division_vars = c('high_tech', 'superstar','young')
#     
#     graph_vars = c('empl', 'rev', 'product')
#     titles = c('Employment', 'Product (intensive)', "Product (extensive)")
#     
#     versions = c('comparison', 'decomp')
#     axis_titles = c('reallocation rate', 'weighted reallocation rate')
#     x_lim_list = list(ceiling(seq(1995,2020, length.out = 4))
#                       ,ceiling(seq(2010,2020, length.out = 4)), ceiling(seq(2010,2020, length.out = 4)))
#     
#     reallocation_output[division== 'high_tech', division_cat_new :=ifelse(division_cat== "TRUE", "high tech", 'low tech')]
#     reallocation_output[division== 'superstar', division_cat_new :=ifelse(division_cat== "TRUE", "> 1%", '< 1 %')]
#     reallocation_output[division== 'young', division_cat_new :=ifelse(division_cat== "TRUE", "young (< 5 years)", 'old')]
#     
#     for( h in seq_along(division_vars)){
#       for(j in seq_along(versions)){
#         y_lim = c(0, max(reallocation_output[version == versions[j] & division == division_vars[h]] %>%pull(reallocation_rate) ))
#       
#         for( i in seq_along(graph_vars)){
#           assign(paste0(graph_vars[i], '_g'),
#                  ggplot(reallocation_output[division == division_vars[h] & var == graph_vars[i] & version == versions[j]],
#                         aes(x =year, y = reallocation_rate, color = division_cat_new))
#                  + geom_line() +  scale_y_continuous(labels = scales::label_percent(scale = 100), limits = y_lim) +
#                    labs(title = titles[i], color = legend_title[h], y = axis_titles[j]) + scale_x_continuous(breaks = x_lim_list[[i]]))
#           }
#             ggsave(paste0('C:/Users/NEWPROD_J_DIAZ-AC/Documents/Reallocation/3 output/graphs/',division_vars[h],'_', versions[j],'.png'),
#                ((empl_g +theme(axis.title.x  = element_blank(), legend.position = 'bottom')) |
#                   (rev_g  + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = 'none')) |
#                   (product_g + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = 'none'))) +
#                 plot_annotation(title = division_titles[h]), height = 5.3, width = 9) 
#                
#       }
#     }
#     
#   
#  
#     
#   
# 
#   
# 
# 
# 
# 
# 
#     
# 
#     
#     
#     
#     
#     
#     
#     
#     
#     
#     
#     
#     
#     
#     
#     
#     
#     
#     
#7) product-HHI analysis by size--------------
comparison_dataset = readRDS('combined_sbs_br_prodcom_data.rds')
comparison_dataset<-comparison_dataset %>% filter(in_prodcom) %>% select(firmid, year, empl)

prodfra_or_pcc8<-"prodfra"
only_prodfra_in_prodcom<-FALSE
filter<-paste0(if(prodfra_or_pcc8=="prodfra") "10_digit" else if(prodfra_or_pcc8=="pcc8") "8_digit" else prodfra_or_pcc8, 
               "_",
               if(only_prodfra_in_prodcom) "only_prodfra_in_prodcom" else "all_prodfra")
exclude_industries<-TRUE
filter<-paste0(filter, if(exclude_industries) "_exclude_industries" else "_not_exclude_industries")

## import data and generate potential firm-year level observations
product_data<-readRDS(paste0("product_level_growth_", filter,  "_.RDS")) %>% group_by(firmid, year) %>% summarise(HHI=mean(HHI, na.rm = T), rev=sum(rev, na.rm=T))
product_data<-merge(product_data, comparison_dataset, by=c("firmid", "year"), all.x=T)
product_data<-as.data.table(product_data)


# var<-"rev"
# for(yr in 2009:2021){
#   temp<-product_data %>% filter(year==yr)
#   kilotiles <- quantile(temp[[var]], probs=seq(0, 1, by=0.001), na.rm=T)
#   print(paste0(yr, ": top quintile: ", round(kilotiles["80.0%"], 2) , 
#         ": top decile: ", round(kilotiles["90.0%"], 2), 
#         ": top percentile: ", round(kilotiles["99.0%"], 2), 
#         ": top 0.1%: ", round(kilotiles["99.9%"],2)))
# }

# Cutoffs correspond roughly to the top 20%, 10%, 1% and 0.1% of employment
empl_cutoffs<-c(40, 80, 600, 3000)
# for (i in seq_along(empl_cutoffs)) {
#   var<-paste0("empl_cut_", empl_cutoffs[i])
#   product_data[[var]]<-product_data$empl>empl_cutoffs[i]
# }

HHI_size_analysis_empl=rbindlist(lapply(seq_along(empl_cutoffs), function(i){
  temp = product_data %>% filter(empl>empl_cutoffs[i])
  temp = temp[, .(HHI = mean(HHI, na.rm = T), rev = sum(rev, na.rm = T)), by =.(firmid, year)]
  temp = temp[, .(unweighted = mean(HHI, na.rm = T), weighted = sum(HHI*rev/sum(rev,na.rm = T),na.rm= T)), by = year]
  temp = temp %>% mutate('Empl. Req' = paste0(">", empl_cutoffs[i]))
}))
HHI_size_analysis_empl$`Empl. Req`<-factor(HHI_size_analysis_empl$`Empl. Req`, levels = unique(HHI_size_analysis_empl$`Empl. Req`))


empl_unweighted<-ggplot(HHI_size_analysis_empl, aes(x =year, y=unweighted, color = `Empl. Req`)) + 
  geom_line()+
  scale_x_continuous(breaks =  seq(2009, 2021, by = 1)) +  
  scale_y_continuous(limits = c(0, 0.8)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90), legend.position = 'bottom',axis.title.y = element_blank()) + labs(title = 'Unweighted',  y="HHI")
empl_weighted<-ggplot(HHI_size_analysis_empl, aes(x =year, y=weighted, color = `Empl. Req`)) + 
  geom_line()+
  scale_x_continuous(breaks =  seq(2009, 2021, by = 1)) +
  scale_y_continuous(limits = c(0, 0.8)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90), legend.position = 'bottom',axis.title.y = element_blank()) + labs(title = 'Revenue Weighted', y="HHI")
# HHI_size<-empl_unweighted | empl_weighted



HHI_size<-((empl_unweighted +theme(axis.title.x = element_blank(), legend.position = 'none')) |
    (empl_weighted + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = 'right'))) +
  plot_annotation(title = "HHI index by firm size")
ggsave(paste0(output_dir, "HHI_size.png"), HHI_size,  height = 5, width=10)




rev_cutoffs<-c(4000, 10000, 100000, 1000000)

HHI_size_analysis_rev=rbindlist(lapply(seq_along(rev_cutoffs), function(i){
  temp = product_data %>% filter(rev>rev_cutoffs[i])
  temp = temp[, .(HHI = mean(HHI, na.rm = T), rev = sum(rev, na.rm = T)), by =.(firmid, year)]
  temp = temp[, .(unweighted = mean(HHI, na.rm = T), weighted = sum(HHI*rev/sum(rev,na.rm = T),na.rm= T)), by = year]
  temp = temp %>% mutate(rev_req = paste0(">", rev_cutoffs[i]))
}))
HHI_size_analysis_rev$rev_req<-factor(HHI_size_analysis_rev$rev_req, levels = unique(HHI_size_analysis_rev$rev_req))


rev<-ggplot(HHI_size_analysis_rev, aes(x =year, y=unweighted, color = rev_req)) + 
  geom_line()+
  scale_y_continuous(limits = c(0,1)) +
  theme(legend.position = 'bottom',axis.title.y = element_blank()) + labs(title = 'Revenue')





HHI_graph = product_data[, .(HHI = mean(HHI, na.rm = T), rev = sum(rev, na.rm = T)), by =.(firmid, year)]
HHI_graph = HHI_graph[, .('unweighted' = mean(HHI, na.rm = T), 'rev. weighted' = sum(HHI*rev/sum(rev,na.rm = T),na.rm= T)), by = year]
HHI_graph = melt(HHI_graph, id.vars = 'year', measure.vars = c('unweighted',  'rev. weighted')) %>% rename(type = variable) 

ggplot(HHI_graph, aes(x = year, y = value*100, color = type)) +
  geom_line() + scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  scale_x_continuous(breaks =  seq(2009, 2021, by = 3)) +  
  labs(title = "Within Firm Product HHI", x = element_blank(),  y = "HHI value", color = 'type') +
  theme_classic() +theme(legend.position = 'bottom')
ggsave(paste0(output_dir, 'HHI_over_time.png'), height = 5.33, width = 9)


temp = sbs_data[year - birth_year <= age_cutoff & birth_year <= end-age_cutoff]
superstar_analysis=rbindlist(lapply(seq_along(superstar_cutoffs), function(i){
  temp = temp[, .(superstar = max(market_share)>superstar_cutoffs[i]), by = .(firmid, birth_year)]
  temp = temp[, .(count = sum(superstar, na.rm = T), share = sum(superstar, na.rm = T)/.N), by = birth_year]
  temp = temp %>% mutate('Market Share Req' = paste0(superstar_cutoffs[i]*100,'%')) %>% rename(cohort = birth_year)
}))
superstar_analysis = superstar_analysis[cohort>1995]








    
