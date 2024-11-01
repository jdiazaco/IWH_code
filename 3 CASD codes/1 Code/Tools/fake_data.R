
#DESCRIPTION 
# For each dataset I use in my analysis; I generate a simulated version that fits the criteria for export (non identifiable, no distributional info for groups of less than four)
# To accomplish this I proceeed in several steps 
# 1) I identify groups of observations that are larger than the cutoff threshold for cell cize (IE greater than 4 firms)
# 2) I randomly assign observations in the simulated data to a group identified in step 1
# 3) For the remainder of variables that not used to identify groups, I generate group level summary statistics (eg mean and SD)
#    and then use those summary statistics to generate simulated values 
# 
# Firm ids are randomly generated (not using any of the original siren ids) just to make the variables in the simulated data
# align with those in the original data. 
# 
# For datasets that were already aggregated (datasets 10a-12 which present industry summary stats)
# I drop cells that are made up less than five firm_id_threshold


# Setup -------------------------------------------------------------------
source("C:/Users/NEWPROD_J_DIAZ-AC/Documents/Reallocation/6 Publish/1 Code/Main.R")
output_dir<-paste0("C:/Users/NEWPROD_J_DIAZ-AC/Documents/Reallocation/6 Publish/Dummy infrastructure/2 Data/")
output_dir_creator(output_dir)

packages = c('data.table', 'haven', 'readxl', 'openxlsx', 'stringr', 'readr', 'dplyr',
             'tidyverse', 'janitor', 'Matrix','parallel', 'bigmemory','bit64','tmvtnorm'
)
lapply(packages, function(package){
  tryCatch({library(package,character.only = T)}, error = function(cond){
    install.packages(package); library(package, character.only = T)
  })})
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("C:/Users/Public/Documents/Big data Project/4) exports/07-31-export/2) Code/00_helper_functions.R")
# raw_dir = 'C:/Users/Public/1. Microprod/0. Raw data processing/Data/'

highest_year<-2021
n_firms<-14000

# 00) parameters / helper functions---------------------------------------------------------
firm_id_threshold = 4; set.seed(1)

simulate_discrete_vars = function(data, data_dummy, group_vars, interest_vars){
  
  # use the group vars to generate unique ids fro each group
  group_keys = data[, ..group_vars] %>% unique() %>% mutate(group_code = 1:nrow(.))
  data = merge(data, group_keys, by=group_vars)
  data_dummy = merge(data_dummy, group_keys, by=group_vars)
  
  # for each group use the joint empirical distribution of values to generate 
  data_dummy = lapply(1:nrow(group_keys), function(i){
    temp_dummy = data_dummy[group_code == i]; num_in_dummy = nrow(temp_dummy)
    if (num_in_dummy > 0){
      temp = data[group_code == i, ..interest_vars]; num_in_temp = nrow(temp);
      temp_dummy = cbind(temp_dummy,temp[sample(1:num_in_temp, num_in_dummy, T)])
    }
    return(temp_dummy)
  }) %>% rbindlist(fill =T, use.names = T)
  data[, group_code := NULL]; data_dummy[, group_code := NULL]
  return(data_dummy)
}

simulate_continuous_vars = function(data, data_dummy, group_vars, interest_vars){
  
  # use the group vars to generate unique ids fro each group
  group_keys = data[, ..group_vars] %>% unique() %>% mutate(group_code = 1:nrow(.))
  data = merge(data, group_keys, by=group_vars)
  data_dummy = merge(data_dummy, group_keys, by = group_vars)
  
  # generate the mins and maxes for the whole dataset, these will serve as bounds
  # for the simulation draws
  mins = apply(data[,..interest_vars],2, NA_min); maxes = apply(data[,..interest_vars],2, NA_max)
  
  # for each group generate the multivariate normal distribution of the variables of interest 
  data_dummy = lapply(1:nrow(group_keys), function(i){
    # temp_dummy = data_dummy[group_code == i]; num_in_dummy = nrow(temp_dummy)
    temp_dummy = data_dummy %>% filter(group_code == i); num_in_dummy = nrow(temp_dummy)
    
    if (num_in_dummy > 0){
      temp_dummy = tryCatch({
        temp = data[group_code == i, ..interest_vars] 
        ## ensure covariance matrix is positive definite
        noise = rnorm(nrow(temp)*length(interest_vars),0, 1e-6) %>% matrix(., nrow = nrow(temp))
        noise = noise - min(noise);temp = temp+ noise
        cov_matrix = cov(temp, use = 'pairwise.complete.obs') %>% nearPD()
        cov_matrix = cov_matrix$mat
        
        # simulate data 
        draws = rtmvnorm(num_in_dummy, colMeans(temp, na.rm = T),cov_matrix,mins,maxes)
        temp_dummy[,(interest_vars) := as.data.table(draws)] 
        return(temp_dummy)
      }, error = function(e){return(temp_dummy)})
    }
    return(temp_dummy)
  }) %>% rbindlist(fill =T, use.names = T)
  
  return(data_dummy)
}


# 1a) Prodcom --------------------------------------------

# Set parameters for prodfra-pcc8 and excluded industries
prodfra_or_pcc8<-"prodfra"
only_prodfra_in_prodcom<-FALSE
parameters(prodfra_or_pcc8, only_prodfra_in_prodcom)

data<-readRDS(paste0("product_data_", filter_indicator,  "_.RDS"))

# Sample firms out of prodcom
set.seed(123)
unique_firmids<-unique(data$firmid)
sample_size<-ceiling(0.5*length(unique_firmids))
sample_firmids<-sample(unique_firmids, sample_size)
data<-data[firmid %in% sample_firmids]

data[, count:= .N, by = .(prodfra_plus, year)]; data = data[count > firm_id_threshold]; data[, count :=NULL ]
num_firms = data[!duplicated(firmid)] %>% nrow(); num_data_points = nrow(data)
year_nace = data[, .(prodfra_plus, year)]

# for each obs. randomly assign an id and then draw from the joint distrib of year-NACE industry combinations
# data_dummy = data.table(firmid = sample(1:num_firms, num_data_points,  T)) %>%
#   cbind(.,year_nace[sample(1:num_data_points, num_data_points, T)]) %>% unique()


entry_year_dist <- data %>% group_by(firmid, prodfra_plus) %>%
  summarize(entry_year=min(year)) %>% group_by(prodfra_plus) %>%
  count(entry_year)  %>% mutate(prob=n/sum(n))

exit_year_dist <- data %>% group_by( firmid, prodfra_plus) %>%
  summarize(exit_year=max(year)) %>% group_by(prodfra_plus) %>%
  count(exit_year)  %>% mutate(prob=n/sum(n))

gap_dist <- data %>% group_by(firmid, prodfra_plus) %>% arrange(firmid, prodfra_plus, year) %>%
  summarize(gaps=sum(diff(year)>1), n=max(year)-min(year), share=gaps/n) %>% group_by(prodfra_plus) %>%
  count(gaps) %>% mutate(prob=n/sum(n))

duration_dist <- data %>% group_by(firmid, prodfra_plus) %>%
  summarize(duration=max(year)-min(year)+1, .groups="drop") %>% group_by(prodfra_plus) %>%
  count(duration) %>% mutate(prob=n/sum(n))

product_dist <- data %>% group_by(firmid) %>%
  summarize(n_products=n_distinct(prodfra_plus)) %>% 
  count(n_products) %>% mutate(prob=n/sum(n))

prodfra_dist <- data %>% group_by(prodfra_plus) %>%
  summarize(n_firms=n_distinct(firmid)) %>% 
  mutate(prob=n_firms/sum(n_firms))




set.seed(789)
data_dummy<-data.frame()


for(i in 1:num_firms){
  print(i)
  # i<-21
  
  n_products<-sample(product_dist$n_products, size=1, prob=product_dist$prob)
  
  for(j in 1:n_products){
    # j<-3
    product<-sample(prodfra_dist$prodfra_plus, size=1, prob=prodfra_dist$prob)
      
    entry_year<-tryCatch({sample(entry_year_dist$entry_year[entry_year_dist$prodfra_plus==product],
                                size=1,
                                prob=entry_year_dist$prob[entry_year_dist$prodfra_plus==product])}, error= function(e)sample(2009:2021, 1))
    print("entry_year")
    exit_year<-tryCatch({sample(exit_year_dist$exit_year[exit_year_dist$prodfra_plus==product],
                      size=1,
                      prob=exit_year_dist$prob[exit_year_dist$prodfra_plus==product])}, error= function(e)sample(2009:2021, 1))
    print("exit_year")


    
    if(entry_year>=exit_year){exit_year<-entry_year + sample(duration_dist$duration, 1, replace=T, duration_dist$prob)}
    if(exit_year>=highest_year){exit_year<-highest_year}
    
    
    num_gaps<-sample(gap_dist$gaps[gap_dist$prodfra_plus==product],size=1, prob=gap_dist$prob[gap_dist$prodfra_plus==product])
    print("num_gaps")
    
    
    observed_years<-seq(entry_year, exit_year)
    if(num_gaps>0 && length(observed_years) >2){
      gap_years<-sample(observed_years[-c(1, length(observed_years))], replace=T, num_gaps)
      
      observed_years<-setdiff(observed_years, gap_years)
    }
    print("observed_years")
    
    
    for(year in observed_years){
      birth_year<-min(observed_years)
      death_year<-ifelse(max(observed_years)==2021, NA, max(observed_years))
      data_dummy<-rbind(data_dummy, data.frame(firmid=i, year=year, prodfra_plus=product))
    }
    
    print("Finished!")
    
  }
  
}


# for remaining vars draw from joint distribution for each group (all groups contain >= 4 members)
interest_vars<-c("rev")
group_vars = c('prodfra_plus', 'year')
set.seed(789)
data_dummy = simulate_continuous_vars(data, data_dummy, group_vars, interest_vars)
data_dummy$group_code<-NULL


# for remaining vars draw from joint distribution for each group (all groups contain >= 4 members)
interest_vars<-c("rev")
group_vars = c('prodfra_plus', 'year')
set.seed(789)
data_dummy = simulate_continuous_vars(data, data_dummy, group_vars, interest_vars)

data_dummy$group_code<-NULL

interest_vars<-c("code_entry_year", "active")
data_dummy = simulate_discrete_vars(data, data_dummy, group_vars, interest_vars)

saveRDS(data_dummy, paste0(output_dir, "product_data_", filter_indicator,  "_.RDS"))


# 2) SBS/BR combined ------------------------------

data<-readRDS('sbs_br_combined.rds')
data<-data[firmid %in% sample_firmids]

data[, count:= .N, by = .(NACE_BR, year)]; data = data[count > firm_id_threshold]; data[, count :=NULL ]
num_firms = data[!duplicated(firmid)] %>% nrow(); num_data_points = nrow(data)
year_nace = data[, .(share=.N/nrow(data)), by=.(NACE_BR, year)]

# for each obs. randomly assign an id and then draw from the joint distrib of year-NACE industry combinations

# data_dummy = data.table(firmid = 1:num_firms, NACE_BR=sample(year_nace$NACE_BR, size=num_firms, replace=T, prob=year_nace$share)) 
# data_dummy = data.table(firmid = 1:num_firms, NACE_BR=sample(year_nace$NACE_BR, size=num_firms, replace=T, prob=year_nace$share)) 
# 
# data_tab
# 
# data_dummy = data.table(firmid = sample(1:num_firms, num_data_points,  T)) %>%
#   cbind(.,year_nace[sample(1:num_data_points, num_data_points, T)]) %>% unique()

# distributions<-data%>% group_by(NACE_BR) %>% summarize(entry_years=list(table(min(year[firmid==firmid]))),
#                                                        exit_years=list(table(max(year[firmid==firmid]))),
#                                                        gap_counts=list(table(sum(diff(year[firmid==firmid])>1))),
#                                                        n_firms=n_distinct(firmid)) %>%
#   mutate(entry_prob=map(entry_years, ~.x/sum(.x)),
#          exit_prob=map(exit_years, ~.x/sum(.x)),
#          gap_prob=map(gap_counts, ~.x/sum(.x)))
# 
# nace_sample<-distributions %>% slice_sample(n=num_firms, weight_by = n_firms, replace=T) %>%
#   rowwise() %>%
#   mutate(firmid=row_number(),
#          entry_year=sample(names(entry_prob[[1]]), size=1, prob=entry_prob[[1]]),
#          exit_year=sample(names(exit_prob[[1]]), size=1, prob=exit_prob[[1]])) %>%
#   mutate(exit_year=ifelse(as.numeric(exit_year)<=as.numeric(entry_year),
#                           as.numeric(entry_year) + sample(1:5, 1),
#                           as.numeric(exit_year)),
#          gaps=as.numeric(sample(names(gap_prob[[1]]), size=1, prob=gap_prob[[1]])))


# entry_year_dist <- data %>% group_by(NACE_BR, firmid) %>% 
#   summarize(entry_year=min(year), .groups="drop") %>% 
#   count(NACE_BR, entry_year) %>% group_by(NACE_BR) %>% mutate(prob=n/sum(n))
# 
# exit_year_dist <- data %>% group_by(NACE_BR, firmid) %>% 
#   summarize(exit_year=max(year), .groups="drop") %>% 
#   count(NACE_BR, exit_year) %>% group_by(NACE_BR) %>% mutate(prob=n/sum(n))
# 
# gap_dist <- data %>% group_by(firmid, NACE_BR) %>% arrange(firmid, year) %>%
#   summarize(gaps=sum(diff(year)>1), n=max(year)-min(year), share=gaps/n, .groups="drop") %>% 
#   group_by(NACE_BR) %>% summarise(prob_1=mean(share))
# 
# duration_dist <- data %>% group_by(NACE_BR, firmid) %>% 
#   summarize(duration=max(year)-min(year)+1, .groups="drop") %>% 
#   count(NACE_BR, duration) %>% group_by(NACE_BR) %>% mutate(prob=n/sum(n))

# for(i in 1:num_firms){
#   nace_br<-sample(nace_dist$NACE_BR, size=1, prob=nace_dist$prob)
#   entry_year<-sample(entry_year_dist$entry_year[entry_year_dist$NACE_BR==nace_br],
#                      size=1,
#                      prob=entry_year_dist$prob[entry_year_dist$NACE_BR==nace_br])
#   exit_year<-sample(exit_year_dist$exit_year[exit_year_dist$NACE_BR==nace_br],
#                     size=1,
#                     prob=exit_year_dist$prob[exit_year_dist$NACE_BR==nace_br])
#   if(entry_year>=exit_year) exit_year<-entry_year + sample(duration_dist$duration[duration_dist$NACE_BR==nace_br], 1, 
#                                                            duration_dist$prob[duration_dist$NACE_BR==nace_br])
#   
#   num_gaps=  exit_year<-sample(exit_year_dist$exit_year[exit_year_dist$NACE_BR==nace_br],
#                                size=1,
#                                prob=exit_year_dist$prob[exit_year_dist$NACE_BR==nace_br])
#   
#   
# }


entry_year_dist <- data %>% group_by(firmid) %>%
  summarize(entry_year=min(year)) %>%
  count(entry_year)  %>% mutate(prob=n/sum(n))

birth_year_dist <- data %>% group_by(firmid) %>%
  summarize(entry_year=min(year)) %>%
  count(entry_year)  %>% mutate(prob=n/sum(n))

exit_year_dist <- data %>% group_by( firmid) %>%
  summarize(exit_year=max(year)) %>%
  count(exit_year)  %>% mutate(prob=n/sum(n))

gap_dist <- data %>% group_by(firmid) %>% arrange(firmid, year) %>%
  summarize(gaps=sum(diff(year)>1), n=max(year)-min(year), share=gaps/n) %>%
  count(gaps) %>% mutate(prob=n/sum(n))

duration_dist <- data %>% group_by(firmid) %>%
  summarize(duration=max(year)-min(year)+1, .groups="drop") %>%
  count(duration) %>% mutate(prob=n/sum(n))

firm_birth_year_dist <- data %>% group_by(firmid) %>%
  summarize(firm_birth_year=min(firm_birth_year)) %>%
  count(firm_birth_year) %>% mutate(prob=n/sum(n))


birth_year_dist <- data %>% group_by(firmid) %>%
  summarize(birth_year=min(firm_birth_year)) %>%
  count(birth_year) %>% mutate(prob=n/sum(n))

death_year_dist <- data %>% group_by(firmid) %>%
  summarize(death_year=min(firm_birth_year)) %>%
  count(death_year) %>% mutate(prob=n/sum(n))



nace_dist<-data %>% count(NACE_BR) %>% mutate(prob=n/sum(n))

set.seed(456)
data_dummy<-data.frame()


for(i in 1:num_firms){
  print(i)
  # i<-21
  nace_br<-sample(nace_dist$NACE_BR, size=1,  prob=nace_dist$prob)
  print("nace_br")
  entry_year<-sample(entry_year_dist$entry_year,
                     size=1,
                     prob=entry_year_dist$prob)
  print("entry_year")
  exit_year<-sample(exit_year_dist$exit_year,
                    size=1,
                    prob=exit_year_dist$prob)
  print("exit_year")
  
  firm_birth_year<-sample(firm_birth_year_dist$firm_birth_year,
                          size=1,
                          prob=firm_birth_year_dist$prob)
  print("firm_birth_year")
  
  
  
  if(!is.na(firm_birth_year) & firm_birth_year>entry_year){entry_year<-max(firm_birth_year, 1994)}
  
  if(entry_year>=exit_year){exit_year<-entry_year + sample(duration_dist$duration, 1, replace=T, duration_dist$prob)}
  
  num_gaps<-sample(gap_dist$gaps,size=1, prob=gap_dist$prob)
  print("num_gaps")
  
  
  observed_years<-seq(entry_year, exit_year)
  if(num_gaps>0 && length(observed_years) >2){
    gap_years<-sample(observed_years[-c(1, length(observed_years))], replace=T, num_gaps)
    
    observed_years<-setdiff(observed_years, gap_years)
  }
  print("observed_years")
  
  
  for(year in observed_years){
    birth_year<-min(observed_years)
    death_year<-ifelse(max(observed_years)==2021, NA, max(observed_years))
    data_dummy<-rbind(data_dummy, data.frame(firmid=i, year=year, NACE_BR=nace_br, firm_birth_year=firm_birth_year,
                                             death_year=death_year, birth_year=birth_year))
  }
  
  print("Finished!")
}


# for remaining vars draw from joint distribution for each group (all groups contain >= 4 members)
interest_vars<-c("nq", "empl", "capital", "turnover", "raw_materials", "labor_cost")
group_vars = c('NACE_BR', 'year')
data_dummy = simulate_continuous_vars(data, data_dummy, group_vars, interest_vars)
data_dummy$group_code<-NULL

# for remaining vars draw from joint distribution for each group (all groups contain >= 4 members)
interest_vars<-c("nq", "empl", "capital", "turnover", "raw_materials", "labor_cost")
group_vars = c('NACE_BR', 'year')
data_dummy = simulate_continuous_vars(data, data_dummy, group_vars, interest_vars)
data_dummy$group_code<-NULL



# interest_vars<-c("birth_year", "death_year", "firm_birth_year")
# data_dummy = simulate_discrete_vars(data, data_dummy, group_vars, interest_vars)

saveRDS(data_dummy, paste0(output_dir, "sbs_br_combined.RDS"))

# 3 CIS ---------------
  
data<-readRDS('cis_data.RDS')
data<-data[firmid %in% sample_firmids]

data[, count:= .N, by = .(NACE_BR, year)]; data = data[count > firm_id_threshold]; data[, count :=NULL ]
num_firms = n_firms; num_data_points = n_firms*4
year_nace = data[, .(NACE_BR, year)]

# for each obs. randomly assign an id and then draw from the joint distrib of year-NACE industry combinations
data_dummy = data.table(1:num_firms) %>%
  cbind(.,year_nace[sample(1:num_data_points, num_data_points, T)]) %>% unique()

# for remaining vars draw from joint distribution for each group (all groups contain >= 4 members)

interest_vars<-names(data)[grepl("(rrdinx|rrdexx|rmacx|rtot)", names(data))]
group_vars = c('NACE_BR', 'year')
data_dummy = simulate_continuous_vars(data, data_dummy, group_vars, interest_vars)
data_dummy$group_code<-NULL

interest_vars<-names(data)[!grepl("(^pnv|firmid|DEFind|year|NACE_BR|ho|rrdinx|rrdexx|rmacx|rtot)", names(data))]
group_vars = c('NACE_BR', 'year')
data_dummy = simulate_discrete_vars(data, data_dummy, group_vars, interest_vars)

saveRDS(data_dummy, paste0(output_dir, "cis_data.RDS"))

# 4 PATENTS ---------------

# Taking firm_data_select from module 4 part 4
data<-firm_data_select %>% select(firmid, year, NACE_BR, n_applications, n_publications)

data[, count:= .N, by = .(NACE_BR, year)]; data = data[count > firm_id_threshold]; data[, count :=NULL ]
num_firms = n_firms; num_data_points = nrow(data)
year_nace = data[, .(NACE_BR, year)]

# for each obs. randomly assign an id and then draw from the joint distrib of year-NACE industry combinations
data_dummy = data.table(firmid = 1:num_firms) %>%
  cbind(.,year_nace[sample(1:num_data_points, num_data_points, T)]) %>% unique()




# for remaining vars draw from joint distribution for each group (all groups contain >= 4 members)

interest_vars<-names(data)[grepl("(n_applications|n_publications)", names(data))]
group_vars = c('NACE_BR', 'year')
data_dummy = simulate_discrete_vars(data, data_dummy, group_vars, interest_vars)
data_dummy$group_code<-NULL
data_dummy$NACE_BR<-NULL



saveRDS(data_dummy, paste0(output_dir, 'patent_apps_published.RDS'))
