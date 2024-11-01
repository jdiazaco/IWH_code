# setup -------------------------------------------------------------------
# libraries
rm(list = ls())
gc()
packages = c('data.table', 'haven', 'readxl', 'openxlsx','stringr', 'readr', 'dplyr', 'tidyverse', 'zoo', 'reshape2','rstudioapi', "broom")
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
output_dir<-"C:/Users/NEWPROD_J_DIAZ-AC/Documents/Reallocation/3 Output/graphs/product breakdown/19.04.24/Addition/"
tools_dir <- 'C:/Users/NEWPROD_J_DIAZ-AC/Documents/Reallocation/1 Code/Product decomposition/Tools/'
source(paste0(tools_dir, "description.R"))
source(paste0(tools_dir, "deflate.R"))
source(paste0(tools_dir, "summary stats helper.R"))


firm_data<-readRDS('combined_sbs_br_prodcom_data.rds')
setorder(firm_data, year)
# make_summary_stats(firm_data, c("capital", "turnover", "nq", "raw_materials", "labor_cost"), "year", "firm_data_years")
# make_summary_stats(firm_data, c("capital", "turnover", "nq", "raw_materials", "labor_cost"), "DEFind", "firm_data_industries")


# # Set parameters for prodfra-pcc8 and excluded industries
# prodfra_or_pcc8<-"prodfra"
# only_prodfra_in_prodcom<-FALSE
# exclude_industries<-TRUE
# filter<-paste0(if(prodfra_or_pcc8=="prodfra") "10_digit" else if(prodfra_or_pcc8=="pcc8") "8_digit" else prodfra_or_pcc8, 
#                "_", 
#                if(only_prodfra_in_prodcom) "only_prodfra_in_prodcom" else "all_prodfra",
#                "_",
#                if(exclude_industries) "exclude_industries" else "not_exclude_industries")



share_capital_costs<-0.1

firm_data<-firm_data %>% filter(labor_cost!=0 & !is.na(labor_cost) & 
                                  turnover!=0 &!is.na(turnover) &  
                                  raw_materials!=0 &  !is.na(raw_materials) & 
                                  capital!=0 & !is.na(capital))

firm_data<-firm_data %>% mutate(log_turnover=log(turnover),
                                log_labor = log(labor_cost),
                                log_capital = log(capital*share_capital_costs),
                                log_inputs = log(raw_materials),
                                log_rev=log(rev),
                                NACE_2d=substr(NACE_BR, 1, 2),
                                NACE_3d=substr(NACE_BR, 1, 3)
                                )

# 1) Grpahs with cost-share analysis-----------------------------------

needed_variables<-c("firmid", "year", "labor_cost", "raw_materials", "turnover", "rev", "capital", "log_turnover", "log_labor", "log_capital", "log_inputs", "log_rev", "NACE_BR", "NACE_2d", "NACE_3d", "in_prodcom")

firm_data_csa<-firm_data %>% select(needed_variables)
firm_data<-NULL
gc()

firm_data_csa<-firm_data_csa %>% mutate(sum_costs=(labor_cost + (capital*share_capital_costs) + raw_materials),
                                        t_l=labor_cost/sum_costs,
                                        t_k=(capital*share_capital_costs)/sum_costs,
                                        t_m=(raw_materials)/sum_costs,
                                        # log_TFP= log_turnover - (t_l*log_labor) - (t_k*log_capital) - (t_m*log_inputs)
                                        )

firm_data_csa <- firm_data_csa %>% group_by(NACE_2d, year) %>% mutate(median_firm=firmid[which.min(abs(turnover-median(turnover)))],
                                                                      t_l_industry=t_l[firmid==median_firm],
                                                                      t_k_industry=t_k[firmid==median_firm],
                                                                      t_m_industry=t_m[firmid==median_firm],
                                                                      log_TFP= log_turnover - (t_l_industry*log_labor) - (t_k_industry*log_capital) - (t_m_industry*log_inputs),
                                                                      TFP=exp(log_TFP),
                                                                      check=t_l_industry+t_k_industry+t_m_industry)

make_summary_stats(firm_data_csa, c(setdiff(needed_variables, c("firmid", "year", "in_prodcom", "NACE_BR")), "t_l_industry", "t_k_industry", "t_m_industry"), "NACE_2d", "productivity_industry")

all_FARE<-FALSE
only_PRODCOM<-TRUE
description_firms<-if(all_FARE) "All firms in FARE/FICUS with information on revenue, capital, inputs and labor costs" else
  "Only firms in both FARE/FICUS and PRODCOM with information on revenue, capital, inputs and labor costs"

if(all_FARE){
  firm_data_graphs<-firm_data_csa
  ext_name<-"all_FARE_FICUS"
}else{
  if(only_PRODCOM){
    firm_product_data<-readRDS(paste0("product_firm_data_fpqi.RDS"))
    # firm_data_graphs<-merge(firm_product_data  %>% summarise(firmid=unique(firmid)), firm_data_csa, by="firmid")
    firm_data_graphs<-firm_data_csa %>% filter(in_prodcom)
    ext_name<-"only_PRODCOM"
  }
}



results_tables <- firm_data_graphs %>% group_by(year) %>% mutate(log_TFP=(log_TFP*turnover)/sum(turnover),
                                                                 TFP=(TFP*turnover)/sum(turnover))
results_tables <- results_tables %>% group_by(year) %>% summarise(log_TFP=sum(log_TFP),
                                                                  TFP=sum(TFP))
results_tables$TFP<-exp(results_tables$log_TFP)
# results_tables$TFP<-results_tables$log_TFP
results_tables <- results_tables %>% mutate(TFP= TFP/TFP[year==2017])
results_tables<- merge(results_tables, read_excel(paste0(output_dir, "france TFP.xlsx")), by="year", all.x = T)
ylim=c(-0.5,4.5)

ggplot(results_tables, aes(x=year)) +
  geom_line(aes(y=TFP.x, color="Estimated TFP")) + 
  geom_line(aes(y=TFP.y, color="Official TFP")) + 
  labs(x="Year", y="TFP", title="Aggregate (revenue weighted) TFP per year", subtitle = paste0(description_firms, " v. official data (base year 2017)")) + 
  scale_x_continuous(limits=c(1994, 2021), breaks =  seq(1994, 2021, by = 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))+
   coord_cartesian(ylim=ylim)
ggsave(paste0(output_dir, 'line_TFP_v_official', ext_name, '.png'),  width=9, height = 5)
description(paste0('line_TFP_v_official', ext_name, '.png'), 
            paste0("Line with the change of TFP through time against official data. ", description_firms, " \n"))


ggplot(results_tables, aes(x=year, y=log_TFP)) +
  geom_line() + 
  labs(x="Year", y="log(TFP)", title="Aggregate (revenue weighted) log(TFP) per year", subtitle=description_firms) + 
  scale_x_continuous(limits=c(1994, 2021), breaks =  seq(1994, 2021, by = 1)) +
  theme_minimal()  +
  theme(axis.text.x = element_text(angle = 90))+
  coord_cartesian(ylim=ylim)
ggsave(paste0(output_dir, 'line_TFP', ext_name, '.png'),  width=9, height = 5)
description(paste0('line_TFP', extension, '.png'), 
            paste0("Line with the change of the interquartile range of firm-level TFP through time. ", description_firms, " \n"))


#Fill this later

# ggsave(paste0(output_dir, 'line_TFP_', ext_name, '.png'),  width=7, height = 5)
# description(paste0('line_TFP_all FARE_FICUS.png'), 
#             paste0("Line with the change of the interquartile range of firm-level TFP through time. ", description_firms, " \n"))

# ggplot(firm_data_graphs, aes(x=log_TFP, y=as.factor(year), fil=..x..)) + 
#   geom_density_ridges(scale=1.5, alpha=0.6, bandwidth=0.1) + 
#   scale_fill_viridis_c() +
#   labs(x="log_TFP", y="Year", title="Distribution of log_TFP per year") + 
#   coord_cartesian(xlim=c(0,3))+
#   theme_minimal()

ggplot(firm_data_graphs, aes(x=as.factor(year), y=log_TFP)) + 
  geom_boxplot(fill="lightblue", color="darkblue", outlier.shape = NA) + 
  labs(x="Year", y="log(TFP)", title="Distribution of firm log TFP per year", subtitle = description_firms) + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90))+
  coord_cartesian(ylim=ylim)
ggsave(paste0(output_dir, 'box_chart_TFP_', ext_name, '.png'),  width=9, height = 5)
description(paste0('box_chart_TFP_all FARE/FICUS.png'),
            "Box chart with the distribution of firm-level TFP through time")

iqr_per_year<-firm_data_graphs %>% group_by(year) %>% summarise(IQR = IQR(log_TFP))
ggplot(iqr_per_year, aes(x=year, y=IQR)) +
  geom_line() + 
  labs(x="Year", y="IQR", title="Firm TFP inter-quartile range per year", subtitle = description_firms) + 
  scale_x_continuous(limits=c(1994, 2021), breaks =  seq(1994, 2021, by = 1)) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90))+
  coord_cartesian(ylim=ylim)
ggsave(paste0(output_dir, 'line_TFP_IQR_', ext_name, '.png'),  width=7, height = 5)
description(paste0('box_chart_TFP_', ext_name, '.png'),
            "Line with the change of the interquartile range of firm-level TFP through time")

make_summary_stats(firm_product_data, "firm_product_quality_index", "year", "fpqi_year.xlsx")

fpqi<-firm_product_data %>% filter(!is.na(firm_product_quality_index))
make_summary_stats(fpqi, "firm_product_quality_index", "year", "fpqi_filtered")

length(unique(fpqi$firmid))
length(unique(fpqi$prodfra_plus))

fpqi<-firm_product_data %>% group_by(firmid, year) %>% summarise(fpqi=(firm_product_quality_index*rev)/sum(rev))
fpqi<-fpqi %>% group_by(firmid, year) %>% summarise(fpqi=sum(fpqi))
fpqi<-merge(fpqi, firm_data_csa %>% select(firmid, year, log_TFP), by=c("firmid", "year"), all.x = T)

ggplot(fpqi, aes(x=fpqi, y=log_TFP)) + 
  geom_point() +
  geom_smooth(method="lm", se=T, color="blue") + 
  labs(x="FPQI", y ="log_TFP", title="Scatter plot of firm-product match quality v. log(TFP)")
ggsave(paste0(output_dir, paste0('scatter_plot_TFP_FPQI.png') ),  width=7, height = 5)
description(paste0('scatter_plot_TFP_FPQI', yr, '.png'),
            paste0("Scatter plot with firm-product match quality index against log(TFP)"))


library(plm)
library(xtable)
model<-plm(log_TFP ~ fpqi + factor(year) + factor(NACE_2d), data=fpqi, model="within")
summary(model)
residuals<-as.data.frame(resid(model))
index<-index(model)
index<-cbind(index, residuals)
index<-merge(index, fpqi, by=c("firmid","year"), all=T)

ggplot(index %>% filter(!is.na(`resid(model)`))) + 
  geom_histogram(aes(x=log_TFP, fill="log TFP"), alpha=0.5) + 
  geom_histogram(aes(x=resid(model), fill="Residuals"), alpha=0.5) + 
  labs(title="Distribution of log(TFP) and residuals",
       x="Value",
       y="frequency",
       fill="Variable")
ggsave("residuals.png", width=7, height = 5)

ggplot(index %>% filter(!is.na(`resid(model)`))) + 
  geom_point(aes(x=fpqi, y=`resid(model)`)) +
  geom_point(aes(x=fpqi, y=residuals)) +
  geom_smooth(method="lm", se=T, color="blue") + 
  labs(x="FPQI", y ="log_TFP/residuals", title="Scatter plot of firm-product match quality v. log(TFP) and residuals")
ggsave("scatter_residuals_log_TFP.png", width=7, height = 5)



latex_table<-xtable(summary(model))
print(latex_table, type="latex", file=paste0(output_dir, "fixed_effect_regression.tex"))



for(yr in 2009:2021){
  ggplot(fpqi %>% filter(year==yr), aes(x=fpqi, y=log_TFP)) + 
    geom_point() +
    geom_smooth(method="lm", se=T, color="blue") + 
    labs(x="FPQI", y ="log_TFP", title="Scatter plot of firm-product match quality v. log(TFP)", subtitle = paste0("Year: ", yr))
  ggsave(paste0(output_dir, paste0('scatter_plot_TFP_FPQI_',  yr, '.png') ),  width=7, height = 5)
  description(paste0('scatter_plot_TFP_FPQI', yr, '.png'),
              paste0("Scatter plot with firm-product match quality index against log(TFP) in year ", yr))
  
}

# 1) Grpahs with ols analysis-----------------------------------


productivity_data<-firm_data %>% select(firmid, year, log_turnover, log_labor, log_capital, log_inputs)
rm(firm_data)
gc()

productivity_data[]<-lapply(productivity_data, function(col){
  replace(col, is.nan(col) | is.infinite(col), NA)
})

productivity_data <- productivity_data %>% group_by(firmid) %>% filter(any(!is.na(log_capital) & !is.na(log_turnover) & !is.na(log_labor)  & !is.na(log_inputs)))

# productivity_data <- productivity_data %>% group_by(firmid) %>% filter(!all(is.na(log_capital)) & !all(is.na(log_turnover)) & !all(is.na(log_labor))  & !all(is.na(log_inputs)))
productivity_data <- productivity_data %>% filter(!is.na(log_turnover))

# productivity_data <- productivity_data %>% filter(!is.na(log_capital) & !is.na(log_turnover) & !is.na(log_labor)  & !is.na(log_inputs))
# productivity_data <- productivity_data %>% filter(firmid=="385132881" | firmid=="383883287" | firmid=="331078352")


regression_results <- productivity_data %>% group_by(firmid) %>% do({results=lm(log_turnover ~ log_labor+ log_capital+ log_inputs, data=.)
  coefficients<-tidy(results)
  coefficients <- coefficients %>% select(term, estimate)
  reshaped_coefficients<-coefficients %>% pivot_wider(names_from = term, values_from = estimate)
  colnames(reshaped_coefficients) <- c("intercept", "theta_l", "theta_k", "theta_m")
  results<-augment(results)
  # intercept<-coefficients[1,2]
  # results<-cbind(results, intercept)
  # results<-cbind(results, coefficients[1,1], coefficients[1,2])
  
  results<-cbind(results, reshaped_coefficients[1,1], reshaped_coefficients[1,2], reshaped_coefficients[1,3], reshaped_coefficients[1,4])
})

fwrite(regression_results, paste0(output_dir, "productivity.csv"))


productivity_data<-merge(productivity_data, regression_results %>% select(firmid, log_turnover, log_labor, log_capital, log_inputs, .resid, intercept, theta_l, theta_k, theta_m), 
                         by=c("firmid", "log_turnover", "log_labor", "log_capital", "log_inputs"), all.x=T)

productivity_data<-productivity_data %>% mutate(check=(intercept + (theta_k*log_capital) + (theta_l*log_labor) + (theta_m*log_inputs)+ .resid))

fwrite(productivity_data, paste0(output_dir, "productivity_firm_data.csv"))

productivity_data<-fread("C:/Users/NEWPROD_J_DIAZ-AC/Documents/Reallocation/3 Output/graphs/product breakdown/Revision product breakdown/productivity_firm_data.csv")

prod_all_ts <- productivity_data %>% filter(!is.na(theta_k) & !is.na(theta_l) & !is.na(theta_m) & !is.na(intercept) )

prod_all_ts <- prod_all_ts %>% mutate(TFP_ols = intercept + .resid)

prod_all_ts<-merge(prod_all_ts, firm_data %>% select(firmid, year, turnover), by=c("firmid", "year"), all.x = T)


results_tables <- prod_all_ts %>% group_by(year) %>% mutate(log_TFP=(TFP_ols*turnover)/sum(turnover, na.rm = T))
results_tables <- results_tables %>% group_by(year) %>% summarise(log_TFP=sum(log_TFP, na.rm=T))

all_FARE<-TRUE
only_PRODCOM<-FALSE
description_firms<-if(all_FARE) "OLS. All firms in FARE/FICUS with information on revenue, capital, inputs and labor costs" else
  "OLS. Only firms in both FARE/FICUS and PRODCOM with information on revenue, capital, inputs and labor costs"

if(all_FARE){
  firm_data_graphs<-prod_all_ts
  ext_name<-"all_FARE_FICUS_ols"
}else{
  if(only_PRODCOM){
    firm_product_data<-readRDS(paste0("product_firm_data_fpqi_", filter,  "_.RDS"))
    firm_data_graphs<-merge(firm_product_data  %>% summarise(firmid=unique(firmid)), prod_all_ts, by="firmid")
    ext_name<-"only_PRODCOM_ols"
  }
}




ggplot(results_tables, aes(x=year, y=log_TFP)) +
  geom_line() + 
  labs(x="Year", y="log(TFP)", title="Aggregate (revenue weighted) TFP per year", subtitle=description_firms) + 
  scale_x_continuous(limits=c(1994, 2021), breaks =  seq(1994, 2021, by = 1)) +
  theme_minimal()  +
  coord_cartesian(ylim=c(0,4))
ggsave(paste0(output_dir, 'line_TFP_ols.png'),  width=7, height = 5)
description(paste0('line_TFP_ols.png'), 
            paste0("Line with the change of the interquartile range of firm-level TFP through time. ", description_firms, " \n"))

results_tables$TFP<-exp(results_tables$log_TFP)
# results_tables$TFP<-results_tables$log_TFP
results_tables <- results_tables %>% mutate(TFP= TFP/TFP[year==2017])
results_tables<- merge(results_tables, read_excel(paste0(output_dir, "france TFP.xlsx")), by="year")

ggplot(results_tables, aes(x=year)) +
  geom_line(aes(y=TFP.x, color="Estimated TFP")) + 
  geom_line(aes(y=TFP.y, color="Official TFP")) + 
  labs(x="Year", y="TFP", title="Aggregate (revenue weighted) TFP per year", subtitle = paste0(description_firms, " v. official data (base year 2017)")) + 
  scale_x_continuous(limits=c(1994, 2021), breaks =  seq(1994, 2021, by = 1)) +
  theme_minimal() +
  coord_cartesian(ylim=c(0,2))
ggsave(paste0(output_dir, 'line_TFP_v_official', ext_name, '.png'),  width=7, height = 5)
description(paste0('line_TFP_v_official', ext_name, '.png'), 
            paste0("Line with the change of TFP through time against official data. ", description_firms, " \n"))



# ggplot(firm_data_graphs, aes(x=log_TFP, y=as.factor(year), fil=..x..)) + 
#   geom_density_ridges(scale=1.5, alpha=0.6, bandwidth=0.1) + 
#   scale_fill_viridis_c() +
#   labs(x="log_TFP", y="Year", title="Distribution of log_TFP per year") + 
#   coord_cartesian(xlim=c(0,3))+
#   theme_minimal()

ggplot(firm_data_graphs, aes(x=as.factor(year), y=TFP_ols)) + 
  geom_boxplot(fill="lightblue", color="darkblue", outlier.shape = NA) + 
  labs(x="Year", y="log(TFP)", title="Distribution of firm log TFP per year", subtitle = description_firms) + 
  theme_minimal() +
  coord_cartesian(ylim=c(-3,8))
ggsave(paste0(output_dir, 'box_chart_TFP_', ext_name, '.png'),  width=7, height = 5)
description(paste0('box_chart_TFP_all FARE/FICUS.png'),
            "Box chart with the distribution of firm-level TFP through time")

iqr_per_year<-firm_data_graphs %>% group_by(year) %>% summarise(IQR = IQR(TFP_ols))
ggplot(iqr_per_year, aes(x=year, y=IQR)) +
  geom_line() + 
  labs(x="Year", y="IQR", title="Firm TFP inter-quartile range per year", subtitle = description_firms) + 
  scale_x_continuous(limits=c(1994, 2021), breaks =  seq(1994, 2021, by = 1)) +
  theme_minimal() +
  coord_cartesian(ylim=c(0,2.6))
ggsave(paste0(output_dir, 'line_TFP_IQR_', ext_name, '.png'),  width=7, height = 5)
description(paste0('box_chart_TFP_', ext_name, '.png'),
            "Line with the change of the interquartile range of firm-level TFP through time")


fpqi<-firm_product_data %>% group_by(firmid, year) %>% summarise(fpqi=(firm_product_quality_index*rev)/sum(rev))
fpqi<-fpqi %>% group_by(firmid, year) %>% summarise(fpqi=sum(fpqi))
fpqi<-merge(fpqi, prod_all_ts %>% select(firmid, year, TFP_ols), by=c("firmid", "year"), all.x = T)


ggplot(fpqi, aes(x=fpqi, y=TFP_ols)) + 
  geom_point() +
  geom_smooth(method="lm", se=T, color="blue") + 
  labs(x="FPQI", y ="TFP_ols", title="Scatter plot of firm-product match quality v. log(TFP)")
ggsave(paste0(output_dir, paste0('scatter_plot_TFP_FPQI.png') ),  width=7, height = 5)
description(paste0('scatter_plot_TFP_FPQI.png'),
            paste0("Scatter plot with firm-product match quality index against log(TFP) in year "))


for(yr in 2009:2021){
  ggplot(fpqi %>% filter(year==yr), aes(x=fpqi, y=TFP_ols)) + 
    geom_point() +
    geom_smooth(method="lm", se=T, color="blue") + 
    labs(x="FPQI", y ="TFP_ols", title="Scatter plot of firm-product match quality v. log(TFP)", subtitle = paste0("Year: ", yr))
  ggsave(paste0(output_dir, paste0('scatter_plot_TFP_FPQI_',  yr, '.png') ),  width=7, height = 5)
  description(paste0('scatter_plot_TFP_FPQI', yr, '.png'),
              paste0("Scatter plot with firm-product match quality index against log(TFP) in year ", yr))
  
}

  





ggplot(results_tables %>% filter(year>=2015), aes(x=log_TFP, y=as.factor(year), fil=..x..)) + 
  geom_density_ridges(scale=1.5, alpha=0.6, bandwidth=0.1) + 
  scale_fill_viridis_c() +
  labs(x="log_TFP", y="Year", title="Distribution of log_TFP per year") + 
  theme_minimal()



firm_data_csa<-firm_data_csa %>% mutate(log_turnover=log(turnover_og),
                                        
                                  log_labor = log(labor_cost_og),
                                log_capital = log(capital_og*share_capital_costs),
                                log_inputs = log(raw_materials_og)
                                )


  
  mutate(t_labor = log(labor_cost_og),
                                    t_capital = log(capital_og*share_capital_costs),
                                    t_inputs = log(raw_materials_og)
                                    )

productivity_data<-firm_data %>% select(firmid, year, log_turnover, log_labor, log_capital, log_inputs)



                                                                    

results_list<-vector("list", length = nrow(regression_results))
  
for(i in 1:nrow(regression_results)){
  results_list[[i]]$firmid<-regression_results$firmid[i]
  results_list[[i]]$coefficients<-augment(regression_results[["results"]][i])
}


coefficients_data <- regression_results %>% tidy(results)


regression_results <- productivity_data %>% group_by(firmid) %>% do(augment(lm(log_turnover ~ log_labor+ log_capital+ log_inputs, data=.)))


regression_results <- productivity_data %>% group_by(firmid) %>% do(results=lm(log_turnover ~ log_labor+ log_capital+ log_inputs, data=.))
coefficients_data <- regression_results %>% collapse(results=tidy(results))


regression_results <- productivity_data %>% group_by(firmid) %>% do(augment(lm(log_turnover ~ log_labor+ log_capital+ log_inputs, data=.)))

regression_results <- productivity_data %>% group_by(firmid) %>% do(results=lm(log_turnover ~ log_labor+ log_capital+ log_inputs, data=.))
coefficients<-regression_results %>% group_by(firmid) %>% summarise(tidy(results))

coefficients_data <- regression_results %>% group_by(firmid) %>% summarise(results=tidy(results))



regression_results <- productivity_data %>% group_by(firmid) %>% do({
  model<-lm(log_turnover ~ log_labor+ log_capital+ log_inputs, data=.)
  # tidy(model)
  augment(model)
  # coefficients<-as.data.frame(coef(model))
})

residuals_data <- regression_results %>% select(firmid, residuals)

regression_results <- productivity_data %>% group_by(firmid) %>% do({
  model<-lm(log_turnover ~ log_labor+ log_capital+ log_inputs, data=.)
  data.frame(firmid=.$firmid,
             coefficients= as.list(coef(model)),
             residuals=residuals(model)
             )
  # tidy(model)
  # augment(model)
})




results=tidy(lm(log_turnover ~ log_labor+ log_capital+ log_inputs, data=.)))


regression_results <- regression_results %>% unnest(cols=c(results))
productivity_data <- left_join(productivity_data, regression_results, by="firmid")

coefficients<-regression_results$results %>% map("coefficients")
residuals<-regression_results$results %>% map("residuals")
productivity_data<-productivity_data %>% mutate( intercept = coefficients %>% map_chr(1),
                                                 a = residuals %>% map_chr(1))


productivity_results<-regression_results %>% mutate(intercept=map_dbl(results, ~coef(summary(.))["(Intercept)", "Estimate"])) %>% select(firmid, year, productivity=intercept)
  
  mutate(summary=map(results, summary)) %>% unnest(c(summary)) %>% filter(term=="Intercept")
productivity_results<-regression_results %>%  tidy(results) %>% select(firmid, year, term, estimate) %>% spread(term, estimate) %>% rename(productivity=`(Intercept)`)

productivity_results<-tidy(regression_results$results)