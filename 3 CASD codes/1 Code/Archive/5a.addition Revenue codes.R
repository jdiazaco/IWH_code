diff_codes=c()

missing_codes_revenue<-data.frame(year=numeric(), 
                                  missing_codes=numeric(),
                                  total_codes=numeric(),
                                  percentage_codes=numeric(), 
                                  missing_revenue=numeric(),
                                  total_revenue=numeric(),
                                  percentage_revenue=numeric())

for(yr in start:end){
  # yr<-2009
  filepath = paste0(raw_dir,'prodcom/prodcom',yr,'.csv')
  dta_temp = fread(filepath)
  dta_temp[, firmid := as.character(firmid)]
  
  prodfra_var = paste0('prodfra_',yr)
  prodfra_codes_vector <- unique(harmonized_prodfra[[prodfra_var]])
  prodcom_codes_vector <- unique(dta_temp$pcc8)
  diff_prodcom_prodfra_codes <- setdiff(prodcom_codes_vector, prodfra_codes_vector)
  diff_codes = c(diff_codes, diff_prodcom_prodfra_codes)
  
  percentage_codes<-(round(100 * sum(!(unique(prodcom_codes_vector) %in% unique(prodfra_codes_vector)))/length(unique(prodcom_codes_vector)), 2))
  print(paste0("Percentage of codes in prodcom not present in prodfra hamonization in year ", yr, " : ", percentage_codes, "%"))
  
  missing_revenue<-sum(dta_temp[dta_temp$pcc8 %in% diff_prodcom_prodfra_codes, ]$rev)
  total_revenue<-sum(dta_temp$rev, na.rm = T)
  percentage_revenue<-(round(100 * (missing_revenue/total_revenue), 2))
  
  print(paste0("Percentage of revenue from codes in prodcom not present in prodfra hamonization in year ", yr, " : ", 
               percentage_revenue, "%"))
  new_row<- data.frame(year=yr, 
                       missing_codes=sum(!(unique(prodcom_codes_vector) %in% unique(prodfra_codes_vector))),
                       total_codes=length(unique(prodcom_codes_vector)),
                       percentage_codes=percentage_codes, 
                       missing_revenue=missing_revenue,
                       total_revenue=total_revenue,
                       percentage_revenue=percentage_revenue)
  missing_codes_revenue<-rbind(missing_codes_revenue, new_row)
}

write.csv(missing_codes_revenue, file = paste0(output_dir, "missing_codes_revenue.csv"))
description(missing_codes_revenue, "Table with number and percentage of prodcom codes not covered by code harmonization in a particular year, total number of codes, the amount and percentage of revenue they represent in total prodcom revenue. \n")



ggplot(missing_codes_revenue, aes(x=year)) +
  geom_line(aes(y = percentage_revenue, color="Revenue")) + 
  geom_line(aes(y = percentage_codes, color="Number of codes")) + 
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  scale_x_continuous(breaks =  seq(2010, 2021, by = 1)) +  
  labs(title = "Codes not covered by prodfra harmonization", subtitle = "Share of total number of codes and total revenue per year", x = element_blank(),  y = element_blank(), color="Metric") +
  theme_classic() +theme(legend.position = 'bottom')
ggsave(paste0(output_dir, 'Share_codes_harmonization.png'),  width=6, height = 5)
description(Share_codes_harmonization.png, "Graph with percentage of prodcom codes not covered by code harmonization in a particular year and percentage of revenue they represent in total prodcom revenue. \n")
