#5) Graphs ------------------------------------

product_firm_data<-product_firm_data %>% mutate(age_product_years_validation=ifelse(age_product_years<0, product_tenure, age_product_years))

#5a) Histograms ------------------------------------

hist_vars<-c("rev_growth", "age_product_years", "age_product_years_validation", "firm_product_quality_index", "HHI", "code_entry_year", "av_rev_growth_superstar", "firm_product_quality_index_single")
hist_vars_description <-c("Unweighted revenue Growth", "Product Age (2001 left-censored)", "Product Age (Validation)", "Firm-Product Match Quality estimate", "Within-Firm Product HHI", "Year of First Appearance in PC8 tables (beginning 2001)", "Average Revenue Growth per product", "Firm-Product Match Quality estimate")
dataset<-c(rep("product_firm_data", 6), "superstar", "fpqi")
level_aggregation<-c(rep("Firm-product-year", 6), "Product", "Firm-product")


for(i in 1:length(hist_vars)){
  ggplot(get(dataset[i]), aes(x=get(hist_vars[i])))+
    geom_histogram(fill="skyblue") + 
    labs(title=paste0("Distribution of ", hist_vars_description[i]),
         subtitle=paste0("Dataset: ", level_aggregation[i]),
         x=hist_vars_description[i],
         y="Frequency"
    )
  ggsave(paste0(output_dir, "histogram_", hist_vars[i], '.png'), height=5, width=5)
}

hist(product_firm_data$age_product_years_validation,
     main = "Distribution of Product Age",
     xlab="Product Age",
     xlim = c(0,20)
)
ggsave(paste0(output_dir, "histogram_age_product_years_validation_nicer.png"), height=5, width=5)


#5b) Match quality rev_growth quantiles decomposition ------------------------------------
product_firm_data$all=T
graphs<-c("fpqi")
vars<-c("percentile", "decile", "quintile", "quartile")

sub_groups = c('first_introduction', 'reintroduced', 'discontinued', 'incumbent', "all")
# sub_groups_g = paste0(sub_groups, '_growth')
# for (i in seq_along(sub_groups)){
#   product_firm_data[, sub_groups_g[i]:= sum(rev_growth_weighted * get(sub_groups[i])), by = year]
# }
# growth_decomposition = unique(product_firm_data %>% select(year,agg,sub_groups_g))

for (sub_group in sub_groups) {
  
  for(graph in graphs){
    for(var in vars){
      # graph<-c("fpqi")
      # var<-c("percentile")
      # sub_group = c('reintroduced')
      graph_var<-paste0(graph, "_", var)
      fpqi_ind<-paste0(var, "_ind")
      max_var<-max(product_firm_data[[graph_var]], na.rm = T)
      min_var<-min(product_firm_data[[graph_var]], na.rm = T)
      median_var<-round((max_var+min_var)/2)
      
      # match_quality_rev_growth= product_firm_data %>% group_by(prodfra_plus) %>% mutate(paste0( "_",sub_group, "_growth"))
      
      match_quality_rev_growth= product_firm_data[, .(av_rev_growth_fpqi = sum(rev_growth_weighted*get(sub_group), na.rm=T)),
                                                  by= .(get(graph_var), year)]
      match_quality_rev_growth <- match_quality_rev_growth %>% filter(!(is.na(get) | year==2009)) %>% 
        mutate(ind=ifelse(get==max_var, paste0("top ", var), 
                          ifelse(get==min_var, paste0("bottom ", var),
                                 #ifelse(var=="percentile", 
                                 paste0("other ", var, "s"))))
      match_quality_rev_growth= match_quality_rev_growth[, .(av_rev_growth_fpqi = sum(av_rev_growth_fpqi, na.rm=T)),
                                                         by= .(ind, year)]
      match_quality_rev_growth<-match_quality_rev_growth %>% arrange(desc(ind))
      # ggplot(match_quality_rev_growth, aes(x = year, y = av_rev_growth_fpqi*100, color = as.factor(ind))) +
      #   geom_line() + scale_y_continuous(labels = scales::label_percent(scale = 1)) +
      #   scale_x_continuous(breaks =  seq(2009, 2021, by = 3)) +  
      #   labs(title = paste0(var, "s"), x = element_blank(),  y = "Av. revenue growth rate", color = 'type') +
      #   theme_classic() +theme(legend.position = 'bottom')
      
      assign(paste0(var, "_", sub_group, '_g'), ggplot(match_quality_rev_growth, aes(x = year, y = av_rev_growth_fpqi*100, color = as.factor(ind))) +
               geom_line() + scale_y_continuous(labels = scales::label_percent(scale = 1)) +
               scale_x_continuous(breaks =  seq(2009, 2021, by = 3)) +  
               labs(title = paste0(var, "s"), x = element_blank(),  y = "Av. revenue growth rate", color = 'type') +
               theme_classic() +theme(legend.position = 'bottom')
      )
    }
  }
  assign(paste0(graph,"_", sub_group, '_g'),
         (get(paste0("percentile_", sub_group, "_g")) +theme(axis.title.x  = element_blank(), legend.position = 'bottom')) |
           (get(paste0("decile_", sub_group, "_g"))  + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = 'bottom')) |
           (get(paste0("quintile_", sub_group, "_g")) + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = 'bottom')))
  assign(paste0(graph,"_", sub_group, '_g'), get(paste0(graph,"_", sub_group, '_g')) + plot_annotation(title = "Economy-level weighted revenue growth by product-firm match quality", subtitle=paste0("Group: ", sub_group)))
  ggsave(paste0(output_dir, graph, "_", sub_group, '_g.png'), get(paste0(graph,"_", sub_group, "_g")), width=10, height = 5)
}

#Superstar products rev_growth quantiles decomposition


superstar_rev_growth= product_firm_data[, .(av_rev_growth = mean(rev_growth, na.rm=T)),
                                        by= .(superstar, year)]
superstar_rev_growth <- superstar_rev_growth %>% filter(!(is.na(av_rev_growth) | year==2009)) 
ggplot(superstar_rev_growth, aes(x = year, y = av_rev_growth*100, color = as.factor(superstar))) +
  geom_line() + scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  scale_x_continuous(breaks =  seq(2009, 2021, by = 3)) +  
  labs(title = "Average revenue growth for high-growth products", 
       subtitle = paste0("'High-growth' is the top quintile of products by average revenue growth (cutoff=", round(av_rev_growth_superstar_top_quintile_cutoff*100,2), "%)"),
       x = element_blank(),  y = "Av. revenue growth rate", color = 'type') +
  theme_classic() +theme(legend.position = 'bottom')
ggsave(paste0(output_dir, 'superstar_av_revenue growth.png'),  width=8, height = 5)


#Match quality rev_growth quintiles decomposition

product_firm_data$fpqi_quintile<-product_firm_data$fpqi_quintile-1
for (sub_group in sub_groups) {
  
  match_quality_rev_growth= product_firm_data[, .(av_rev_growth = sum(rev_growth_weighted*get(sub_group), na.rm=T)),
                                              by= .(fpqi_quintile, year)]
  match_quality_rev_growth <- match_quality_rev_growth %>% filter(!(is.na(fpqi_quintile) | year==2009)) 
  
  ggplot(match_quality_rev_growth, aes(x = year, y = av_rev_growth*100, color = as.factor(fpqi_quintile))) +
    geom_line() + scale_y_continuous(labels = scales::label_percent(scale = 1)) +
    scale_x_continuous(breaks =  seq(2009, 2021, by = 3)) +  
    labs(title = "Economy-level weighted revenue growth by product-firm match quality", subtitle=paste0("Group: ", sub_group), x = element_blank(),  y = "Av. revenue growth rate", color = 'quintile') +
    theme_classic() +theme(legend.position = 'bottom')
  ggsave(paste0(output_dir, 'fpqi_', sub_group, '_5quintiles.png'),  width=6, height = 5)
}

#Age graphs

product_firm_data$fpqi_quintile<-product_firm_data$fpqi_quintile+1
for (sub_group in sub_groups) {
  
  age_rev_growth= product_firm_data %>% group_by(age_product, year) %>% filter(get(sub_group)) %>% summarise(av_rev_growth = mean(rev_growth, na.rm=T)) 
  age_rev_growth<-age_rev_growth %>% filter(year!=2009 & !is.na(age_product))
  
  ggplot(age_rev_growth, aes(x = year, y = av_rev_growth*100, color = age_product)) +
    geom_line() + scale_y_continuous(labels = scales::label_percent(scale = 1)) +
    scale_x_continuous(breaks =  seq(2010, 2021, by = 1)) +  
    labs(title = "Unweighted product-level average revenue growth by product age", subtitle = paste0("Group: ", sub_group), x = element_blank(),  y = "Av. revenue growth rate", color = 'type') +
    theme_classic() +theme(legend.position = 'bottom')
  ggsave(paste0(output_dir, sub_group, '_product_unweighted_age_av_rev_growth.png'),  width=6, height = 5)
}


for (sub_group in sub_groups) {
  
  age_rev_growth= product_firm_data[, .(av_rev_growth = sum(rev_growth_weighted*get(sub_group), na.rm=T)),
                                    by= .(age_product, year)]
  age_rev_growth<-age_rev_growth %>% filter(year!=2009 & !is.na(age_product))
  
  ggplot(age_rev_growth, aes(x = year, y = av_rev_growth*100, color = age_product)) +
    geom_line() + scale_y_continuous(labels = scales::label_percent(scale = 1)) +
    scale_x_continuous(breaks =  seq(2010, 2021, by = 1)) +  
    labs(title = "Weighted economy-level revenue growth by product age",  subtitle = paste0("Group: ", sub_group), x = element_blank(),  y = "Revenue growth rate", color = 'type') +
    theme_classic() +theme(legend.position = 'bottom')
  ggsave(paste0(output_dir, sub_group, 'economy_weighted_age_av_rev_growth.png'),  width=6, height = 5)
}

for (sub_group in sub_groups) {
  subgroup="incumbent"
  age_rev_growth= product_firm_data %>% group_by(year) %>% filter(get(sub_group)) %>% summarise(av_rev_growth = mean(firm_product_quality_index, na.rm=T)) 
  
  ggplot(age_rev_growth, aes(x = year, y = av_rev_growth*100)) +
    geom_line() + scale_y_continuous(labels = scales::label_percent(scale = 1)) +
    scale_x_continuous(breaks =  seq(2010, 2021, by = 1)) +  
    labs(title = "Average firm-product quality match estimate throguh time",  subtitle = paste0("Group: ", sub_group), x = element_blank(),  y = "Match quality index", color = 'type') +
    theme_classic() +theme(legend.position = 'bottom')
  ggsave(paste0(output_dir, sub_group, 'time_fpqi.png'),  width=6, height = 5)
}



n_new_products= product_firm_data %>% group_by(year, age_product) %>% summarise(count=n())
n_new_products= n_new_products %>% group_by(year) %>% mutate(share= count/sum(count, na.rm = T))

ggplot(subset(n_new_products, age_product=="new"), aes(x = year, y = share*100, color = age_product)) +
  geom_line() + scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  scale_x_continuous(breaks =  seq(2009, 2021, by = 3)) +  
  labs(title = "Share of new producs in yearly PRODCOM cohort", subtitle = "'New' defined as the PC8 code appearing in the last 4 years", x = element_blank(),  y = "Share", color = 'type') +
  theme_classic() +theme(legend.position = 'bottom')
ggsave(paste0(output_dir, 'Share_new_products.png'),  width=6, height = 5)

#Alex HHI_graph
HHI_graph = product_firm_data[, .(HHI = mean(HHI, na.rm = T), rev = sum(rev, na.rm = T)), by =.(firmid, year)]
HHI_graph = HHI_graph[, .('unweighted' = mean(HHI, na.rm = T), 'rev. weighted' = sum(HHI*rev/sum(rev,na.rm = T),na.rm= T)), by = year]
HHI_graph = melt(HHI_graph, id.vars = 'year', measure.vars = c('unweighted',  'rev. weighted')) %>% rename(type = variable) 

ggplot(HHI_graph, aes(x = year, y = value*100, color = type)) +
  geom_line() + scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  scale_x_continuous(breaks =  seq(2009, 2021, by = 3)) +  
  labs(title = "Within Firm Product HHI", x = element_blank(),  y = "HHI value", color = 'type') +
  theme_classic() +theme(legend.position = 'bottom')
ggsave(paste0(output_dir, 'HHI_over_time_jul.png'), height = 5.33, width = 9)

#HHI revenue_growth graph
HHI_rev_growth <- product_firm_data %>% mutate(HHI_ind=ifelse(HHI>=1, "Single-product firms", "Other"))
HHI_rev_growth= HHI_rev_growth[, .(av_rev_growth = mean(rev_growth, na.rm=T)),
                               by= .(HHI_ind, year)]
HHI_rev_growth<-HHI_rev_growth %>% filter(year>2009 & !is.na(HHI_ind))

ggplot(HHI_rev_growth, aes(x = year, y = av_rev_growth*100, color = as.factor(HHI_ind))) +
  geom_line() + scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  scale_x_continuous(breaks =  seq(2010, 2021, by = 1)) +  
  labs(title = "Average revenue growth by within-firm product concentration", subtitle = "Within firm froduct measured by HHI", x = element_blank(),  y = "Av. revenue growth rate", color = 'type') +
  theme_classic() +theme(legend.position = 'bottom')
ggsave(paste0(output_dir, 'HHI_rev_growth.png'),  width=6, height = 5)


for(i in 2:19){
  product_firm_data<-product_firm_data %>% mutate(age_product=ifelse((year-code_entry_year)>i, "established", "new"))
  n_new_products= product_firm_data %>% group_by(year, age_product) %>% summarise(count=n())
  n_new_products= n_new_products %>% group_by(year) %>% mutate(share= count/sum(count, na.rm = T))
  
  assign(paste0("old_", i, "_years"), ggplot(subset(n_new_products, age_product=="new"), aes(x = year, y = share*100, color = age_product)) +
           geom_line() + scale_y_continuous(labels = scales::label_percent(scale = 1)) +
           scale_x_continuous(breaks =  seq(2009, 2021, by = 3)) +
           labs(title = paste0(i, " years"), x = element_blank(),  y = "Share", color = 'type') +
           theme_classic() +theme(legend.position = 'bottom'))
}
# 
# library(cowplot)
# for(i in 0:5){
#   assign(paste0("row", i+1), plot_grid(get(paste0("old_", 2+i, "_years")), get(paste0("old_", 3+i, "_years")), get(paste0("old_", 4+i, "_years"))))
# }
# 
# combined_plot<-plot_grid(row1, row2, row3, row4, row5, row6, ncol = 1)
# print(combined_plot)
# ggsave(paste0(output_dir, 'different_year_thresholds.png'),  width=8, height = 5)


assign(paste0('old_g'),
       (old_4_years +theme(axis.title.x  = element_blank())) |
         (old_5_years  + theme(axis.title.x = element_blank(), axis.title.y = element_blank())) |
         (old_6_years + theme(axis.title.x = element_blank(), axis.title.y = element_blank())))
assign(paste0('old_g'), get(paste0('old_g')) + plot_annotation(title = "Share of new products in cohort using different age thresholds"))


ggsave(paste0(output_dir, 'different_year_thresholds.png'),  width=8, height = 5)
