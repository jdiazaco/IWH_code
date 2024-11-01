# We can:
# 1. Use 8-digit (prodcom) or 10-digit (prodfra) product codes. Based on this, we bring in 8 or 10 digit product code harmonization to create the Prodcom database
# 2. EAP has three different categorizations for prodfra codes: prodfra nested in prodcon, prodfra outside of prodcom or prodfra not nested, but with Prodcom information.
# We can decide to use only prodfra codes nested in prodcom or use all prodfra codes
# 3. Exclude certain industries (e.g. public utilities industries)
# We do the above by setting parameters below. The name of the files we will save changes depending on those parameters


parameters<-function(prodfra_or_pcc8, only_prodfra_in_prodcom, exclude_industries=NULL){
  filter<-paste0(if(prodfra_or_pcc8=="prodfra") "10_digit" else if(prodfra_or_pcc8=="pcc8") "8_digit" else prodfra_or_pcc8, 
                 "_",
                 if(only_prodfra_in_prodcom) "only_prodfra_in_prodcom" else "all_prodfra")
  # label<<-paste0(if(prodfra_or_pcc8=="prodfra") "using 10-digit codes" else if(prodfra_or_pcc8=="pcc8") "using 8-digit codes" else prodfra_or_pcc8, 
  #               ",",
  #               if(only_prodfra_in_prodcom) " only prodfra codes in prodcom " else " all prodfra codes ")
  filter_indicator<<-filter
  
  if(!is.null(exclude_industries)){
    filter_indicator<<-paste0(filter, if(exclude_industries) "_exclude_industries" else "_not_exclude_industries")
    # label<<-paste0(label, if(exclude_industries) "and excluding utilities and wholesale trade" else "and all industries")
  }
  
  # return(filter)
  # return(label)
}
