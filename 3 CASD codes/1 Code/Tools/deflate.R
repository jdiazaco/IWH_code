deflate<-function(data, NACE_var, deflated_vars, base_year){
  deflators <- fread("C:/Users/Public/1. Microprod/1. Microprod-H2020/NEW_INFRASTRUCTURE/Infra/AuxData/deflators.csv")
  deflators <- deflators %>% filter(cc=="FR") %>% select(DEFind, year, pnv)
  nace_DEFind <- fread("C:/Users/Public/1. Microprod/1. Microprod-H2020/NEW_INFRASTRUCTURE/Infra/MetaData/nace_DEFind.conc", colClasses = c('character'))
  
  deflators <- deflators %>% group_by(DEFind) %>% mutate(pnv_start= pnv/pnv[year==base_year]) # Base year 2009
  
  nace_DEFind$nace<-str_pad(nace_DEFind$nace, width = 4, side="left", pad="0")
  nace_DEFind$nace<-substr(nace_DEFind$nace, 1,2)
  nace_DEFind<-unique(nace_DEFind)

  data[[NACE_var]]<-as.character(data[[NACE_var]])
  data[[paste0(NACE_var, "_og")]]<-data[[NACE_var]]
  data[[NACE_var]]<-str_pad(data[[NACE_var]], width = 4, side="left", pad="0")
  data[[NACE_var]]<-substr(data[[NACE_var]], 1,2)
  
  
  data <- merge(data,nace_DEFind,by.x=NACE_var, by.y='nace',all.x=T)
  data <- merge(data,deflators %>% select(year, pnv_start, DEFind),by=c('DEFind','year'),all.x=T)
  
  for(i in deflated_vars){
    var_og<-paste0(i, "_og")
    data[[var_og]]<-data[[i]]
    data[[i]]<-data[[i]]/(data$pnv_start)
  }
  data[[NACE_var]]<-data[[paste0(NACE_var, "_og")]]
  data[[paste0(NACE_var, "_og")]]<-NULL
  return(data)
}