make_summary_stats = function(data_frame, interest_vars, sub_groups, output_name){
  ## generate the basic version of the summary stats table 
  og_sub_groups<-sub_groups
  if(sub_groups!="full_sample"){
    data_frame$full_sample=T
    for(sub_group in unique(data_frame[[sub_groups]])){
      data_frame[[paste0(sub_group)]]<-(data_frame[[sub_groups]]==sub_group)
    }
    sub_groups<-c("full_sample", as.character(unique(data_frame[[sub_groups]])))
  }
  
  table_1 = rbindlist(lapply(seq_along(sub_groups),function(i){
    if (is.na(sub_groups[i])){
      data_frame = data_frame %>% filter(is.na(sub_groups[i]))
    }else{
      if (sub_groups[i]!='full_sample'){
        data_frame = data_frame[get(sub_groups[i]) == T]
      }
    }
    
    if(n_distinct(data_frame$firmid)>3){
      means = data_frame %>% summarize(across(.cols = interest_vars, ~ mean(.x[is.finite(.x)], na.rm = T))) %>% 
        mutate(var = 'mean')
      sd = data_frame %>% summarize(across(.cols = interest_vars, ~ sd(.x[is.finite(.x)], na.rm = T))) %>% 
        mutate(var = 'sd')
      na_inf = data_frame %>% summarize(across(.cols = interest_vars, ~ (sum(is.na(.x) | is.infinite(.x) | is.nan(.x))/nrow(data_frame)))) %>% 
        mutate(var = 'NAs')
      empty = data_frame %>% summarize(across(.cols=interest_vars, ~ (sum(.x==0 | .x=="" | grepl("^\\s*$", .x), na.rm=TRUE)/nrow(data_frame)))) %>% 
        mutate(var="empty")
      p10 = data_frame %>% summarize(across(.cols=interest_vars, ~ quantile(.x, 0.1, na.rm = T))) %>% 
        mutate(var="p10")
      p90 = data_frame %>% summarize(across(.cols=interest_vars, ~ quantile(.x, 0.9, na.rm = T))) %>% 
        mutate(var="p90")

    }else{
      means=-999
      sd=-999
      na_inf=-999
      empty=-999
      p10=-999
      p90=-999
      n_distinct=-999
    }
    n_distinct = data.frame(n_distinct = c(if (n_distinct(data_frame$firmid)>3) n_distinct(data_frame$firmid) else -999,NULL, NULL, NULL))
    sample_size = data.frame(sample_size = c(if (nrow(data_frame)>3) nrow(data_frame) else -999,NULL, NULL, NULL))
    output = cbind(rbind(means, sd, na_inf, empty, p10, p90), n_distinct, sample_size) %>% mutate(sample = sub_groups[i])
    }
  ))
  write.xlsx(table_1, paste0(output_dir, output_name, '.xlsx'), rowNames = F)
  
  file_conn = file(paste0(output_dir, output_name, '.tex'), 'w')
  ##preamble = '\\documentclass{article}\n\\begin{document}\n'
  column_format = paste0('l ', paste(rep('c', length(sub_groups)), collapse = " "))
  column_heads = paste0(' & ', paste(sub_groups, collapse = ' &'))
  table_head = paste0('\\begin{table}[h]\n\\centering\n\\begin{tabular}{',
                      column_format, '}\n\\hline\n',column_heads, '\\\\\n\\hline\n'
  )

  #cat(preamble, file = file_conn)
  cat(table_head, file = file_conn)
  for (j in seq_along(interest_vars)){
    for (k in seq_along(sub_groups)){
      k_mean =  round(table_1 %>% filter(sample == sub_groups[k], var == 'mean') %>% pull(interest_vars[j]),2)
      k_sd =   round(table_1 %>% filter(sample == sub_groups[k], var == 'sd') %>% pull(interest_vars[j]),2)
      j_sample_size = table_1 %>% filter(sample == sub_groups[k], var == 'mean') %>% pull(sample_size)

      if (k == 1){
        value_row =paste0(interest_vars[j], ' & ', k_mean)
        sd_row = paste0(' & (', k_sd, ")")
        sample_size = paste0('sample size & ', j_sample_size)

      } else{
        value_row = paste0(value_row, ' & ', k_mean)
        sd_row = paste0(sd_row,' & (', k_sd, ") ")
        sample_size = paste0(sample_size,' & ',  j_sample_size)
      }
    }
    value_row = paste0(value_row, '\\\\'); sd_row = paste0(sd_row, '\\\\')
    blank_row = paste0(paste(rep('&', length(sub_groups)), collapse = " "),'\\\\')

    cat(value_row, file = file_conn, sep = '\n'); cat(sd_row, file = file_conn, sep = '\n')
    cat(blank_row, file = file_conn, sep = '\n')
  }

  cat(paste0(sample_size,'\\\\'), file = file_conn, sep = '\n')
  cat('\\hline\n\\end{tabular}\n\\end{table}', file = file_conn)

  #cat('\\end{document}\n', file = file_conn)
  close(file_conn)
  
  ### Horizontal descriptives
  
  stats_adj<-c("Mean", "SD", "NAs", "0 or blank", "10th Pct.", "90th Pct.", "N. Units", "N. Obs.")
  stats<-c("mean", "sd", "NAs", "empty", "p10", "p90")
  
  file_conn = file(paste0(output_dir, output_name, '_horizontal.tex'), 'w')
  ##preamble = '\\documentclass{article}\n\\begin{document}\n'
  column_format = paste0('l ', 'l', paste(rep('r', length(stats_adj)), collapse = " "))
  column_heads = paste0(' & Sample &', paste(stats_adj, collapse = ' &'))
  table_head = paste0('\\begin{table}[h]\n  \\caption{Summary Statistics by ', og_sub_groups, ' }\n  \\centering\n\\begin{tabular}{',
                      column_format, '}\n\\hline\n',column_heads, '\\\\\n\\hline\n'
  )
  
  # #cat(preamble, file = file_conn)
  # cat(table_head, file = file_conn)
  # for (j in seq_along(interest_vars)){
  #   for (k in seq_along(stats)){
  #     k_mean =  round(table_1 %>% filter(var == stats[k], sample == 'full_sample') %>% pull(interest_vars[j]),2)
  #     # k_sd =   round(table_1 %>% filter(var == stats[k], sample == 'full_sample') %>% pull(interest_vars[j]),2)
  #     j_sample_size = table_1 %>% filter(var == stats[k], sample == 'full_sample') %>% pull(sample_size)
  #     
  #     if (k == 1){
  #       value_row =paste0(gsub("_", "\\\\_", interest_vars[j]), ' & ', k_mean)
  #       # sd_row = paste0(' & (', k_sd, ")")
  #       # sample_size = paste0('sample size & ', j_sample_size)
  #       
  #     } else{
  #       value_row = paste0(value_row, ' & ', k_mean)
  #       # sd_row = paste0(sd_row,' & (', k_sd, ") ")
  #       # sample_size = paste0(sample_size,' & ',  j_sample_size)
  #     }
  #     
  #     if(k==length(stats)){
  #       value_row = paste0(value_row, ' & ', j_sample_size)
  #       
  #     }
  #   }
  #   value_row = paste0(value_row, '\\\\')#; sd_row = paste0(sd_row, '\\\\')
  #   # blank_row = paste0(paste(rep('&', length(sub_groups)), collapse = " "),'\\\\')
  #   
  #   cat(value_row, file = file_conn, sep = '\n')#; cat(sd_row, file = file_conn, sep = '\n')
  #   # cat(blank_row, file = file_conn, sep = '\n')
  # }
  # 
  # # cat(paste0(sample_size,'\\\\'), file = file_conn, sep = '\n')
  # cat('\\hline\n\\end{tabular}\n\\end{table}', file = file_conn)
  # 
  # #cat('\\end{document}\n', file = file_conn)
  # close(file_conn)
  
  #cat(preamble, file = file_conn)
  cat(table_head, file = file_conn)
  
  for (j in seq_along(interest_vars)){
    for (x in seq_along(sub_groups)){
      # cat(paste0(sub_groups[x], paste(rep('&', length(stats_adj)), collapse = " "), "\\\\\n"), file = file_conn)
      
    for (k in seq_along(stats)){
        k_mean =  round(table_1 %>% filter(var == stats[k], sample == sub_groups[x]) %>% pull(interest_vars[j]),4)
        # k_sd =   round(table_1 %>% filter(var == stats[k], sample == 'full_sample') %>% pull(interest_vars[j]),2)
        j_n_firms = table_1 %>% filter(var == stats[k], sample == sub_groups[x]) %>% pull(n_distinct)
        j_sample_size = table_1 %>% filter(var == stats[k], sample == sub_groups[x]) %>% pull(sample_size)
        
        if (k == 1){
          value_row =paste0(gsub("_", "\\\\_", interest_vars[j]), ' & ', gsub("_", "\\\\_", sub_groups[x]), ' & ', k_mean)
          # sd_row = paste0(' & (', k_sd, ")")
          # sample_size = paste0('sample size & ', j_sample_size)
          
        } else{
          value_row = paste0(value_row, ' & ', k_mean)
          # sd_row = paste0(sd_row,' & (', k_sd, ") ")
          # sample_size = paste0(sample_size,' & ',  j_sample_size)
        }
        
        if(k==length(stats)){
          value_row = paste0(value_row, ' & ', j_n_firms, "&", j_sample_size)
        }
    }
      value_row = paste0(value_row, '\\\\')#; sd_row = paste0(sd_row, '\\\\')
      # blank_row = paste0(paste(rep('&', length(sub_groups)), collapse = " "),'\\\\')
      
      cat(value_row, file = file_conn, sep = '\n')#; cat(sd_row, file = file_conn, sep = '\n')
      # cat(blank_row, file = file_conn, sep = '\n')
      
      
    }
    cat('\\hline\n', file = file_conn)
    
  }
  
  # cat(paste0(sample_size,'\\\\'), file = file_conn, sep = '\n')
  cat('\\hline\n\\end{tabular}\n\\end{table}', file = file_conn)
  
  #cat('\\end{document}\n', file = file_conn)
  close(file_conn)
  
  
}
