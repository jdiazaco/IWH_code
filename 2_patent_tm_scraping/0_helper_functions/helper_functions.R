exclude_from = function(base_list, exclude_elements){
  return(base_list[!base_list %in% exclude_elements])
}

word_match <- function(denm, invne){
  tryCatch({
  ## returns true if all words in denm are present in invne
  denm_words <- unlist(strsplit(denm, "\\s+"))
  invne_words <- unlist(strsplit(invne, "\\s+"))
  answer = all(sapply(denm_words, function(word) any(grepl(word, invne_words))))
  },error = function(e){F})

}

str_cleaning = function(x){
  x %>% str_trim(.) %>% str_replace(., "&apos;", "'") %>% toupper(.) %>% stri_trans_general(.,"Latin-ASCII")
}

add_failed_attempt = function(type){
  failed_attempts = fread(file.path(type,'failed_attempts.csv')); failed_attempts$value = failed_attempts$value + 1
  fwrite(failed_attempts, file.path(type,'failed_attempts.csv'))
  print(paste('failed attempt:', failed_attempts))
}

reset_failed_attempts = function(type){
  failed_attempts = fread(file.path(type,'failed_attempts.csv')); failed_attempts$value = 0
  fwrite(failed_attempts, file.path(type,'failed_attempts.csv'))
}

nodes_to_df <- function(nodes){
  # Convert XML nodes to strings before parallel processing
  nodes_str <- lapply(nodes, function(node){
    as.character(node)
  })
  
  # Export the serialized nodes
  
  df_list <-foreach(node_str = nodes_str) %do% {
    # Deserialize the XML node from string
    node <- read_xml(node_str)
    
    # Process the node as before
    fields <- xml_find_all(node, "./fields/field")
    field_names <- vector("list", length(fields))
    field_values <- vector("list", length(fields))
    
    for (j in seq_along(fields)) {
      field_names[[j]] <- xml_attr(fields[j], "name")
      values_nodes <- xml_find_all(fields[j], ".//value")
      field_values[[j]] <- if (length(values_nodes) > 1) {
        paste(xml_text(values_nodes), collapse = ", ")
      } else {
        xml_text(values_nodes)
      }
    }
    
    as.data.frame(t(setNames(unlist(field_values, recursive = FALSE), unlist(field_names))), stringsAsFactors = FALSE)
  }
  # Combine all data frames into one
  result <- bind_rows(df_list)
  return(result)
}

scraping_iteration = function(user_name, password, type){
  siren_numbers = fread(file.path(type,'siren_numbers.csv'), colClasses = list(character = "siren"))
  failed_attempts = data.frame(value = 0); fwrite(failed_attempts, file.path(type,'failed_attempts.csv')) 
  
  while  (failed_attempts$value[1] <3){
    ## check to see if we have number lists / counters left over from last attempt
    if (file.exists(file.path(type,'counter.RDS')) & file.exists(file.path(type,'number_list.RDS'))){
      counter <- readRDS(file.path(type,'counter.RDS'))
      number_list <- readRDS(file.path(type,'number_list.RDS'))
    }else{
      siren_numbers_unsolved <- siren_numbers[is.na(category)]
      number_list <- list(siren_numbers_unsolved$siren[1:min(nrow(siren_numbers_unsolved), 2000)])
      counter <- 1
    }
    output = data.frame(siren = character(), category = numeric())
    while (counter <= length(number_list) & failed_attempts$value[1] <3){
      print(nrow(siren_numbers %>% filter(!is.na(category))) / nrow(siren_numbers))
      tryCatch({
        current_numbers = number_list[[counter]]
        if (type == "1_patent"){
          arguments = paste0("(",paste(paste0('[DESI=',current_numbers,']'), collapse = ' OU '),") ET [DEPD=1990:2023]")
        } else{
        arguments = paste0("(",paste(paste0('[ApplicantIdentifier=',current_numbers,']'), collapse = ' OU '),") ET [ApplicationDate=1990:2023]")
        }
        system2('0_input_data/scraping_script.sh', args =  c(user_name, password,type,shQuote(arguments)))
        xml_data = read_xml(paste0(type,'_scraping_output.xml'))
        
        ## if we have succeeded at doing the search 
        if (length(xml_find_all(xml_data, "//metadata")) == 1){
          xml_data <- read_xml(paste0(type,'_scraping_output.xml')) %>% xml_find_all( "//result")
          #we know there are no firms with patents in this list 
          if(length(xml_data) == 0){
            output = rbind(output,data.frame(siren = current_numbers, category = 0))
            
            # we know there are firms with patents in this list (but not which one)
          }else if (length(current_numbers)!=1){ 
            midway_point = floor(length(current_numbers) / 2)
            number_list[[length(number_list)+1]] = current_numbers[1:midway_point]
            number_list[[length(number_list)+1]] = current_numbers[(midway_point+1): length(current_numbers)]
            
            # we have identified a patenting firm and know all of its patents  
          } else if (length(xml_data)!=10000){
            fwrite(nodes_to_df(xml_data), paste0(type,'/results_siren/results_', current_numbers[1], '.csv'))
            output = rbind(output,data.frame(siren = current_numbers, category = 1))
            
            # we have identified a patenting firm, and need to split its patents up by time period 
          } else{
            output = rbind(output,data.frame(siren = current_numbers, category = 2))
          }
          counter = counter+1; reset_failed_attempts(type)
          
          # if we didn't do the search or hit another error mark it down  
        }else{add_failed_attempt(type)}}, error = function(e){ add_failed_attempt(type)})
      failed_attempts = fread(file.path(type,'failed_attempts.csv'))
      file.remove(paste0(type,'_scraping_output.xml'))
    }
    ## if we exited bc we hit 3 failed attempts, save the counter and number_list
    if (failed_attempts$value[1] == 3){
      saveRDS(counter, file.path(type,'counter.RDS')); saveRDS(number_list, file.path(type,'number_list.RDS'))
    }else{
      file.remove(file.path(type,'counter.RDS')); file.remove(file.path(type,'number_list.RDS'))
      print(paste0('took ', counter -1, ' iterations'))
    }
    siren_numbers = merge(siren_numbers, output, by = 'siren', all.x = T)
    siren_numbers = siren_numbers[,category := ifelse(is.na(category.x), category.y, category.x)] %>% select(-c(category.x, category.y))
    fwrite(siren_numbers, file.path(type,'siren_numbers.csv'))
    print(nrow(siren_numbers %>% filter(!is.na(category))) / nrow(siren_numbers))
  }
  }

scraping_iteration_dates_only = function(node_num, user_name, password, type){
  failed_attempts = 0
  date_path = paste0('data/2_patent_tm_scraping/2_working/',type,'_dates_',node_num,'.csv')
  dates = fread(date_path) %>% mutate(across(-completed, ~as.Date(., format = "%Y%m%d")))
  
  while (failed_attempts < 3 & min(dates$completed) == 0){
    i = which(dates$completed == 0)[1]; date_string = paste0(dates$start_dates[i], ":",dates$end_dates[i]) %>% gsub("-", "", .)
    if (type =="patent"){date_string =  paste0('[DEPD=', date_string,']')}else{date_string =  paste0('[ApplicationDate=', date_string,']')}
    system2('IWH_code/2_patent_tm_scraping/0_helper_functions/scraping_script.sh', args =  c(user_name, password,type,date_string, node_num))
    
    output = tryCatch({
      output = read_xml(paste0('data/2_patent_tm_scraping/2_working/scraping_output', node_num,'.xml'))}, 
      error = function(e){output = 1})
    
    if(typeof(output) == "double"){failed_attempts = failed_attempts + 1; print('failed')}else{
      failed_attempts = 0
      nodes <- xml_find_all(output, "//result")
      
      if (length(nodes) != 10000){
        fwrite(nodes_to_df(nodes), paste0('data/2_patent_tm_scraping/2_working/',type, "_time/results_",
        paste0(dates$start_dates[i], "_",dates$end_dates[i]) %>% gsub("-", "", .),".csv"))
      } else{
        midpoint = ymd(as.Date(floor((as.numeric(dates$start_dates[i]) + as.numeric(dates$end_dates[i])) / 2)))
        dates = add_row(dates, start_dates = dates$start_dates[i], end_dates = midpoint, completed = 0)
        dates = add_row(dates, end_dates = dates$end_dates[i], start_dates = midpoint, completed = 0)
      }
      dates$completed[i] = 1; fwrite(dates,date_path)
      print(i/nrow(dates))
    }
  }
}






