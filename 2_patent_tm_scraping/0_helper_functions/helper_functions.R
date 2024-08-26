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


scraping_iteration_dates_only = function(user_name, password, type){
  failed_attempts = data.frame(value = 0); fwrite(failed_attempts, file.path(type,'failed_attempts.csv'))

  while (fread(file.path(type,'failed_attempts.csv')) %>% pull(value) <3 &
         fread(file.path(type, 'dates.csv')) %>% pull(completed) %>% min() == 0){

    dates <- fread(file.path(type,'dates.csv')) %>% mutate(across(c(start_dates, end_dates), ~as.Date(., format = "%m/%d/%y")))
    i = which(dates$completed == 0)[1]
    if (type =="1_patent"){
    date_string =  paste0('[DEPD=', format(dates$start_dates[i], "%Y%m%d"), ":",
                        format(dates$end_dates[i], "%Y%m%d"),']')
    }else{
      date_string =  paste0('[ApplicationDate=', format(dates$start_dates[i], "%Y%m%d"), ":",
                            format(dates$end_dates[i], "%Y%m%d"),']')
    }
    tryCatch({
      # Construct and execute the system call
      system2('0_input_data/scraping_script.sh', args =  c(user_name, password,type,date_string))
      xml_data = read_xml(paste0(type,'_scraping_output.xml'))

      ## if we don't return data mark search as a failure
      if (length(xml_find_all(xml_data, "//metadata")) != 1){
        add_failed_attempt(type)
      }else{
        reset_failed_attempts(type)
        nodes <- xml_find_all(xml_data, "//result")

        ##if we don't need to split up the time period
        if (length(nodes) != 10000){
          fwrite(nodes_to_df(nodes), paste0(type, '/results_time/','results_', i, '.csv'))
        } else{
          midpoint = ymd(as.Date(floor((as.numeric(dates$start_dates[i]) + as.numeric(dates$end_dates[i])) / 2)))
          dates = add_row(dates, start_dates = dates$start_dates[i], end_dates = midpoint, completed = 0)
          dates = add_row(dates, end_dates = dates$end_dates[i], start_dates = midpoint, completed = 0)
        }
        dates$completed[i] = 1
        fwrite(dates,file.path(type,'dates.csv'))
        # Print progress
        print(i/nrow(dates))
      }
      }, error = function(e) {
        add_failed_attempt(type)
    })
  }
}






