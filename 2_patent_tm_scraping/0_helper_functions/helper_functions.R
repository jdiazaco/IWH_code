

# process data ------------------------------------------------------------
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

NA_string_collapse =function(x, delimiter, elim_empty_strings){
  if (elim_empty_strings){ # if we want to remove blanks 
    if(!all(is.na(x) | x == "")){
      return(x %>% .[!is.na(.) & x != ""] %>% paste(., collapse = delimiter))
    }else{
      return(NA_character_)
    }
  }else{ #if we want to remove only NA
    if(!all(is.na(x))){ 
      return(x %>% .[!is.na(.)] %>% paste(., collapse = delimiter))
    }else{
      return(NA_character_)
    }
  }
}



# scrape data -------------------------------------------------------------
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

scraping_siren_version = function(node_num, user_name, password, type){
  siren_path = paste0('data/2_patent_tm_scraping/2_working/',type,'_siren_numbers_',node_num,'.csv')
  counter_path = paste0('data/2_patent_tm_scraping/2_working/counter',node_num,'.RDS')
  number_path = paste0('data/2_patent_tm_scraping/2_working/number_list',node_num,'.RDS')
  scraping_path = 'IWH_code/2_patent_tm_scraping/0_helper_functions/scraping_script.sh'
  siren_numbers = fread(siren_path, colClasses = list(character = "siren"))
  failed_attempts = 0
  while(failed_attempts < 3 & any(is.na(siren_numbers$category))){
    # set up number list to check & counter for what sublist we're on 
    if (file.exists(counter_path) & file.exists(number_path)){ # if we have number list from last attempt
      counter <- readRDS(counter_path); number_list <- readRDS(number_path)
    }else{
      number_list = siren_numbers[is.na(category)][['siren']] %>% .[1:min(nrow(.), 2000)] %>% list()
      counter <- 1
    }
    ## go through the sublists looking for patents 
    output = data.frame(siren = character(), category = numeric()) 
    while (counter <= length(number_list) & failed_attempts <3){
      current_numbers = number_list[[counter]]
      if (type == "patent"){
        arguments = paste0("(",paste(paste0('[DESI=',current_numbers,']'), collapse = ' OU '),") ET [DEPD=1990:2023]")
      }else{
        arguments = paste0("(",paste(paste0('[ApplicantIdentifier=',current_numbers,']'), collapse = ' OU '),") ET [ApplicationDate=1990:2023]")
      }
      system2(scraping_path, args = c(user_name, password,type,shQuote(arguments),node_num))
      scraping_result = tryCatch({
        scraping_result = read_xml(paste0('data/2_patent_tm_scraping/2_working/scraping_output', node_num,'.xml'))
        if(length(xml_find_all(scraping_result, "//metadata")) != 1){scraping_result = 1}; scraping_result = scraping_result
      }, error = function(e){print('non-readable'); scraping_result = 1})
      
      if(typeof(scraping_result) == "double"){failed_attempts = failed_attempts + 1; print(paste0('failed ', failed_attempts))
      }else{ # we successfully scraped
        scraping_result = scraping_result %>% xml_find_all( "//result")
        if (length(scraping_result)==0){  #we know there are no firms with patents in this list 
          output = rbind(output,data.frame(siren = current_numbers, category = 0))
        }else if (length(current_numbers)!=1){  # we know there are firms with patents in this list (but not which one)
          midway_point = floor(length(current_numbers) / 2)
          number_list[[length(number_list)+1]] = current_numbers[1:midway_point]
          number_list[[length(number_list)+1]] = current_numbers[(midway_point+1): length(current_numbers)]
        }else if (length(scraping_result)!=10000){  # we have identified a patenting firm and know all of its patents  
          paste0('data/2_patent_tm_scraping/2_working/', type,"_siren/results", current_numbers[1], '.csv') %>%
            fwrite(nodes_to_df(scraping_result),.)
          output = rbind(output,data.frame(siren = current_numbers, category = 1))
        }else{  #we have identified a patenting firm, and need to split its patents up by time period 
          output = rbind(output,data.frame(siren = current_numbers, category = 2))
        }
        counter = counter+1; failed_attempts = 0
        print(paste("counter:",counter, "; number list length:", length(number_list)))
      }
    }
    ## if we exited bc we hit 3 failed attempts, save the counter and number_list; otherwise remove
    if (failed_attempts == 3){saveRDS(counter, counter_path); saveRDS(number_list, number_path); print('failed out')
    }else{file.remove(counter_path); file.remove(number_path); print('successfully finished')}
    siren_numbers = merge(siren_numbers, output, by = 'siren', all.x = T)
    siren_numbers = siren_numbers[,category := ifelse(is.na(category.x), category.y, category.x)] %>% select(-c(category.x, category.y))
    fwrite(siren_numbers, siren_path)
  }
}

scraping_date_version = function(node_num, user_name, password, type){
  failed_attempts = 0
  date_path = paste0('data/2_patent_tm_scraping/2_working/',type,'_dates_',node_num,'.csv')
  dates = fread(date_path) %>% mutate(across(-completed, ~as.Date(., format = "%Y%m%d")))
  
  while (failed_attempts < 3 & min(dates$completed) == 0){
    i = which(dates$completed == 0)[1]
    date_string = paste0(dates$start_dates[i], ":",dates$end_dates[i]) %>% gsub("-", "", .)
    if (type =="patent"){date_string =  paste0('[DEPD=', date_string,']')}else{date_string =  paste0('[ApplicationDate=', date_string,']')}
    system2('IWH_code/2_patent_tm_scraping/0_helper_functions/scraping_script.sh', args =  c(user_name, password,type,date_string, node_num))
    
    output = tryCatch({
      output = read_xml(paste0('data/2_patent_tm_scraping/2_working/scraping_output', node_num,'.xml'))
      if(length(xml_find_all(output, "//metadata")) != 1){output = 1}; output = output
      }, error = function(e){print('non-readable'); output = 1})
    
    if(typeof(output) == "double"){failed_attempts = failed_attempts + 1; print(paste0('failed ', failed_attempts))}else{
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

check_progress = function(type){
  dirlist = dir('data/2_patent_tm_scraping/2_working/') %>% .[grepl(paste0(type,"_siren_numbers_"),.)] 
  siren_numbers_stubs = lapply(dirlist,function(stub){
                        file_name = paste0('data/2_patent_tm_scraping/2_working/',stub)    
                        output = fread(file_name, colClasses = list(character = "siren"))
                        return(output)}) %>% rbindlist() %>% unique() %>% as.data.table()
 
  siren_numbers = fread(paste0('data/2_patent_tm_scraping/2_working/',type,'_siren_numbers.csv'),
                  colClasses = list(character = "siren")) %>% unique() %>%
                  merge(., siren_numbers_stubs, all.x = T, by = 'siren') %>%
                  .[,category := ifelse(is.na(category.x), category.y, category.x)] %>%
                  select(-c(category.x, category.y))
  
  return(nrow(siren_numbers[!is.na(category)])/ nrow(siren_numbers))
}


