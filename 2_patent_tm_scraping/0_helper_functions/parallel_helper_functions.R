add_failed_attempt = function(id_num){
  failed_attempts = fread(paste0('0_input_data/failed_attempts_',id_num,'.csv')); failed_attempts$value = failed_attempts$value + 1
  fwrite(failed_attempts, paste0('0_input_data/failed_attempts_',id_num,'.csv'))
  print(paste('failed attempt:', failed_attempts$value[1]))
}

reset_failed_attempts = function(id_num){
  failed_attempts = fread(paste0('0_input_data/failed_attempts_',id_num,'.csv')); failed_attempts$value = 0
  fwrite(failed_attempts, paste0('0_input_data/failed_attempts_',id_num,'.csv'))
}

nodes_to_df <- function(nodes){
  # Convert XML nodes to strings before parallel processing
  nodes_str <- lapply(nodes, function(node){
    as.character(node)
  })
  
  df_list <- foreach(node_str = nodes_str) %do% {
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

scraping_iteration = function(user_name, password, id_num, type){
  siren_numbers = fread(file.path(type,'siren_numbers.csv'), colClasses = list(character = "siren"))
  failed_attempts = data.frame(value = 0); fwrite(failed_attempts, paste0('0_input_data/failed_attempts_',id_num,'.csv')) 
  while  (failed_attempts$value[1] <3){
    
    ## identify the next 2000 siren codes that haven't been solved or handed to another node yet
    siren_numbers_unsolved = siren_numbers[is.na(category)]
    output = data.table(siren = character(), category = numeric()) 
    number_list = list(siren_numbers_unsolved$siren[1: min(nrow(siren_numbers_unsolved),2000)])
    
    ## mark down the codes that this node is working on 
    siren_numbers[ siren %in% unlist(number_list), category := id_num]
    fwrite(siren_numbers, file.path(type,'siren_numbers.csv'))
    counter = 1; 
    while (counter <= length(number_list) & failed_attempts$value[1] <3){
      tryCatch({
        current_numbers = number_list[[counter]]
        arguments = paste0("(",paste(paste0('[TISI=',current_numbers,']'), collapse = ' OU '),") ET [DEPD=1990:2023]")
        system2(paste0('./',type,'/scraping_script_par.sh'), args =  c(user_name, password,shQuote(arguments), id_num))
        xml_data = read_xml(paste0('scraping_output_',id_num,'.xml'))
        
        ## if we have succeeded at doing the search 
        if (length(xml_find_all(xml_data, "//metadata")) == 1){
          xml_data <- read_xml(paste0('scraping_output_',id_num,'.xml')) %>% xml_find_all( "//result")
          #we know there are no firms with patents in this list 
          if(length(xml_data) == 0){
            output = rbind(output,data.frame(siren = current_numbers, category = 0))
            
            # we know there are firms with patents in this list (but not which one)
          }else if (length(current_numbers)!=1){ 
            midway_point = floor(length(current_numbers) / 2)
            number_list[[length(number_list)+1]] = current_numbers[1:midway_point]
            number_list[[length(number_list)+1]] = current_numbers[(midway_point+1): length(current_numbers)]
            
            # we have identified a patenting firm and know all of its patents  
          } else if (length(xml_data)!=1000){
            fwrite(nodes_to_df(xml_data), paste0(type,'/results_siren/results_', current_numbers[1], '.csv'))
            output = rbind(output,data.frame(siren = current_numbers, category = 1))
            
            # we have identified a patenting firm, and need to split its patents up by time period 
          } else{
            output = rbind(output,data.frame(siren = current_numbers, category = 2))
          }
          counter = counter+1; reset_failed_attempts(id_num)
          # if we didn't do the search or hit another error mark it down  
        }else{add_failed_attempt(id_num)}}, error = function(e){ add_failed_attempt(id_num)})
      failed_attempts = fread(paste0('0_input_data/failed_attempts_',id_num,'.csv'))
      file.remove(paste0('scraping_output_',id_num,'.xml'))
    }
    
    ## add back in identified nodes 
    siren_numbers = fread(file.path(type,'siren_numbers.csv'), colClasses = list(character = "siren")) %>% 
      merge(.,output, by = 'siren', all.x = T)
    siren_numbers = siren_numbers[,category := ifelse(category.x == id_num, category.y, category.x)] %>% select(-c(category.x, category.y))
    fwrite(siren_numbers, file.path(type,'siren_numbers.csv'))
  }
  
  ## add in the data that we managed to solve and release data points we didn't for other nodes 
  siren_numbers = fread(file.path(type,'siren_numbers.csv'), colClasses = list(character = "siren")) %>% 
    merge(.,output, by = 'siren', all.x = T)
  siren_numbers = siren_numbers[,category := ifelse(category.x == id_num, category.y, category.x)] %>% select(-c(category.x, category.y))
  fwrite(siren_numbers, file.path(type,'siren_numbers.csv'))
  
  
  ### Mark this password combo as on cooldown
  login_info = fread('0_input_data/login_info.csv')
  login_info$availability[chosen_index] = 2
  fwrite(login_info,'0_input_data/login_info.csv')
  
  ## delete extraneous files
  file.remove(paste0('cookie_',id_num,'.txt'))
  file.remove(paste0('0_input_data/failed_attempts_',id_num,'.csv'))
}


exclude_from = function(base_list, exclude_elements){
  return(base_list[!base_list %in% exclude_elements])
}