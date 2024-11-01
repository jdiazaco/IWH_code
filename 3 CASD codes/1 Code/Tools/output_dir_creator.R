output_dir_creator<-function(output_dir){
  # Create output folder if it does not exist
  if(!dir.exists(output_dir)){
    dir.create(output_dir)
  }
  
  # Create the test folder if we're in trials
  if(!final){
    output_dir<-paste0(output_dir, "test/")
    if(!dir.exists(output_dir)){
      dir.create(output_dir)
    }
  }
}

