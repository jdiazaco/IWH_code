description<-function(object, description){
  # object<-deparse(substitute(object))
  write(paste0(object, ": ", description), append = TRUE,file = paste0(output_dir, "ReadMe.txt"))
}
