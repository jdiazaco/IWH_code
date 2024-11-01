# libraries
rm(list = ls())
gc()
packages = c('data.table', 'haven', 'readxl', 'openxlsx','stringr', 'readr', 'dplyr', 
             'tidyverse', 'zoo', 'reshape2','rstudioapi', "plm", 'foreign', "fixest", 
             'data.table', 'haven', 'stringr', 'readr', 'dplyr',
             'ggplot2', 'tidyverse', 'rstudioapi', 'zoo', 'reshape2',
             'patchwork', 'latex2exp', "RColorBrewer", "texreg", "stargazer", "modelsummary", "broom", "fixest",
             "xtable")
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
#Packages loading
invisible(lapply(packages, library, character.only = TRUE))

#Establish whether this is the final run of the code or not
set.seed(123)
final<-T

#Bring tools
tools_dir <- 'C:/Users/nb/Dropbox/Reallocation - shared folder/Code/IWH_code/3 CASD codes/1 Code/Tools/'
source(paste0(tools_dir, "description.R"))
source(paste0(tools_dir, "deflate.R"))
source(paste0(tools_dir, "summary stats helper.R"))
source(paste0(tools_dir, "parameters.R"))
source(paste0(tools_dir, "output_dir_creator.R"))
# source("C:/Users/Public/1. Microprod/1. Microprod-H2020/NEW_INFRASTRUCTURE/Infra/Rtools/nace_conc.R")


# set up directories 
raw_dir = "C:/Users/Public/1. Microprod/0. Raw data processing/Data/"
setwd('C:/Users/nb/Dropbox/Reallocation - shared folder/Code/IWH_code/3 CASD codes/2 Data/')
code_dir<-"C:/Users/nb/Dropbox/Reallocation - shared folder/Code/IWH_code/3 CASD codes/1 Code/"
output_dir<-"C:/Users/nb/Dropbox/Reallocation - shared folder/Code/IWH_code/3 CASD codes/3 Output/"

# raw_dir = "C:/Users/Public/1. Microprod/0. Raw data processing/Data/"
# setwd('C:/Users/NEWPROD_J_DIAZ-AC/Documents/Reallocation/6 Publish/2 Data/')
# code_dir<-"C:/Users/NEWPROD_J_DIAZ-AC/Documents/Reallocation/6 Publish/1 Code/"
# output_dir<-"C:/Users/NEWPROD_J_DIAZ-AC/Documents/Reallocation/6 Publish/3 Output/"


# Create output_dir
output_dir_creator(output_dir)

# Set parameters for prodfra-pcc8 and excluded industries
exclude_industries<-TRUE
prodfra_or_pcc8<-"prodfra"
only_prodfra_in_prodcom<-FALSE
parameters(prodfra_or_pcc8, only_prodfra_in_prodcom, exclude_industries)

# Set start and end years
start<-2009
end<-2021
