
# setup problem -------------------------------------------------------------------
rm(list = ls())
library(tidyverse)
library(rstudioapi)
library(data.table)
library(tidyverse)
library(haven)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# generate biannual data  ---------------------
product_data = fread('product_data.csv') %>% arrange(firmid,year)


product_data[, rev_bar :=.5*(rev_l + rev)]
product_data[, rev_growth := abs((rev-rev_l)/ rev_bar)]
product_data[, rev_share := rev_bar/sum(rev_bar),
             by = .(firmid,year) ]
firm_in = product_data[, .(rev_growth=sum(rev_growth*rev_share),
                              rev_bar = sum(rev_bar)), by = .(firmid, year)]
firm_in[, rev_share:= rev_bar/sum(rev_bar), by= year]

