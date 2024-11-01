"
This file creates a variable with the birth year of the firm according to br information

It was written as a module of part 0 of the script 'a. Data preparation.R'

It assumes sbs_br_combined is available in the environment with variable Start_Ent

It returns sbs_br_combined with variable firm_birth_year

It looks for different cases on how the birth year can be enconded in Start_Ent

After doing that, it selects the most likely firm_birth_year based on the number of times it is listed as the year indicated in Start_Ent

Author: Julián Díaz-Acosta
Last update: 29/08/2024
"


# Fill leading zeroes
sbs_br_combined$Start_Ent<-str_pad(sbs_br_combined$Start_Ent, 8, side = "left", pad="0")
table(nchar(sbs_br_combined$Start_Ent))

# Create variables for analysis
sbs_br_combined$date_length<-nchar(sbs_br_combined$Start_Ent)
sbs_br_combined$firm_birth_year <- substr(sbs_br_combined$Start_Ent, nchar(sbs_br_combined$Start_Ent)-3, nchar(sbs_br_combined$Start_Ent))
sbs_br_combined$first_4_digits_birth_year <- substr(sbs_br_combined$Start_Ent, 1, 4)
sbs_br_combined$digits_3_4_birth_year <- substr(sbs_br_combined$Start_Ent, 3, 4)
sbs_br_combined$digits_5_6_birth_year <- substr(sbs_br_combined$Start_Ent, 5, 6)

# # Check the distribution of years. They are not consistent outside the 1902-2021 period
# table(sbs_br_combined$firm_birth_year)
# 
# ## Begin by styding results above the end_year baseline
# test<-sbs_br_combined[firm_birth_year>end]
# table(substr(test[first_4_digits_birth_year=="0101"]$firm_birth_year, 1,2)) #These look like years, especially helpful because they do not include 00-21 (31-99)
# table(substr(test[first_4_digits_birth_year=="0101"]$firm_birth_year, 3,4)) #These look like months (0-12)
# # Result: first_4_digits_birth_year=="0101" ~ paste0("19", substr(firm_birth_year, 1,2)),
# 
# test<-test[first_4_digits_birth_year!="0101" & firm_birth_year!="faus" &  substr(Start_Ent, 1,2)!="00"]
# table(substr(test[first_4_digits_birth_year!="0101"]$Start_Ent, 1,2)) # They are all 01
# table(substr(test[first_4_digits_birth_year!="0101"]$Start_Ent, 3,4)) #These look like years, but they include years 02-32 (02-96)
# hist(as.numeric(substr(test[first_4_digits_birth_year!="0101"]$Start_Ent, 3,4)), breaks=100 )
# table(substr(test[first_4_digits_birth_year!="0101"]$Start_Ent, 5,6)) #
# hist(as.numeric(substr(test[first_4_digits_birth_year!="0101"]$Start_Ent, 5,6)), breaks=100 )
# table(substr(test[first_4_digits_birth_year!="0101"]$Start_Ent, 7,8)) # These are most likely months  but they include years 02-32 (02-96)
# # Result: firm_birth_year=="faus" ~ NA,
# # Result: first_4_digits_birth_year!="0101" & digits_3_4_birth_year>31 & digits_5_6_birth_year<=31~ paste0("19", substr(firm_birth_year, 3,4)),
# # Result: first_4_digits_birth_year!="0101" & digits_3_4_birth_year<=31 & digits_5_6_birth_year>31~ paste0("19", substr(firm_birth_year, 5,6)),

# # Continue with results below 1902
# # test<-sbs_br_combined[firm_birth_year<"1902" & firm_birth_year!="" & firm_birth_year!="0000" & firm_birth_year!="1900" & firm_birth_year!="1901" & substr(Start_Ent, 1,2)!="26" & substr(Start_Ent, 1,2)!="00"]
# test<-sbs_br_combined[firm_birth_year<"1902"]
# table(substr(test[firm_birth_year=="0101"]$Start_Ent, 1,2)) #These are all 01s
# table(substr(test[firm_birth_year=="0101"]$Start_Ent, 3,4)) #These look like years
# # Result: firm_birth_year=="0101" ~paste0("19", substr(Start_Ent, 3,4))
# 
# test<-test[firm_birth_year!="0101" ]
# table(substr(test[firm_birth_year!="1900"]$Start_Ent, 1,2)) #These are all 01s
# table(substr(test[firm_birth_year!="1900"]$Start_Ent, 3,4)) #These are the years except for values==01,07 and 11. For those we take year==5,6 orr 7,8
# table(substr(test[firm_birth_year!="1900"]$Start_Ent, 5,6)) #These look like years (0-12)
# table(substr(test[firm_birth_year!="1900"]$Start_Ent, 7,8)) #These look like years (0-12)
# # Result: substr(Start_Ent, 3,4)


sbs_br_combined <- sbs_br_combined %>% mutate(firm_birth_year=case_when(firm_birth_year=="0000" ~ NA,
                                                                        firm_birth_year=="faus" ~ NA,
                                                                        firm_birth_year=="1900" ~ NA,
                                                                        firm_birth_year=="1901" ~ NA,
                                                                        firm_birth_year=="1902" ~ NA,
                                                                        firm_birth_year=="" ~ NA,
                                                                        substr(Start_Ent, 1,2)=="00" ~NA,
                                                                        substr(Start_Ent, 1,2)=="26" ~NA,
                                                                        Start_Ent=="00000000"~ NA,
                                                                        firm_birth_year>end & first_4_digits_birth_year=="0101" ~ paste0("19", substr(firm_birth_year, 1,2)),
                                                                        firm_birth_year>end & first_4_digits_birth_year!="0101" & digits_3_4_birth_year>31 & digits_5_6_birth_year<=31~ paste0("19", substr(firm_birth_year, 3,4)),
                                                                        firm_birth_year>end & first_4_digits_birth_year!="0101" & digits_3_4_birth_year<=31 & digits_5_6_birth_year>31~ paste0("19", substr(firm_birth_year, 5,6)),
                                                                        # firm_birth_year>end & firm_birth_year=="0101" ~ paste0("19", substr(Start_Ent, 3,4)),
                                                                        firm_birth_year<"1900" & firm_birth_year=="0101" ~ paste0("19", substr(Start_Ent, 3,4)),
                                                                        firm_birth_year<"1900" & firm_birth_year!="0101" & substr(Start_Ent, 3,4) %in% c("01", "07") ~ paste0("19", substr(Start_Ent, 5,6)),
                                                                        firm_birth_year<"1900" & firm_birth_year!="0101" & substr(Start_Ent, 3,4)=="11" ~ paste0("19", substr(Start_Ent, 7,8)),
                                                                        firm_birth_year<"1900" & firm_birth_year!="0101" & !(substr(Start_Ent, 3,4) %in% c("01", "07", "11"))  ~ paste0("19", substr(Start_Ent, 3,4)),
                                                                        TRUE ~ firm_birth_year))

# test<-sbs_br_combined[firm_birth_year=="19"]
# table(substr(test[firm_birth_year!="1900"]$Start_Ent, 1,2)) #These are all 01s
# table(substr(test[firm_birth_year!="1900"]$Start_Ent, 3,4)) #These look like days (02-31)
# table(substr(test[firm_birth_year!="1900"]$Start_Ent, 5,6)) #These look like years (0-12)
# table(substr(test[firm_birth_year!="1900"]$Start_Ent, 7,8)) #These look like months (01-12)

# Fix firm_birth_year==19 (no idea why this appears) by adding digits 5 and 6 of Start_Ent
sbs_br_combined[, firm_birth_year:=ifelse(firm_birth_year=="19", paste0("19", substr(Start_Ent, 5,6)), firm_birth_year)]

# Make sure all years are between 1901 and end_year
table(sbs_br_combined$firm_birth_year)

# Create test dataset with unique observations based on firmid, Start_Ent and firm_birth_year, counting the number of those unique observations per firmid
test<-sbs_br_combined %>%  group_by(firmid, firm_birth_year) %>% mutate(count=n()) %>% distinct(firmid, Start_Ent, firm_birth_year, .keep_all=T) %>%
  ungroup() %>% select(firmid, Start_Ent, firm_birth_year, count)

# Drop rows with NA valuse in the firm_birth_year column
test<-test %>% group_by(firmid) %>% mutate(distinct_years=n_distinct(firm_birth_year[!is.na(firm_birth_year)]))
setorder(test,  distinct_years, firmid, firm_birth_year)

# Keep unique observations in terms of firmid, firm_birth_year, count and distinct_year
test<- test %>% distinct(firmid, firm_birth_year, count, distinct_years, .keep_all = T)
setorder(test,  distinct_years, firmid, firm_birth_year)

test<-test %>% filter(!is.na(firm_birth_year))

# Keep the firm_birth_year observation that appears the most per firmid and, in case of a tie,
# keep the one with the oldest firm_birth_year
# CAUTION: this takes quite a bit of time
test2<-test %>% group_by(firmid) %>% slice_max(order_by = count, with_ties = T) %>% arrange(firmid, firm_birth_year) %>% slice(1) %>% ungroup()
test2<-test2 %>% select(firmid, firm_birth_year)

# Delete usless variables created along the way
sbs_br_combined$date_length<-NULL
sbs_br_combined$first_4_digits_birth_year<-NULL
sbs_br_combined$digits_3_4_birth_year <-NULL
sbs_br_combined$digits_5_6_birth_year <-NULL
sbs_br_combined$firm_birth_year <-NULL

# Merge sbs_br_combined with the created dataset
sbs_br_combined<-merge(sbs_br_combined, test2, by="firmid", all.x = T)

# Clean test and test2
test<-NULL
test2<-NULL