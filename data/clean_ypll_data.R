# File purpose: File which cleans Wisconsin County Health Ranking Data 
# Created by: Harsha 
# Last updated: October 18, 2022

# set up
library(readxl)
library(tibble)
library(stringr)
library(dplyr)

# check if all files have the same columns...
#Purpose: loop ; read the files and sheets; store it in a data frame df
data_dir <- "C:/Users/frc2/UW/og_uw_social_weather - data/"
files <- list.files(path=paste0(data_dir, "/raw_data/wisconsin_chrr/"), pattern="*.xls", full.names=TRUE, recursive=FALSE)
reference_header <- unlist(read.csv2(paste0(data_dir, "documentation/ref_header.csv"), stringsAsFactors = FALSE))

df <- NULL

for (x in 1:length(files)) {
  
  # uncomment below to trouble-shoot
  # x <- 5
  
  file_name <- files[x]
  
  # Print name of file that is being read in case it breaks you know which file it broke on
  print(paste0("FILE ", x, ": ", file_name))
  
  # pull all the names of each sheet in the dataset
  sheets <- excel_sheets(file_name)
  
 # save sheet of interest
  if("Measure Data" %in% sheets) {
    file <- suppressMessages(read_excel(file_name, sheet = "Measure Data", col_names = FALSE))
  } else if("Ranked Measure Data" %in% sheets) {
    file <- suppressMessages(read_excel(file_name, sheet = "Ranked Measure Data", col_names = FALSE))
  } else {
    print(paste("fail", sheets))
  }
  
  # subset columns of interest (FIPS, state, )
  fips_col <- grep("FIPS", file)
  state_col <- grep("State", file)
  county_col <- grep("County", file)
  
  if (file_name%in%c("C:/Users/frc2/UW/og_uw_social_weather - data//raw_data/wisconsin_chrr/2015 County Health Rankings Data - v3.xls",
                     "C:/Users/frc2/UW/og_uw_social_weather - data//raw_data/wisconsin_chrr/2016 County Health Rankings Data - v3.xls",
                     "C:/Users/frc2/UW/og_uw_social_weather - data//raw_data/wisconsin_chrr/2017CountyHealthRankingsData.xls",
                     "C:/Users/frc2/UW/og_uw_social_weather - data//raw_data/wisconsin_chrr/2018 County Health Rankings Data - v2.xls",
                     "C:/Users/frc2/UW/og_uw_social_weather - data//raw_data/wisconsin_chrr/2019 County Health Rankings Data - v3.xls",
                     "C:/Users/frc2/UW/og_uw_social_weather - data//raw_data/wisconsin_chrr/2020 County Health Rankings Data - v2.xlsx",
                     "C:/Users/frc2/UW/og_uw_social_weather - data//raw_data/wisconsin_chrr/2022 County Health Rankings Data - v1.xlsx")){
    ypll_col <- grep("Years of Potential Life Lost", file)
  } else {
    ypll_col <- grep("YPLL", file)
  }
  
  # if multiple columns match, keep first only
  if (length(ypll_col)>1) {
    ypll_col <- ypll_col[1]
  }

  # rename columns
  colnames(file)[fips_col] <- "fips"
  colnames(file)[state_col] <- "state"
  colnames(file)[county_col] <- "county"
  colnames(file)[ypll_col] <- "ypll_rate"
  
  # Subset to only these columns.
  file <- file %>% select(fips, state, county, ypll_col)
  
  # Subset Rows
  start_row <- grep("state", tolower(file$state)) + 1
  end_row <- grep("weston", tolower(file$county))
  
  y = 1 
  while (end_row[y] < start_row) {
    y = y + 1
  }
  end_row = end_row[y]
  
  # validate that these are correct
  stopifnot(length(start_row)==1 & length(end_row)==1)
  file <- file[start_row:end_row,]
  
  # generate new variables
  if (file_name=="C:/Users/frc2/UW/og_uw_social_weather - data//raw_data/wisconsin_chrr/2010 County Health Rankings National Data_v2.xls") {
    file$year <- 2010
  } else if (file_name=="C:/Users/frc2/UW/og_uw_social_weather - data//raw_data/wisconsin_chrr/2011 County Health Rankings National Data_v2_0.xls") {
    file$year <- 2011
  } else if (file_name=="C:/Users/frc2/UW/og_uw_social_weather - data//raw_data/wisconsin_chrr/2012 County Health Rankings National Data_v2_0.xls") {
    file$year <- 2012
  } else if (file_name=="C:/Users/frc2/UW/og_uw_social_weather - data//raw_data/wisconsin_chrr/2013CountyHealthRankingsNationalData.xls") {
    file$year <- 2013
  } else if (file_name=="C:/Users/frc2/UW/og_uw_social_weather - data//raw_data/wisconsin_chrr/2014 County Health Rankings Data - v6.xls") {
    file$year <- 2014
  } else if (file_name=="C:/Users/frc2/UW/og_uw_social_weather - data//raw_data/wisconsin_chrr/2015 County Health Rankings Data - v3.xls") {
    file$year <- 2015
  } else if (file_name=="C:/Users/frc2/UW/og_uw_social_weather - data//raw_data/wisconsin_chrr/2016 County Health Rankings Data - v3.xls") {
    file$year <- 2016
  } else if (file_name=="C:/Users/frc2/UW/og_uw_social_weather - data//raw_data/wisconsin_chrr/2017CountyHealthRankingsData.xls") {
    file$year <- 2017
  } else if (file_name=="C:/Users/frc2/UW/og_uw_social_weather - data//raw_data/wisconsin_chrr/2018 County Health Rankings Data - v2.xls") {
    file$year <- 2018
  } else if (file_name=="C:/Users/frc2/UW/og_uw_social_weather - data//raw_data/wisconsin_chrr/2019 County Health Rankings Data - v3.xls") {
    file$year <- 2019
  } else if (file_name=="C:/Users/frc2/UW/og_uw_social_weather - data//raw_data/wisconsin_chrr/2020 County Health Rankings Data - v2.xlsx") {
    file$year <- 2020
  } else if (file_name=="C:/Users/frc2/UW/og_uw_social_weather - data//raw_data/wisconsin_chrr/2022 County Health Rankings Data - v1.xlsx") {
    file$year <- 2022
  }
  
  # diabetes_col <- grep("Diabet", file)
  # identify name row
  # correctly_named = grepl("FIPS", tolower(names(file)))
  
  # id specific columns of interest
  
  # previous_col <- ""
  # for(column_index in 4:ncol(file)) {
  #   current_col <- file[1, column_index]
    
    # TODO move renaming to later
    #remove columns with significant nas. (YPLL data had significant nas)
  #   if(is.na(current_col)) {
  #     
  #   } else if(current_col == "Preventable hospital stays (Ambulatory Care Sensitive Conditions)") {
  #     current_col <- "Preventable hospital stays"
  #   } else if(current_col == "Premature death (Years of Potential Life Lost)") {
  #     current_col <- "Premature death"
  #   } else if(current_col == "Some college (post-secondary education)") {
  #     current_col <- "Some college"
  #   }  
  #   file[1, column_index] <- current_col
  #   
  #   if(is.na(current_col)) {
  #     file[1, column_index] <- previous_col
  #   }
  #   
  #   previous_col <- file[1, column_index]
  # }
  
  #Ensuring uniformity and readability of the headers
  
  # file <- file[, !is.na(unlist(file[1, ]))]
  # 
  # header <- str_c(unlist(file[1, ]), ":", unlist(file[2, ]))
  # header[1:3] <- c("FIPS", "State", "County")
  
  # header <- header %>% 
  #   str_replace("Premature death:Deaths", "Premature death:# Deaths") %>% 
  #   str_replace("Premature death:Years of Potential Life Lost Rate", "Premature death:YPLL Rate") %>% 
  #   str_replace("Low birthweight:LBW Births", "Low birthweight:# Low Birthweight Births") %>% 
  #   str_replace("Low birthweight:Live Births", "Low birthweight:# Live births") %>% 
  #   str_replace("Low birthweight:# Live Births", "Low birthweight:# Live births") %>%  
  #   str_replace("Sexually transmitted infections:Chlamydia Cases", "Sexually transmitted infections:# Chlamydia Cases") %>% 
  #   str_replace("Sexually transmitted infections:Chlamydia Rate", "Sexually transmitted infections:Chlamydia Incidence") %>% 
  #   str_replace("Primary care physicians:PCP", "Primary care physicians:# PCP") %>% 
  #   str_replace("Primary care physicians:# Primary Care Physicians", "Primary care physicians:# PCP") %>% 
  #   str_replace("Preventable hospital stays:Preventable Hosp. Rate", "Preventable hospital stays:ACSC Rate") %>% 
  #   str_replace("Preventable hospital stays:# Medicare Enrollees", "Preventable hospital stays:# Medicare enrollees") %>%
  #   str_replace("Preventable hospital stays:Medicare enrollees", "Preventable hospital stays:# Medicare enrollees") %>% 
  #   str_replace("Violent crime:# Violent Crimes", "Violent crime:# Annual Violent Crimes")%>%
  #   str_replace("Violent crime:Annual Violent Crimes", "Violent crime:# Annual Violent Crimes") %>%
  #   str_replace("Air pollution - particulate matter:Average Daily PM2.5", "Air pollution - particulate matter:Average daily PM25") %>%
  #   str_replace("Drinking water violations:% Pop in Viol", "Drinking water violations:% pop in viol" ) %>%
  #   str_replace("Injury deaths:Injury Deaths", "Injury deaths:# Injury Deaths") %>%
  #   str_replace("Poor or fair health:% Fair or Poor Health", "Poor or fair health:% Fair/Poor") %>%
  #   str_replace("Poor physical health days:Average Number of Physically Unhealthy Days", "Poor physical health days:Physically Unhealthy Days") %>%
  #   str_replace("Poor mental health days:Average Number of Mentally Unhealthy Days", "Poor mental health days:Mentally Unhealthy Days") %>%
  #   str_replace("Adult obesity:% Adults with Obesity", "Adult obesity:% Obese") %>%
  #   str_replace("Driving alone to work:Workers", "Driving alone to work:# Workers") %>%
  #   str_replace("Long commute - driving alone:Long Commute - Drives Alone", "Long commute - driving alone:% Long Commute - Drives Alone") %>%
  #   str_replace("Long commute - driving alone:Workers who Drive Alone", "Long commute - driving alone:# Workers who Drive Alone") %>%
  #   str_replace("Severe housing problems:# Household with Severe Problems" , "Severe housing problems:# Households with Severe Problems" )
    

# 
#   #trim empty lines
#   file <- file[!is.na(file[,1]),]
#   
#   # remove header lines
#   file <- file[-c(1:2), ]
#   
#   
#   names(file) <- header
#   
#   if(!all(reference_header %in% header)) {
#     print("fail")
#   } else {
#     file <- file %>% select(all_of(reference_header))
#     
#     year <- substr(file_name, 10, 13)
#     file <- add_column(file, year = year, .before = 1)
#     
#     
#     file[,5:dim(file)[2]] <- sapply(file[,5:dim(file)[2]], as.numeric)
#     file[,5:dim(file)[2]] <- sapply(file[,5:dim(file)[2]], round, digits = 2)
#   }
#   
#   
  #save_path <- paste0("data/clean/", file[2,1], file[2,3], ".csv")
  #write.csv2(file, save_path, row.names = FALSE)
  
    #Bind data together 
    if(x==1){
      extracted_data = file
    } else {
      extracted_data = rbind(extracted_data, file, use.names=TRUE, fill = TRUE)
    }
    
}
  
saveRDS(extracted_data, paste0(data_dir, "prepped_data/01_prepped_ypll_data.RDS"))
