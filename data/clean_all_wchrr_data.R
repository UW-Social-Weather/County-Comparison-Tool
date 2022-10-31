# File purpose: Clean and merge all Wisc CHRR Data 
# Created by: Caitlin Drover 
# Created: October 19, 2022

# set up
library(readxl)
library(tibble)
library(stringr)
library(dplyr)
library(plyr)
library(tidyverse)

## Note: need to adapt/update this ## 
# Set shared team drive and code repo dynamically
if (Sys.info()["user"]=="caitlindrover"){
    file_folder  <- '/Users/caitlindrover/Desktop/OneDrive - UW/county_comparison_tool/data/raw_data/wisconsin_chrr'
  # #add Francisco's info here: 
  #   } else if (Sys.info()["user"]=='frc2'){
  #   file_folder  <- ' '
  # #add Harsha's info here: 
  #   } else if (Sys.info()["user"]==' '){
  #   file_path  <- ' '
  }


# Create list of data files - directory path will need to be updated for each user
files <- list.files(path=file_folder, full.names=TRUE, recursive=FALSE)


# List of columns to keep from each excel sheet
reference_header <- unlist(read.csv2("data/ref_header_works.csv", stringsAsFactors = FALSE))
reference_header_add <- unlist(read.csv2("data/ref_header_addmeasures.csv", stringsAsFactors = FALSE))


## Loop to read all files and sheets; store it in a data frame df

df <- NULL

for (x in 2:length(files)) {
  # use this to troubleshoot
  # x = 2
  
  file_name <- files[x]
  
  print(file_name)
  
  sheets <- excel_sheets(file_name)
  
  # Read Ranked Measure Data sheet
  if("Measure Data" %in% sheets) {
    file <- suppressMessages(read_excel(file_name, sheet = "Measure Data", col_names = FALSE))
  } else if("Ranked Measure Data" %in% sheets) {
    file <- suppressMessages(read_excel(file_name, sheet = "Ranked Measure Data", col_names = FALSE))
  } else {
    print(paste("sheet fail", sheets))
  }
  
  ### Work in progress ###
  
  # Read Additional Measure Data sheet
    if("Additional Measure Data" %in% sheets) {
    fileAdd <- suppressMessages(read_excel(file_name, sheet = "Additional Measure Data", col_names = FALSE))
  } else {
    print(paste("sheet fail", sheets))
  }
  
  # adding measure section name to each column - same as old cleaning script 
  previous_col <- ""
  for(column_index in 4:ncol(file)) {
    current_col <- file[1, column_index]
    
    if(is.na(current_col)) {
      
    } else if(current_col == "Preventable hospital stays (Ambulatory Care Sensitive Conditions)") {
      current_col <- "Preventable hospital stays"
    } else if(current_col == "Premature death (Years of Potential Life Lost)") {
      current_col <- "Premature death"
    } else if(current_col == "Some college (post-secondary education)") {
      current_col <- "Some college"
    } else if(current_col == "") {
      current_col <- NA
    }   
    file[1, column_index] <- current_col
    
    if(is.na(current_col)) {
      file[1, column_index] <- previous_col
    }
    
    previous_col <- file[1, column_index]
  }
  
  file[1, 1] <- "FIPS"		
  file[1, 2] <- "State"
  file[1, 3] <- "County"
  
  
  ### Work in progress ###
  
  ## Repeat for additional measure data
  previous_col <- ""
  for(column_index in 4:ncol(fileAdd)) {
    current_col <- fileAdd[1, column_index]

    if(is.na(current_col)) {

    } else if(current_col == "Preventable hospital stays (Ambulatory Care Sensitive Conditions)") {
      current_col <- "Preventable hospital stays"
    } else if(current_col == "Premature death (Years of Potential Life Lost)") {
      current_col <- "Premature death"
    } else if(current_col == "Some college (post-secondary education)") {
      current_col <- "Some college"
    } else if(current_col == "") {
      current_col <- NA
    }
    fileAdd[1, column_index] <- current_col

    if(is.na(current_col)) {
      fileAdd[1, column_index] <- previous_col
    }

    previous_col <- fileAdd[1, column_index]
  }

  fileAdd[1, 1] <- "FIPS"
  fileAdd[1, 2] <- "State"
  fileAdd[1, 3] <- "County"
  
  
  # Standardizing headers
  ### note: these will likely need to be renamed (simpler, no spaces/special characters)
  
  file <- file[, !is.na(unlist(file[1, ]))]

  header <- str_c(unlist(file[1, ]), ":", unlist(file[2, ]))
  header[1:3] <- c("FIPS", "State", "County")
  
  header <- header %>% 
    str_replace("Premature death:Deaths", "Premature death:# Deaths") %>% 
    str_replace("Premature death:Years of Potential Life Lost Rate", "Premature death:YPLL Rate") %>% 
    str_replace("Low birthweight:LBW Births", "Low birthweight:# Low Birthweight Births") %>% 
    str_replace("Low birthweight:Live Births", "Low birthweight:# Live births") %>% 
    str_replace("Low birthweight:# Live Births", "Low birthweight:# Live births") %>%  
    str_replace("Sexually transmitted infections:Chlamydia Cases", "Sexually transmitted infections:# Chlamydia Cases") %>% 
    str_replace("Sexually transmitted infections:Chlamydia Rate", "Sexually transmitted infections:Chlamydia Incidence") %>% 
    str_replace("Chlamydia rate:Cases", "Sexually transmitted infections:# Chlamydia Cases") %>% 
    str_replace("Chlamydia rate:Rates per 100,000", "Sexually transmitted infections:Chlamydia Incidence") %>% 
    str_replace("Sexually transmitted infections:Cases", "Sexually transmitted infections:# Chlamydia Cases") %>% 
    str_replace("Sexually transmitted infections:Rates per 100,000", "Sexually transmitted infections:Chlamydia Incidence") %>% 
    str_replace("Primary care physicians:PCP", "Primary care physicians:# PCP") %>% 
    str_replace("Primary care physicians:# Primary Care Physicians", "Primary care physicians:# PCP") %>% 
    str_replace("Preventable hospital stays:Preventable Hosp. Rate", "Preventable hospital stays:ACSC Rate") %>% 
    str_replace("Preventable hospital stays:# Medicare Enrollees", "Preventable hospital stays:# Medicare enrollees") %>%
    str_replace("Preventable hospital stays:Medicare enrollees", "Preventable hospital stays:# Medicare enrollees") %>% 
    str_replace("Violent crime:# Violent Crimes", "Violent crime:# Annual Violent Crimes")%>%
    str_replace("Violent crime:Annual Violent Crimes", "Violent crime:# Annual Violent Crimes") %>%
    str_replace("Air pollution - particulate matter:Average Daily PM2.5", "Air pollution - particulate matter:Average daily PM25") %>%
    str_replace("Drinking water violations:% Pop in Viol", "Drinking water violations:% pop in viol" ) %>%
    str_replace("Injury deaths:Injury Deaths", "Injury deaths:# Injury Deaths") %>%
    str_replace("Poor or fair health:% Fair or Poor Health", "Poor or fair health:% Fair/Poor") %>%
    str_replace("Poor physical health days:Average Number of Physically Unhealthy Days", "Poor physical health days:Physically Unhealthy Days") %>%
    str_replace("Poor mental health days:Average Number of Mentally Unhealthy Days", "Poor mental health days:Mentally Unhealthy Days") %>%
    str_replace("Adult obesity:% Adults with Obesity", "Adult obesity:% Obese") %>%
    str_replace("Driving alone to work:Workers", "Driving alone to work:# Workers") %>%
    str_replace("Long commute - driving alone:Long Commute - Drives Alone", "Long commute - driving alone:% Long Commute - Drives Alone") %>%
    str_replace("Long commute - driving alone:Workers who Drive Alone", "Long commute - driving alone:# Workers who Drive Alone") %>%
    str_replace("Severe housing problems:# Household with Severe Problems" , "Severe housing problems:# Households with Severe Problems") #%>%  
    # str_replace("Diabetic monitoring:% Receiving HbA1c", "Diabetic screening:% HbA1c") %>%
    # str_replace("Diabetic monitoring:95% CI - High" , "Diabetic screening:95% CI - High") %>%
    # str_replace("Diabetic monitoring:95% CI - Low", "Diabetic screening:95% CI - Low")
 
   
  #trim empty lines
  file <- file[!is.na(file[,1]),]
  
  # remove header lines
  file <- file[-c(1:2), ]
  
  # add clean column names 
  names(file) <- header
  
  ### Work in progress ###
  
  ## Repeat for the additional measure data
  fileAdd <- fileAdd[, !is.na(unlist(fileAdd[1, ]))]

  headerAdd <- str_c(unlist(fileAdd[1, ]), ":", unlist(fileAdd[2, ]))
  headerAdd[1:3] <- c("FIPS", "State", "County")

  headerAdd <- headerAdd %>%
    str_replace("% diabetic:Diabetes", "Diabetes prevalence:% Diabetic") %>%
    str_replace("% diabetic:95% CI - High", "Diabetes prevalence:95% CI - High") %>%
    str_replace("% diabetic:95% CI - Low", "Diabetes prevalence:95% CI - Low") %>%
    str_replace("Diabetes:% diabetic", "Diabetes prevalence:% Diabetic") %>%
    str_replace("Diabetes:% Diabetic", "Diabetes prevalence:% Diabetic") %>%
    str_replace("Diabetes prevalence:% Adults with Diabetes", "Diabetes prevalence:% Diabetic") %>%
    str_replace("Diabetes:95% CI - High", "Diabetes prevalence:95% CI - High") %>%
    str_replace("Diabetes:95% CI - Low", "Diabetes prevalence:95% CI - Low") %>%
    str_replace("COVID-19 age-adjusted mortality:# deaths due to COVID-19 during 2020", 
                "COVID-19 mortality:# deaths during 2020") %>%
    str_replace("COVID-19 age-adjusted mortality:COVID-19 death rate", "COVID-19 mortality:Death rate")
    

  fileAdd <- fileAdd[!is.na(fileAdd[,1]),]
  fileAdd <- fileAdd[-c(1:2), ]
  names(fileAdd) <- headerAdd

  
  # Trim columns to keep measures of interest 
  if(!all(reference_header %in% header)) {
    print("header fail")
  } else {
    file <- file[, which(header %in% reference_header), drop = FALSE]
    # add year col
    year <- 2009+x
    file <- add_column(file, year = year, .before = 1)
    
    file_bind[,5:dim(file_bind)[2]] <- sapply(file_bind[,5:dim(file_bind)[2]], as.numeric)
    file_bind[,5:dim(file_bind)[2]] <- sapply(file_bind[,5:dim(file_bind)[2]], round, digits = 2)
  }
  
  if(!any(reference_header_add %in% headerAdd)) {
    print("additional header fail")
  } else {
    fileAdd <- fileAdd[, which(headerAdd %in% reference_header_add), drop = FALSE]
    # add year col
    year <- 2009+x
    fileAdd <- add_column(fileAdd, year = year, .before = 1)
    
    file_bind[,5:dim(file_bind)[2]] <- sapply(file_bind[,5:dim(file_bind)[2]], as.numeric)
    file_bind[,5:dim(file_bind)[2]] <- sapply(file_bind[,5:dim(file_bind)[2]], round, digits = 2)
  } 

  # Merge the two data sheets
  file_bind <- merge(file, fileAdd, by=c("year", "FIPS", "State", "County"))
  
  # Bind each year into master df 
  df <- rbind.fill(df, file_bind)
}

# Fix incorrect county row 
df$County[df$County==2000] <- "Chilton" 

# Change variable class to numeric
df[,5:dim(df)[2]] <- sapply(df[,5:dim(df)[2]], as.numeric)

# Add column names back
# reference_header[1:3] <- c("FIPS", "State", "County")
# names(df) <- c("year", reference_header)

### note: these will likely need to be renamed (simpler, no spaces/special characters)
df <- df %>% rename("YPLL Rate" = "Premature death:YPLL Rate", 
                    "YPLL Rate-CIL" = "Premature death:95% CI - Low", 
                    "YPLL Rate-CIH" = "Premature death:95% CI - High", 
                    
                    "Poor or fair health [in %]" = "Poor or fair health:% Fair/Poor",
                    "Poor or fair health [in %]-CIL" = "Poor or fair health:95% CI - Low",
                    "Poor or fair health [in %]-CIH" = "Poor or fair health:95% CI - High",
                    
                    "Physically Unhealthy Days" = "Poor physical health days:Physically Unhealthy Days",
                    "Physically Unhealthy Days-CIL" = "Poor physical health days:95% CI - Low", 
                    "Physically Unhealthy Days-CIH" = "Poor physical health days:95% CI - High", 
                    
                    "Mentally Unhealthy Days" = "Poor mental health days:Mentally Unhealthy Days", 
                    "Mentally Unhealthy Days-CIL" = "Poor mental health days:95% CI - Low",
                    "Mentally Unhealthy Days-CIH" = "Poor mental health days:95% CI - High",
                    
                    "Adult smoking [in %]" = "Adult smoking:% Smokers", 
                    "Adult smoking [in %]-CIL" = "Adult smoking:95% CI - Low", 
                    "Adult smoking [in %]-CIH" = "Adult smoking:95% CI - High", 
                    
                    "Adult obesity [in %]" = "Adult obesity:% Obese",
                    "Adult obesity [in %]-CIL" = "Adult obesity:95% CI - Low",
                    "Adult obesity [in %]-CIH" = "Adult obesity:95% CI - High",
                    
                    "Chlamydia Cases" = "Sexually transmitted infections:# Chlamydia Cases",
                    "Chlamydia Incidence [per 100,000]" = "Sexually transmitted infections:Chlamydia Incidence")


# #still getting an error that "object 'us_election_states' not found"
# #may need to update this later 
# df <- df %>% left_join(us_election_states %>% select(State, ST))
# df <- df %>% relocate(ST, .before = County)

# saved a copy of this cleaned data without the two rows above 
# write.csv2(df, "data/clean/all_wchrr.csv", row.names = FALSE)
#
# 
# states <- df %>% filter(is.na(County))
# write.csv2(states, "data/clean/us_health_states.csv", row.names = FALSE)
# 
# counties <- df %>% filter(!is.na(County))
# write.csv2(counties, "data/clean/us_health_counties.csv", row.names = FALSE)