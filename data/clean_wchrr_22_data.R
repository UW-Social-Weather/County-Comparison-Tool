# File purpose: Clean the 2022 Wisc CHRR Data to merge with other years 
# Created by: Caitlin Drover 
# Last updated: October 18, 2022

# set up
library(readxl)
library(tibble)
library(stringr)
library(dplyr)

# 2022 data doesn't have Z-Scores
reference_headers <- unlist(read.csv2("data/ref_header_2022.csv", stringsAsFactors = FALSE))

data_2022 <- suppressMessages(read_excel("data/2022_wchrr_data.xlsx", sheet = "Ranked Measure Data", col_names = FALSE))

# adding measure section name to each column - same as other cleaning script 
previous_col <- ""
for(column_index in 4:ncol(data_2022)) {
  current_col <- data_2022[1, column_index]
  
  if(is.na(current_col)) {
    
  } else if(current_col == "Preventable hospital stays (Ambulatory Care Sensitive Conditions)") {
    current_col <- "Preventable hospital stays"
  } else if(current_col == "Premature death (Years of Potential Life Lost)") {
    current_col <- "Premature death"
  } else if(current_col == "Some college (post-secondary education)") {
    current_col <- "Some college"
  }  
  data_2022[1, column_index] <- current_col
  
  if(is.na(current_col)) {
    data_2022[1, column_index] <- previous_col
  }
  
  previous_col <- data_2022[1, column_index]
}

data_2022[1, 1] <- "FIPS"		
data_2022[1, 2] <- "State"
data_2022[1, 3] <- "County"

# standardizing headers - to match other cleaned files 

data_2022 <- data_2022[, !is.na(unlist(data_2022[1, ]))]

headers <- str_c(unlist(data_2022[1, ]), ":", unlist(data_2022[2, ]))
headers[1:3] <- c("FIPS", "State", "County")

headers <- headers %>% 
  str_replace("Premature death:Deaths", "Premature death:# Deaths") %>% 
  str_replace("Premature death:Years of Potential Life Lost Rate", "Premature death:YPLL Rate") %>% 
  str_replace("Low birthweight:LBW Births", "Low birthweight:# Low Birthweight Births") %>% 
  str_replace("Low birthweight:Live Births", "Low birthweight:# Live births") %>% 
  str_replace("Low birthweight:# Live Births", "Low birthweight:# Live births") %>%  
  str_replace("Sexually transmitted infections:Chlamydia Cases", "Sexually transmitted infections:# Chlamydia Cases") %>% 
  str_replace("Sexually transmitted infections:Chlamydia Rate", "Sexually transmitted infections:Chlamydia Incidence") %>% 
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
  str_replace("Severe housing problems:# Household with Severe Problems" , "Severe housing problems:# Households with Severe Problems" )


# trim empty lines
data_2022 <- data_2022[!is.na(data_2022[,1]),]

# remove header lines
data_2022 <- data_2022[-c(1:2), ]

# add clean col names
names(data_2022) <- headers

if(!all(reference_headers %in% headers)) {
  print("fail")
} else {
  data_2022 <- data_2022 %>% select(all_of(reference_headers))
  
  data_2022 <- add_column(data_2022, year = 2022, .before = 1)

  data_2022[,5:dim(data_2022)[2]] <- sapply(data_2022[,5:dim(data_2022)[2]], as.numeric)
  data_2022[,5:dim(data_2022)[2]] <- sapply(data_2022[,5:dim(data_2022)[2]], round, digits = 2)

}
reference_headers[1:3] <- c("FIPS", "State", "NAME_2")
names(data_2022) <- c("year", reference_headers)


data_2022 <- data_2022 %>% rename("YPLL Rate" = "Premature death:YPLL Rate", 
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
                    "Adult obesity [in %]-CIH" = "Adult obesity:95% CI - High" #,

                    # Should chlamydia measures be renamed? 
                    # "Chlamydia Cases" = "Sexually transmitted infections:# Chlamydia Cases",
                    # "Chlamydia Incidence [per 100,000]" = "Sexually transmitted infections:Chlamydia Incidence"
                    )

# still getting an error that "object 'us_election_states' not found"
# may need to update this later 
data_2022 <- data_2022 %>% left_join(us_election_states %>% select(State, ST))
data_2022 <- data_2022 %>% relocate(ST, .before = NAME_2)

# add merge with other cleaned data 



