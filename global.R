# # Old code 
# library(shiny)
# library(shinyjs)
# library(shinythemes)
# library(leaflet)
# library(leafdown)
# library(echarts4r)
# library(dplyr)
# library(tidyr)
# library(RColorBrewer)
# library(shinydashboard)
# library(shinyWidgets)
# library(readr)
# 
# # resources -----------------------------------------------------------------
# source("healthdown/healthdown_ui.R")
# source("healthdown/healthdown.R")
# 
# 
# source("healthdown/helpers/utils.R")
# source("healthdown/helpers/scatter_plot.R")
# source("healthdown/helpers/line_graph.R")
# source("healthdown/helpers/table.R")
# source("healthdown/helpers/bar_chart.R")
# 
# states <- readRDS("shapes/us1.RDS")
# counties <- readRDS("shapes/us2.RDS")
# spdfs_list <- list(states, counties)
# 
# us_health_states <- readr::read_delim(
#   "data/clean/us_health_states.csv", ";",
#   escape_double = FALSE, trim_ws = TRUE,
#   col_types = readr::cols(),
#   locale = readr::locale(decimal_mark = ",", grouping_mark = ".")
# )
# 
# us_health_counties <- readr::read_delim(
#   "data/clean/us_health_counties.csv", ";",
#   escape_double = FALSE, trim_ws = TRUE,
#   col_types = readr::cols(),
#   locale = readr::locale(decimal_mark = ",", grouping_mark = ".")
# )
# 
# SVI_grouping_data <- readr::read_delim(
#   "data/clean/SVI_grouping_data.csv", ",",
#   escape_double = FALSE, trim_ws = TRUE,
#   col_types = readr::cols(),
#   locale = readr::locale(decimal_mark = ",", grouping_mark = ".")
# )
# 
# SVI_grouping_data <- rename(SVI_grouping_data, ST_NUM = ST)
# SVI_grouping_data <- rename(SVI_grouping_data, ST = ST_ABBR)
# 
# us_health_all <- rbind(us_health_states, us_health_counties)
# all_years <- unique(us_health_all$year)
# 
# us_health_all <- merge(us_health_all, SVI_grouping_data, by=c("NAME_2","FIPS","ST"), all.x= TRUE)
# all_SVI <- unique(na.omit(SVI_grouping_data$SVI_Group))
# all_SVI
# 
# # us_health_all <- rename(us_health_all, FIPS = FIPS.x)
# us_health_all <- rename(us_health_all, State = State.x)
# 
# us_health_counties1 <- merge(us_health_counties,SVI_grouping_data, by=c("NAME_2","FIPS","ST"), all.x=TRUE)
# us_health_states1 <- merge(us_health_states,SVI_grouping_data, by=c("NAME_2","FIPS","ST"), all.x=TRUE)
# 
# # us_health_counties1 <- rename(us_health_counties1, FIPS = FIPS.x)
# us_health_states1 <- rename(us_health_states1, State = State.x)
# 
# all_vars <- sort(names(us_health_all)[6:29])
# # all_vars <- sort(names(us_health_all)[6:ncol(us_health_all)])
# all_vars <- all_vars[!grepl("-CI", all_vars)]
# all_vars <- all_vars[!grepl("-Z", all_vars)]

# Work in progress code
library(shiny)
library(shinyjs)
library(shinythemes)
library(leaflet)
library(leafdown)
library(echarts4r)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(shinydashboard)
library(shinyWidgets)
library(readr)
library(readxl)
library(data.table)

# resources -----------------------------------------------------------------
source("healthdown/healthdown.R")
source("healthdown/healthdown_ui.R")

source("healthdown/helpers/utils.R")
source("healthdown/helpers/scatter_plot.R")
source("healthdown/helpers/line_graph.R")
source("healthdown/helpers/table.R")
source("healthdown/helpers/bar_chart.R")

states <- readRDS("shapes/us1.RDS")
counties <- readRDS("shapes/us2.RDS")
spdfs_list <- list(states, counties)

# us_health_states_old <- readr::read_delim(
#   "data/clean/us_health_states.csv", ";",
#   escape_double = FALSE, trim_ws = TRUE,
#   col_types = readr::cols(),
#   locale = readr::locale(decimal_mark = ",", grouping_mark = ".")
# )
# 
# us_health_counties_old <- readr::read_delim(
#   "data/clean/us_health_counties.csv", ";",
#   escape_double = FALSE, trim_ws = TRUE,
#   col_types = readr::cols(),
#   locale = readr::locale(decimal_mark = ",", grouping_mark = ".")
# )

# load health measure WCHRR data
us_wchrr_all <- readr::read_delim(
  "data/clean/all_wchrr_updated.csv", ";",
  escape_double = FALSE, trim_ws = TRUE,
  col_types = readr::cols(),
  locale = readr::locale(decimal_mark = ",", grouping_mark = ".")
)

# load SVI data
SVI_grouping_data <- readr::read_delim(
  "data/clean/SVI_grouping_data.csv", ",",
  escape_double = FALSE, trim_ws = TRUE,
  col_types = readr::cols(),
  locale = readr::locale(decimal_mark = ",", grouping_mark = ".")
)

# load/clean up population data 
pop_grouping <- suppressMessages(read_excel('data/ruralurbancodes2013.xls', col_names = TRUE))
pop_grouping <- rename(pop_grouping, ST = State)
all_pop <- unique(na.omit(pop_grouping$Description))
all_pop

# clean up SVI data
SVI_grouping_data <- rename(SVI_grouping_data, ST_NUM = ST)
SVI_grouping_data <- rename(SVI_grouping_data, ST = ST_ABBR)
SVI_grouping_data$FIPS <- sprintf("%05d",SVI_grouping_data$FIPS)
all_SVI <- unique(na.omit(SVI_grouping_data$SVI_Group))
all_SVI

all_years <- unique(us_wchrr_all$year)

# clean up health measure data 
us_wchrr_all <- us_wchrr_all %>% mutate_if(is.numeric, round, digits = 1)
us_wchrr_all <- rename(us_wchrr_all, NAME_2 = County)

# add in SVI data
us_health_all <- merge(us_wchrr_all, SVI_grouping_data, by="FIPS", all.x = TRUE) 
us_health_all <- rename(us_health_all, State = State.x)
us_health_all <- rename(us_health_all, NAME_2 = NAME_2.x)

# add in population data
us_health_all <- merge(us_health_all, pop_grouping, by=c("FIPS","ST"), all.x= TRUE)
us_health_all <- us_health_all %>% select(-State.y, -NAME_2.y, -ST, -ST_NUM, -LOCATION, -AREA_SQMI, -County_Name, -Population_2010, -RUCC_2013)

# add state abbreviations 
us_abbrev <- rbind(us_health_states_old, us_health_counties_old)
us_abbrev <- us_abbrev %>% select("FIPS","ST")
us_abbrev <- us_abbrev %>% distinct()
us_health_all <- merge(us_abbrev, us_health_all, by = "FIPS", all.y = TRUE)
us_health_all <- us_health_all %>% arrange(FIPS, year)

# state data only 
us_health_states <- us_health_all %>% filter(is.na(NAME_2))
# county data only; with SVI and Population data
us_health_counties1 <- us_health_all %>% filter(!is.na(NAME_2))


health_vars <- sort(names(us_health_all)[6:33]) 
health_vars <- health_vars[!grepl("CI -", health_vars)]
health_vars <- health_vars[!grepl("#", health_vars)]

# health_vars_covid <- health_vars[! health_vars %in% 'COVID-19 mortality:Death rate']
# health_vars_lifeexp <- health_vars[! health_vars %in% 'Life expectancy:Life Expectancy']


