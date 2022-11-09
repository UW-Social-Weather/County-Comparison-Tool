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


# resources -----------------------------------------------------------------
source("healthdown/healthdown_ui.R")
source("healthdown/healthdown.R")


source("healthdown/helpers/utils.R")
source("healthdown/helpers/scatter_plot.R")
source("healthdown/helpers/line_graph.R")
source("healthdown/helpers/table.R")
source("healthdown/helpers/bar_chart.R")

states <- readRDS("shapes/us1.RDS")
counties <- readRDS("shapes/us2.RDS")
spdfs_list <- list(states, counties)

us_health_states <- readr::read_delim(
  "data/clean/us_health_states.csv", ";", 
  escape_double = FALSE, trim_ws = TRUE,
  col_types = readr::cols(),
  locale = readr::locale(decimal_mark = ",", grouping_mark = ".")
)

us_health_counties <- readr::read_delim(
  "data/clean/us_health_counties.csv", ";", 
  escape_double = FALSE, trim_ws = TRUE,
  col_types = readr::cols(),
  locale = readr::locale(decimal_mark = ",", grouping_mark = ".")
)

SVI_grouping_data <- readr::read_delim(
  "data/clean/SVI_grouping_data.csv", ",", 
  escape_double = FALSE, trim_ws = TRUE,
  col_types = readr::cols(),
  locale = readr::locale(decimal_mark = ",", grouping_mark = ".")
)

# SVI_grouping_data <- rename(SVI_grouping_data, ST_NUM = ST)
# SVI_grouping_data <- rename(SVI_grouping_data, ST = ST_ABBR)

us_health_all <- rbind(us_health_states, us_health_counties)
all_years <- unique(us_health_all$year)

us_health_all <- merge(us_health_all, SVI_grouping_data, by="NAME_2", all.x= TRUE)
all_SVI <- unique(na.omit(us_health_all$SVI_Group))
all_SVI

us_health_all <- rename(us_health_all, FIPS = FIPS.x)
us_health_all <- rename(us_health_all, State = State.x)

us_health_counties1 <- merge(us_health_counties,SVI_grouping_data, by="NAME_2", all.x=TRUE)
us_health_states1 <- merge(us_health_states,SVI_grouping_data, by="NAME_2", all.x=TRUE)

us_health_counties1 <- rename(us_health_counties1, FIPS = FIPS.x)
us_health_states1 <- rename(us_health_states1, State = State.x)

all_vars <- sort(names(us_health_all)[6:ncol(us_health_all)])
all_vars <- all_vars[!grepl("-CI", all_vars)]
all_vars <- all_vars[!grepl("-Z", all_vars)]

