forma# Date Last Updated: October 6 2022
# Purpose: This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


##### Set working directory
setwd("C:/Users/sarah/Downloads/Work/Dashboard")

source("scripts/01_packages.r") #install/loads necessary packages

data_dir <- here("Data") #define data folder directory

source("scripts/02_alter_data.r") #script to edit data frames
source("scripts/03_ui.r")
source("scripts/04_server.r")

shinyApp(body, server)
