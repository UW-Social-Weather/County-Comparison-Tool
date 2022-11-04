#
# This is a Shiny web application for Social Weather Community Well-Being Dashboard. 
# This App is currently NOT used to publish on rsc.csde.washington.edu and integrate into the social weather website, but will require 
# for the future once the database authentication to shiny server issue is solved.
# Note: this app reads tables in bbfsw database
#
# Ruihan(Bonnie) Bao ruihab@uw.edu
# 2021-12-15
#

############################################# load libraries #############################################
library(shiny)
library(shinythemes)
library(dplyr)
library(RPostgreSQL)
library(magrittr)
library(tidyverse)
library(leaflet)
library(tigris)
options(tigris_use_cache = FALSE)

library(sf)
library(classInt)
library(shinydashboard)

library(RColorBrewer)
library(viridis)
library(ggplot2)

library(DT)
library(shinycssloaders)
library(plotly)

###################################### Connect to bbfsw database ############################################

# script to connect
source("code_base/dbconnect.R")
source("code_base/pgpass.R")

# create a connection to the postgresql database
# note that "con" will be used later in each connection to the database
con <- connectdb(host = "bffsw.csde.washington.edu", dbname = "ruihab_sb")

# test
dbListTables(conn = con) 


# get the required tables from the sql database 
social_index_dataset = dbGetQuery(con, "SELECT * from public.tbl_social_weather_dataset
                                        LEFT JOIN public.tbl_dataset_info using(dataset_id)
                                        LEFT JOIN public.tbl_geography using(geo_id)")
tbl_age = dbReadTable(con,"tbl_Age")
tbl_population =  dbReadTable(con,"tbl_Population")
tbl_lifeexpectancy =  dbReadTable(con,"tbl_LifeExpectancy")
tbl_race = dbReadTable(con,"tbl_Race")
tbl_geography =  dbReadTable(con,"tbl_geography")

tbl_geography$geo_level[tbl_geography$geo_level=='ZCTA']  <- "Zip code" 
social_index_dataset$geo_level[social_index_dataset$geo_level=='ZCTA']  <- "Zip code" 

tbl_age_geo = dplyr::left_join(tbl_age,tbl_geography, "geo_id", "geo_id")
tbl_pop_geo = dplyr::left_join(tbl_population,tbl_geography, "geo_id", "geo_id")
tbl_le_geo = dplyr::left_join(tbl_lifeexpectancy,tbl_geography, "geo_id", "geo_id")
tbl_race_geo = dplyr::left_join(tbl_race,tbl_geography, "geo_id", "geo_id")

# disconnect database
dbDisconnect(con) 

############################################# Data Wrangling #############################################
# dataframe wrangling
social_index_dataset<-social_index_dataset %>% mutate(sex = case_when(startsWith(social_index_dataset$variable,"Female") ~ "Female", 
                                                                      startsWith(social_index_dataset$variable,"Male")   ~ "Male", TRUE ~ "All"))
# add age column
social_index_dataset <- social_index_dataset %>%
  mutate(age = case_when(
    (social_index_dataset$dataset_id == 12 &(endsWith(social_index_dataset$variable, "years_percent insured estimate") | endsWith(social_index_dataset$variable,"older_percent insured estimate")))  ~ substr(social_index_dataset$variable, 1, nchar(social_index_dataset$variable)-25),TRUE ~ 'All'))

# add race column
social_index_dataset <- social_index_dataset %>%
  mutate(race = case_when(
    (social_index_dataset$dataset_id == 8 & social_index_dataset$sex == 'All' & endsWith(social_index_dataset$variable,"prison_pop_rate"))  ~ substr(social_index_dataset$variable, 1, nchar(social_index_dataset$variable)-16),
    (social_index_dataset$dataset_id == 8 & social_index_dataset$sex == 'All' & endsWith(social_index_dataset$variable,"jail_pop_rate"))  ~ substr(social_index_dataset$variable, 1, nchar(social_index_dataset$variable)-14),
    (social_index_dataset$dataset_id == 11 & social_index_dataset$sex == 'All')  ~ substr(social_index_dataset$variable, 1, nchar(social_index_dataset$variable)-15),
    (social_index_dataset$dataset_id == 12 & social_index_dataset$sex == 'All'& social_index_dataset$age == 'All')  ~ substr(social_index_dataset$variable, 1, nchar(social_index_dataset$variable)-25),
    (social_index_dataset$dataset_id == 15 & social_index_dataset$sex == 'All')  ~ substr(social_index_dataset$variable, 1, nchar(social_index_dataset$variable)-14),TRUE ~ 'All'))

# change variable column
social_index_dataset <- social_index_dataset %>%
  mutate(variables = case_when(
    (social_index_dataset$dataset_id == 8 & endsWith(social_index_dataset$variable,"prison_pop_rate"))  ~ "Prison Population Rate",
    (social_index_dataset$dataset_id == 8 & endsWith(social_index_dataset$variable,"jail_pop_rate"))  ~ "Jail Population Rate",
    (social_index_dataset$dataset_id == 11)  ~  "Native Analysis Value",
    (social_index_dataset$dataset_id == 12)  ~"Percent Insured Estimate",
    (social_index_dataset$dataset_id == 15)  ~"Percent Voted",
    TRUE ~social_index_dataset$variable))

social_index_dataset$race[social_index_dataset$race=='Total']  <- "All" 
social_index_dataset$race[social_index_dataset$race=='Aapi']  <- "Asian Americans and Pacific Islanders"
social_index_dataset$subdomain <- ifelse(is.na(social_index_dataset$subdomain), 'N/A', social_index_dataset$subdomain)
social_index_dataset$value[is.na(social_index_dataset$value)] <- '0'
social_index_dataset <- social_index_dataset %>%
  mutate(value = case_when(endsWith(social_index_dataset$value,"%") ~substr(social_index_dataset$value,0,nchar(social_index_dataset$value)-1),TRUE ~social_index_dataset$value))

social_index_dataset_copy <- social_index_dataset
# clean up non numeric values
social_index_dataset$value[social_index_dataset$dataset_id == 4 & social_index_dataset$value=='LE20']  <- "10"
social_index_dataset$value[social_index_dataset$dataset_id == 4 & social_index_dataset$value=='PS']  <- "60"
social_index_dataset$value[social_index_dataset$dataset_id == 4 & social_index_dataset$value=='LE5']  <- "2.5"
social_index_dataset$value[social_index_dataset$dataset_id == 4 & social_index_dataset$value=='GE95']  <- "97.5"
social_index_dataset$value[social_index_dataset$dataset_id == 4 & social_index_dataset$value=='LT50']  <- "25"
social_index_dataset$value[social_index_dataset$dataset_id == 4 & social_index_dataset$value=='GE50']  <- "75"
social_index_dataset$value[social_index_dataset$dataset_id == 4 & social_index_dataset$value=='LE10']  <- "5"
social_index_dataset$value[social_index_dataset$dataset_id == 4 & social_index_dataset$value=='GE90']  <- "95"
social_index_dataset$value[social_index_dataset$dataset_id == 4 & social_index_dataset$value=='GE80']  <- "90"
social_index_dataset$value[social_index_dataset$dataset_id == 4 & social_index_dataset$value=='GE99']  <- "99.5"
social_index_dataset$value[social_index_dataset$dataset_id == 4 & social_index_dataset$value=='LE1']  <- "0.5"

social_index_dataset$value <- apply(social_index_dataset[c("dataset_id", "value")], 1, function(df) {
  if (as.numeric(df["dataset_id"]) == 4 & is.na(as.numeric(df["value"]))) {
    splitted = strsplit(df["value"],"-")
    toString((as.numeric(splitted[[1]][2]) + as.numeric(splitted[[1]][1]) /2))
  } else {
    df["value"]
  }
})

social_index_dataset$value[social_index_dataset$dataset_id == 4 & is.na(as.numeric(social_index_dataset$value))] <- toString((as.numeric(strsplit(social_index_dataset$value,"-")[[1]][2]) + as.numeric(strsplit(social_index_dataset$value,"-")[[1]][1]))/2)

############################################# shape file #############################################
# get tract shapefile
wa_tracts <-tracts(state = "WA", county = c('King', 'Pierce','Yakima'))
md_tracts <-tracts(state = 'MD', county = c('Baltimore county', 'Baltimore city','Prince George','Montgomery'))
test_tracts = union(wa_tracts,md_tracts)
# get county shapefile
wa_counties <- counties(state = 'WA')
md_counties <- counties(state = 'MD')
test_counties <- union(wa_counties[,c("GEOID","geometry")],md_counties[,c("GEOID","geometry")])
#get state shapefile
test_states <- filter(states(),STUSPS == 'WA' | STUSPS == 'MD')
# get zcta shapefile
test_zcta<-zctas(starts_with = c("98", "99","20","21"))
test_area <- bind_rows(test_tracts,test_counties,test_states)

names(test_zcta)[1] <- 'GEOID'
test = union(test_area[,c("GEOID","geometry")],test_zcta[,c("GEOID","geometry")])


############################################### ui.R ##################################################
body <-shinyUI(
  tagList(tags$head(tags$style(type = 'text/css','.navbar-brand{display:none;}'),
                    tags$style( type = 'text/css',
                                ".selectize-input { word-wrap : break-word;}
                    .selectize-input { word-break: break-word;}
                    .selectize-dropdown {word-wrap : break-word;} "
                    )),
          fluidPage(theme = shinytheme("flatly"), 
                    collapsible = TRUE,
                    "",
                    sidebarPanel(
                      h3("Select Location"),
                      selectInput("line_state", "State:",choices=c("Washington","Maryland")),
                      selectInput("geo_level", "Geographic Scale:",choices=NULL),
                      selectInput("linelocation", "Geographic Area:",choices=NULL),
                      h3("Select Map Dataset"),
                      selectInput("domain","Domain:",choices=c("Resources: the tangible assets within a community",
                                                               "Connection: social and civic connection within a community",
                                                               "Opportunity: individual or household capacity to achieve goals",
                                                               "Structural Equity: the fairness of a community's systems and institutions"),selected = NULL),
                      selectInput("subdomain", "Subdomain:", choices=NULL,selected = NULL),
                      selectInput("indicator", "Indicator:", choices=NULL,selected = NULL),
                      selectInput("variable_name", "Variable:", choices=NULL,selected = NULL),
                      selectInput("sex", "Sex:",choices=NULL,selected = NULL),
                      selectInput("race", "Race:",choices=NULL),
                      selectInput("age", "Age:",choices=NULL),
                      selectInput("year", "Year:",choices=NULL),
                      width = 3
                    ),
                    mainPanel(
                      tags$style(type="text/css",
                                 ".shiny-output-error { visibility: hidden; }",
                                 ".shiny-output-error:before { visibility: hidden; }"
                      ),
                      tabsetPanel(
                        tabPanel("Map View", verbatimTextOutput("mapview"),
                                 fluidRow(column(width = 12, h3("Welcome to the Social Weather Map!",style='text-align:center'))),
                                 fluidRow(column(width = 12, "Use the left panel to filter data, and click on the map to switch between locations, trend comparisons, and profile views. Please note that data are not currently available for every county in every year, and estimates may change as we process more data.", 
                                                 style='font-family:Avenir, Helvetica;font-size:30;text-align:center')),
                                 fluidRow(column(12, wellPanel(tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar{
                                             background: #48C9B0;
                                             border-top: 1px solid #48C9B0 ; border-bottom: 1px solid #48C9B0}")),
                                                               div(class="outer",
                                                                   fluidRow(column(width = 12, div(id = "mymap", leaflet::leafletOutput("mymap", height = "60vh"))%>% withSpinner(color="#0dc5c1"))),
                                                                   absolutePanel(id = "controls", class = "panel panel-default",
                                                                                 top = 245, left = 1400, width = 260, fixed=TRUE,
                                                                                 draggable = TRUE, height = "auto",
                                                                                 tags$i(h4("Data Description", style="color:#045a8d;text-align:center")),
                                                                                 h5(textOutput("datadescription"),style="text-align:left"))),
                                                               fluidRow(column(width = 12, " ", style='padding:3px;')),
                                                               plotOutput("lineplot", height = "225px"), 
                                                               div(actionButton("twitter_share",
                                                                                label = "Twitter",
                                                                                icon = icon("twitter"),
                                                                                onclick = sprintf("window.open('%s')", "https://twitter.com/intent/tweet?text=Hello%20world&url=https://shiny.rstudio.com/gallery/widget-gallery.html/"),
                                                                                style='padding:6px; font-size:90%'),
                                                                   actionButton(
                                                                     "facebook_share",
                                                                     label = "Facebook",
                                                                     icon = icon("facebook-square"),
                                                                     onclick = sprintf("window.open('%s')", "https://www.facebook.com//"),
                                                                     style='padding:6px; font-size:90%'),
                                                                   actionButton(
                                                                     "linkedin_share",
                                                                     label = "LinkedIn",
                                                                     icon = icon("linkedin"),
                                                                     onclick = sprintf("window.open('%s')", "https://www.linkedin.com/feed/"),
                                                                     style='padding:6px; font-size:90%'),
                                                                   actionButton(
                                                                     "ins_share",
                                                                     label = "Instagram",
                                                                     icon = icon("instagram"),
                                                                     onclick = sprintf("window.open('%s')", "https://www.instagram.com/"),
                                                                     style='padding:6px; font-size:90%'),
                                                                   style = "display:inline-block; float:right")
                                 )))),
                        tabPanel("Map Data", verbatimTextOutput("viewdata"),
                                 fluidRow(column(width = 12, h3(textOutput("mapdatatitle"),style='text-align:center'))),
                                 fluidRow(column(width = 12, "Use the left panel to filter map data.", 
                                                 style='font-family:Avenir, Helvetica;font-size:30;text-align:center')),
                                 DT::dataTableOutput("mytable")%>% withSpinner(color="#0dc5c1")),
                        tabPanel("Profile View", verbatimTextOutput("locprofview"),
                                 fluidRow(column(width = 12, h3(textOutput("text"),style='text-align:center'))),
                                 fluidRow(column(width = 12, "Select location on left panel or click location on map to filter profile view. Mouse over the plots to see details and click camera icon to download plot.",
                                                 style='font-family:Avenir, Helvetica;font-size:30;text-align:center')),
                                 tags$head(tags$style(type='text/css', ".slider-animate-button { font-size: 20pt !important; }")),
                                 fluidRow(column(6, div(style = "margin: auto; width: 80%",
                                                        sliderInput("yearperiod", "Race Year Slider", value =1990, min = 1990, max=2021, sep = "",step=1,animate=TRUE,width = "100%")),
                                                 plotlyOutput("raceplot")),
                                          column(6, div(style = "margin: auto; width: 80%",
                                                        sliderInput("yearperiod2", "Age Year Slider", value =1990, min = 1990, max=2021, sep = "",step=1,animate=TRUE,width = "100%")),
                                                 plotlyOutput("ageplot"))),
                                 fluidRow(column(6, plotlyOutput("popplot")),
                                          column(6,  plotlyOutput("leplot")))
                        ),
                        tabPanel("Profile Data", verbatimTextOutput("Profdata"),
                                 fluidRow(column(width = 11, h3(textOutput("demogeotext"),style='text-align:center'))),
                                 fluidRow(column(width = 11, "Select location on left panel or click location on map to filter profile data.", 
                                                 style='font-family:Avenir, Helvetica;font-size:30;text-align:center')),
                                 radioButtons("selectdemo", "Select demographic:", choices=c("Age","Population","Life Expectancy","Race"),inline = TRUE),
                                 DT::dataTableOutput("profiledata")%>% withSpinner(color="#0dc5c1"),
                                 DT::dataTableOutput("allprofiledata"))
                        
                      ))))
)

############################################### server.R ##################################################

# Define server logic required to draw a histogram
server <- function(input,output,session) {
  domain <- reactive({
    #print("domain changed")
    if (input$domain == "Resources: the tangible assets within a community"){
      s_domain = "Resources"
    }
    else if (input$domain == "Connection: social and civic connection within a community"){
      s_domain = "Connection"
    }
    else if (input$domain == "Opportunity: individual or household capacity to achieve goals"){
      s_domain = "Opportunity"
    }
    else {
      s_domain="Structural Equity"
    }
    filter(social_index_dataset, social_index_dataset$domain==s_domain)
  })
  
  observeEvent(domain(), {
    #print("observe domain change,return subdomain choices")
    choice = sort(unique(domain()$subdomain))
    if ("Education Quality" %in% choice){
      new_list<-choice[! choice %in% c("Education Quality")]
      choice = c(c("Education Quality"),new_list)
    }
    updateSelectInput(session=session,"subdomain",choices = choice)
    #print(choice)
  })
  
  subdomain <- reactive({
    #print("subdomain changed")
    req(input$subdomain)
    filter(domain(), subdomain == input$subdomain)
  })
  
  observeEvent(subdomain(),{
    #print("observe subdomain change,return indicator choices")
    choices = sort(unique(subdomain()$indicator))
    updateSelectInput(session,"indicator",choices = choices)
  })
  
  indicator <- reactive({
    #print("indicator changed")
    req(input$indicator)
    filter(subdomain(), indicator == input$indicator)
  })
  observeEvent(indicator(),{
    #print("observe indicator change,return variable names choices")
    choices = sort(unique(indicator()$variables))
    updateSelectInput(session,"variable_name",choices = choices)
  })
  variable_name <- reactive({
    #print("variable_name changed")
    req(input$variable_name)
    filter(indicator(), variables== input$variable_name)
  })
  observeEvent(variable_name(),{
    #print("observe variable_name change,return sex choices")
    choices = sort(unique(variable_name()$sex))
    updateSelectInput(session,"sex",choices = choices)
  })
  
  sex <- reactive({
    #print("sex changed")
    req(input$sex)
    filter(variable_name(), sex == input$sex)
  })
  observeEvent(sex(), {
    #print("observe sex change,return race choices")
    choices = sort(unique(sex()$race))
    updateSelectInput(session,"race",choices = choices)
  })
  race <- reactive({
    #print("race changed")
    req(input$race)
    filter(sex(), race == input$race)
  })
  observeEvent(race(),{
    #print("observe race change,return age choices")
    choices = sort(unique(race()$age))
    updateSelectInput(session,"age",choices = choices)
  })
  age <- reactive({
    #print("age changed")
    req(input$age)
    filter(race(), age == input$age)
  })
  observeEvent(age(),{
    #print("observe age change,return geo_level choices")
    choices = sort(unique(age()$geo_level), decreasing=TRUE)
    updateSelectInput(session,"geo_level",choices = choices)
  })
  geo_level <- reactive({
    #print("geo_level changed")
    req(input$geo_level)
    filter(age(), geo_level == input$geo_level)
  })
  observeEvent({geo_level()},{
    #print("observe geo_level change,return year choices")
    choices = sort(unique(geo_level()$year))
    updateSelectInput(session,"year",choices = choices)
  })
  
  year <- reactive({
    #print("year changed")
    req(input$year)
    filter(geo_level(), year == input$year)
  })
  
  line_state <- reactive({
    #print("linestate changed")
    if(input$line_state == "Washington"){
      filter(geo_level(), grepl("Washington|WA",geo_name))
    }
    else{
      filter(geo_level(), grepl("Maryland|MD",geo_name))
    }
  })
  
  observeEvent({line_state()},{
    updateSelectInput(session,"linelocation",choices = sort(unique(line_state()$geo_name)))
  })
  
  linelocation <- reactive({
    #print("location changed")
    req(input$linelocation)
    filter(line_state(), geo_name == input$linelocation)
  })
  
  ############################################# draw map #############################################
  output$mymap <- renderLeaflet({
    #print("observe geo level change, change map")
    req(input$year)
    print(c("before year", Sys.time()))
    f_data <- year()
    f_data$geo_id = substr(f_data$geo_id,3,nchar(f_data$geo_id))
    names(f_data)[1] <- 'GEOID'
    print(c("before join", Sys.time()))
    mapdata_merged <- dplyr::left_join(f_data,test[,c("GEOID","geometry")], "GEOID", "GEOID")
    print(c("before aggregate", Sys.time()))
    if (unique(mapdata_merged$dataset_id == 4)){
      new_filtered <- aggregate(as.numeric(value) ~ GEOID + geo_name + variables , data = mapdata_merged, FUN = mean)
      names(new_filtered)[1] <- 'GEOID'
      names(new_filtered)[2] <- 'geo_name'
      names(new_filtered)[3] <- 'variables'
      names(new_filtered)[4] <- 'value'
      new_merged <- dplyr::inner_join(new_filtered,test[,c("GEOID","geometry")], "GEOID","GEOID")
      mapdata_merged <- new_merged
    }
    
    # transfer to spatial dataset
    mapdata_merged_sf <-st_as_sf(mapdata_merged)   
    
    pal_fun_num <- colorBin("Blues", domain =as.numeric(mapdata_merged_sf$value), bins = 7)
    
    ############################################# popup,data description,and map color #############################################
    if (unique(mapdata_merged_sf$variables) == "Percent Voted"){
      p_popup <- paste0("<strong> Location: </strong>",mapdata_merged_sf$geo_name,"<br/>",
                        "<strong> Social Weather Index: </strong>",mapdata_merged_sf$variables,"<br/>",
                        "<strong> Total estimate: </strong>", mapdata_merged_sf$value,"%")
      output$datadescription <- renderText({ 
        paste0("The map depicts reported voting percent data in state-level for the 2020 election")
      })
    }
    else if (unique(mapdata_merged_sf$variables) == "Percent of householder living alone"){
      p_popup <- paste0("<strong> Location: </strong>",mapdata_merged_sf$geo_name,"<br/>",
                        "<strong> Social Weather Index: </strong>",mapdata_merged_sf$variables,"<br/>",
                        "<strong> Total estimate: </strong>", mapdata_merged_sf$value,"%")
      pal_fun_num <- colorBin("GnBu", domain =as.numeric(mapdata_merged_sf$value), bins = 7)
      output$datadescription <- renderText({ 
        paste0("The map depicts the rate of people/householder living alone in county level")
      })
      
    }
    else if (unique(mapdata_merged_sf$variables) == "Percent Insured Estimate"){
      p_popup <- paste0("<strong> Location: </strong>",mapdata_merged_sf$geo_name,"<br/>",
                        "<strong> Social Weather Index: </strong>",mapdata_merged_sf$variables,"<br/>",
                        "<strong> Total estimate: </strong>", mapdata_merged_sf$value,"%")
      pal_fun_num <- colorBin("Purples", domain =as.numeric(mapdata_merged_sf$value), bins = 7)
      output$datadescription <- renderText({ 
        paste0("The map depicts percent of health insurance coverage in county level")
      })
    }
    else if (unique(mapdata_merged_sf$variables) == "Food insecurity rate"){
      p_popup <- paste0("<strong> Location: </strong>",unique(mapdata_merged_sf$geo_name),"<br/>",
                        "<strong> Social Weather Index: </strong>",unique(mapdata_merged_sf$variables),"<br/>",
                        "<strong> Total estimate: </strong>", unique(mapdata_merged_sf$value),"%")
      pal_fun_num <- colorBin("YlOrRd", domain =as.numeric(mapdata_merged_sf$value), bins = 7)
      output$datadescription <- renderText({ 
        paste0("Food insecruity rate is the  number of people who may be food insecure in every U.S. county, Map the Meal Gap uses publicly available state and local data from the U.S. Census Bureau and Bureau of Labor Statistics on factors that research has shown to contribute to food insecurity. 
               These factors include unemployment and poverty, as well as other demographic and household characteristics. 
               In addition to measuring how pervasive the need is, the study also estimates the cost of a meal, 
               and the amount of need among people who are food insecure, using local data from Nielsen and national survey data from the Census Bureau.")
      })
    }
    else if (unique(mapdata_merged_sf$variables) == "Metro area cost-burdened households (%)"){
      p_popup <- paste0("<strong> Metro Area: </strong>", mapdata_merged_sf$metro_area,"<br/>",
                        "<strong> Location: </strong>",unique(mapdata_merged_sf$geo_name),"<br/>",
                        "<strong> Social Weather Index: </strong>",unique(mapdata_merged_sf$variables),"<br/>",
                        "<strong> Total estimate: </strong>", unique(mapdata_merged_sf$value),"%")
      output$datadescription <- renderText({ 
        paste0("The map depicts Share of households with cost-burdens (percent) in county level. Cost-burdened (severely cost-burdened) households pay more than 30% (more than 50%) of income for housing.
        Households with zero or negative income are assumed to have severe burdens, while households paying no cash rent are assumed to be without burdens. Monthly housing costs include the contract rent and utilities for renter households. 
               For homeowners, monthly housing costs include any mortgage payments, property taxes, insurance, utilities, and condominium or mobile home fees.
               The data we use is Metro area cost-burdened households (%), so county in the same metro area share the same data point")
      })
      pal_fun_num <- colorBin("PuBu", domain =as.numeric(mapdata_merged_sf$value), bins = 7)
    }
    else if (unique(mapdata_merged_sf$variables) == "Total number of household with internet subscription"){
      total_state = sum(as.numeric(mapdata_merged_sf$value))
      p_popup <- paste0("<strong> Location: </strong>",mapdata_merged_sf$geo_name,"<br/>",
                        "<strong> Social Weather Index: </strong>",mapdata_merged_sf$variables,"<br/>",
                        "<strong> Total estimate: </strong>", mapdata_merged_sf$value,"(",round(as.numeric(mapdata_merged_sf$value)/total_state*100,2),"%)")
      output$datadescription <- renderText({ 
        paste0("The map shows the total number of Households with an Internet subscription. It is census data from 2018.")
      })
      
    }
    else if (unique(mapdata_merged_sf$variables) == "Children aged 3-4 enrolled in school (percent)"){
      p_popup <- paste0("<strong> Location: </strong>",mapdata_merged_sf$geo_name,"<br/>",
                        "<strong> Social Weather Index: </strong>",mapdata_merged_sf$variables,"<br/>",
                        "<strong> Total estimate: </strong>", mapdata_merged_sf$value,"%")
      output$datadescription <- renderText({ 
        paste0("Children aged 3-4 enrolled in school (percent) is 1-year data that depicts the number of children aged 3-4 years enrolled in public or private school divided by the number of children aged 3-4 years, times 100.", sep="\n",
               "Data available for 2009-2017, except for city neighborhoods (2010-2017), ZIP code tabulation areas (2011-2017), and census tracts (2012 & 2017).",sep="\n",
               "Sources: American Community Survey 1-Year Summary Files, 2009-2017, Table(s): B14003. American Community Survey 5-Year Summary Files, 2009-2017, Table(s): B14003.")
      })
    }
    else if (unique(mapdata_merged_sf$variables) == "Adjusted cohort graduation rate"){
      total_state = sum(as.numeric(mapdata_merged_sf$value))
      p_popup <- paste0("<strong> Location: </strong>",mapdata_merged_sf$geo_name,"<br/>",
                        "<strong> Social Weather Index: </strong>",mapdata_merged_sf$variables,"<br/>",
                        "<strong> Total estimate: </strong>",round(as.numeric(mapdata_merged_sf$value),2),"%")
      output$datadescription <- renderText({ 
        paste0("Adjusted cohort graduation rate is the number of students who graduate in four years or less with a  regular high school diploma divided by the number of students who form the adjusted-cohort.
               The graduation rate shown on the map is calculated by taking the average of the graduation rates of all schools in each county.
               Please check 'Map Data' page for details of graduation rate for each school.")
      })
      pal_fun_num <- colorBin("BuPu", domain =as.numeric(mapdata_merged_sf$value), bins = 7)
      
    }
    else if (unique(mapdata_merged_sf$variables) == "National risk index score"){
      p_popup <- paste0("<strong> Location: </strong>",mapdata_merged_sf$geo_name,"<br/>",
                        "<strong> Social Weather Index: </strong>",mapdata_merged_sf$variables,"<br/>",
                        "<strong> Total estimate: </strong>", round(as.numeric(mapdata_merged_sf$value),2))
      output$datadescription <- renderText({ 
        paste0("The National Risk Index (NRI) is an online tool to help illustrate the nation's communities most at risk of natural hazards. 
        It leverages authoritative nationwide datasets and multiplies values for exposure, hazard frequency, and historic loss ratios to derive
        Expected Annual Loss for 18 natural hazards; and it combines this metric with Social Vulnerability and Community Resilience
               data to generate a unitless, normalized Risk Index score for every census tract and county in the United States.",sep="\n",
               "The NRI incorporates data for the following natural hazards: Avalanche, Coastal Flooding, Cold Wave, Drought, Earthquake, Hail, Heat Wave,
               Hurricane, Ice Storm, Landslide, Lightning, Riverine Flooding, Strong Wind, Tornado, Tsunami, Volcanic Activity, Wildfire, and Winter Weather.")
      })
      pal_fun_num <- colorBin("YlOrRd", domain =as.numeric(mapdata_merged_sf$value), bins = 7)
    }
    
    else if (unique(mapdata_merged_sf$variables) == "County-level real gdp"){
      total_state = sum(as.numeric(mapdata_merged_sf$value))
      p_popup <- paste0("<strong> Location: </strong>",mapdata_merged_sf$geo_name,"<br/>",
                        "<strong> Social Weather Index: </strong>",mapdata_merged_sf$variables,"<br/>",
                        "<strong> Total estimate: </strong>", as.numeric(mapdata_merged_sf$value),"(",round(as.numeric(mapdata_merged_sf$value)/total_state*100,2),"%)")
      output$datadescription <- renderText({ 
        paste0("County level real gdp is a comprehensive measure of the economies of counties. 
               Gross domestic product estimates the value of the goods and services produced in an area. 
               It can be used to compare the size and growth of county economies across the nation.")
      })
    }
    else if (unique(mapdata_merged_sf$variables) == "Gdp percent change from preceding period"){
      p_popup <- paste0("<strong> Location: </strong>",mapdata_merged_sf$geo_name,"<br/>",
                        "<strong> Social Weather Index: </strong>",mapdata_merged_sf$variables,"<br/>",
                        "<strong> Total estimate: </strong>", as.numeric(mapdata_merged_sf$value),"%")
      output$datadescription <- renderText({ 
        paste0("Gdp percent change from preceding period is the percent of real Gdp changes from preceding year. 
        Real gdp is a comprehensive measure of the economies of counties, metropolitan statistical areas, and some other local areas. 
        Gross domestic product estimates the value of the goods and services produced in an area. 
               It can be used to compare the size and growth of county economies across the nation.")
      })
    }
    else if (unique(mapdata_merged_sf$variables) == "Hudper100"){
      p_popup <- paste0("<strong> Location: </strong>",mapdata_merged_sf$geo_name,"<br/>",
                        "<strong> Social Weather Index: </strong>",mapdata_merged_sf$variables,"<br/>",
                        "<strong> Total estimate: </strong>",round(as.numeric(mapdata_merged_sf$value),2))
      output$datadescription <- renderText({ 
        paste0("Hudper100 is U.S. Department of Agriculture (HUD) assisted units per 100 extremely low-income (ELI) renter households at the county level in the United States (=(HUD/Total)*100)")
      })
      
    }
    else if (unique(mapdata_merged_sf$variables) == "Usdaper100"){
      p_popup <- paste0("<strong> Location: </strong>",mapdata_merged_sf$geo_name,"<br/>",
                        "<strong> Social Weather Index: </strong>","USDAper100","<br/>",
                        "<strong> Total estimate: </strong>",round(as.numeric(mapdata_merged_sf$value),2),"<br/>",
                        "<strong> Data Description: </strong>","USDAper100 is USDA units per 100 ELI renters (=(USDA/Total)*100)")
      output$datadescription <- renderText({ 
        paste0("USDAper100 is U.S. Department of Agriculture (USDA) assisted units per 100 extremely low-income (ELI) renter households at the county level in the United States (=(USDA/Total)*100)")
      })
      
    }
    else if (unique(mapdata_merged_sf$variables) == "Native Analysis Value"){
      p_popup <- paste0("<strong> Location: </strong>",mapdata_merged_sf$geo_name,"<br/>",
                        "<strong> Social Weather Index: </strong>",mapdata_merged_sf$variables,"<br/>",
                        "<strong> Total estimate: </strong>", mapdata_merged_sf$value)
      output$datadescription <- renderText({ 
        paste0("The Native Analysis Value is emergency department visit rate per 1,000 beneficiaries")
      })
      
      pal_fun_num <- colorBin("Purples", domain =as.numeric(mapdata_merged_sf$value), bins = 7)
      
    }
    else if (unique(mapdata_merged_sf$variables) == "Jail Population Rate" | unique(mapdata_merged_sf$variables) == "Prison Population Rate"){
      p_popup <- paste0("<strong> Location: </strong>",mapdata_merged_sf$geo_name,"<br/>",
                        "<strong> Social Weather Index: </strong>",unique(mapdata_merged_sf$variables),"<br/>",
                        "<strong> Total estimate: </strong>", mapdata_merged_sf$value," per 100,000 residents age 15-64")
      pal_fun_num <- colorBin("YlOrRd", domain =as.numeric(mapdata_merged_sf$value), bins = 7)
      output$datadescription <- renderText({ 
        paste0("The map depicts ", unique(mapdata_merged_sf$variables), " Per 100,000 residents age 15-64. Annual demographic data for county
               resident population from the U.S Census Bureau are used in combination with the Jail and Prison popluation to calculate ", unique(mapdata_merged_sf$variables))
      })
    }
    else if (unique(mapdata_merged_sf$variables) == "Top-to-bottom ratio"){
      p_popup <- paste0("<strong> Location: </strong>",mapdata_merged_sf$geo_name,"<br/>",
                        "<strong> Social Weather Index: </strong>",mapdata_merged_sf$variables,"<br/>",
                        "<strong> Total estimate: </strong>", mapdata_merged_sf$value)
      output$datadescription <- renderText({ 
        paste0("Ratio of top 1% income to bottom 99% income for all U.S. counties")
      })
      pal_fun_num <- colorBin("YlOrRd", domain =as.numeric(mapdata_merged_sf$value), bins = 7)
      
    }
    else{
      p_popup <- paste0("<strong> Location: </strong>",unique(mapdata_merged_sf$geo_name),"<br/>",
                        "<strong> Social Weather Index: </strong>",unique(mapdata_merged_sf$variables),"<br/>",
                        "<strong> Total estimate </strong>", round(as.numeric(mapdata_merged_sf$value),2))
    }
    if (unique(mapdata_merged_sf$variables) == "Violent crimes per 100,000"){
      pal_fun_num <- colorBin("YlOrRd", domain =as.numeric(mapdata_merged_sf$value), bins = 7)
      output$datadescription <- renderText({ 
        paste0("The map depicts violent crime rate per 100,000 population in county level.")
      })
    }  
    else if (unique(mapdata_merged_sf$variables) == "Median air quality"){
      pal_fun_num <- colorBin("YlGn", domain =as.numeric(mapdata_merged_sf$value), bins = 7)
      output$datadescription <- renderText({ 
        paste0("The map depicts daily median AQI by County for 1980-2021. 
        The Air Quality Index (AQI) values is calculated each day for each monitor for the Criteria Gases and PM10 and PM2.5 (FRM and non FRM).")
      })
    }
    
    req(input$geo_level)
    req(input$line_state)
    
    # set zoom level for different state
    if (input$line_state=="Washington"){
      lng = -120.740135
      lat = 47
    }
    else{
      lng = -76.61
      lat = 39.3
    }
    if(input$line_state =="Washington"){
      if(input$geo_level == "State"){
        zoom = 6
      }
      else if (input$geo_level == 'County' | input$geo_level == 'Tract'){
        zoom = 7.5
      }
      else{
        zoom=6.5
      }
    }
    else if(input$line_state == "Maryland"){
      if(input$geo_level == 'State'){
        zoom=6.5
      }
      else if (input$geo_level == 'ZCTA'){
        zoom = 7.5
      }
      else{
        zoom = 8.5
      }
    }
    # draw map
    leaflet(mapdata_merged_sf, options=leafletOptions(preferCanvas = TRUE)) %>%
      addProviderTiles(provider = "CartoDB") %>%
      addPolygons(
        layerId = mapdata_merged_sf$geo_name,
        color = "black",
        weight = 1,
        stroke = TRUE, 
        smoothFactor = 1, 
        fillOpacity = 0.55,
        fillColor = ~pal_fun_num(as.numeric(value)), # set fill color with function from above and value
        #popup = p_popup,
        label = ~lapply(p_popup,HTML),
        highlightOptions = highlightOptions(weight = 2,
                                            color = "white",
                                            fillOpacity = 0.7, 
                                            opacity =1,
                                            bringToFront = T)) %>% 
      addLegend("bottomright",  # location
                pal = pal_fun_num,     # palette function
                values = ~as.numeric(value),
                title = unique(mapdata_merged_sf$variables)) %>% # legend title 
      setView(lng = lng, lat = lat, zoom = zoom)
  })
  
  # draw line trend comparison
  observeEvent(linelocation(), {
    output$lineplot <- renderPlot({
      lineloc<-linelocation()
      geo_level_data <-geo_level()
      if (unique(geo_level_data$variables) == "Adjusted cohort graduation rate"){
        lineloc <- aggregate(as.numeric(lineloc$value), list(lineloc$year,lineloc$variables,lineloc$geo_name),FUN = mean)
        names(lineloc)[1] <- 'year'
        names(lineloc)[2] <- 'variables'
        names(lineloc)[3] <- 'geo_name'
        names(lineloc)[4] <- 'value'
        geo_level_data <- aggregate(as.numeric(geo_level_data$value), list(geo_level_data$geo_level,geo_level_data$year,geo_level_data$geo_name), FUN=mean)
        names(geo_level_data)[1] <- 'geo_level'
        names(geo_level_data)[2] <- 'year'
        names(geo_level_data)[3] <- 'geo_name'
        names(geo_level_data)[4] <- 'value'
        geo_level_data<-unique(geo_level_data)
      }
      
      US_values <- aggregate(as.numeric(geo_level_data$value), list(geo_level_data$year), FUN=mean)
      names(US_values)[1] <- 'year'
      names(US_values)[2] <- 'value'
      
      
      plotdata = merge(lineloc[,c("year","variables","value")], US_values[,c("year","value")], by="year")
      names(plotdata)[3] <- unique(lineloc$geo_name)
      names(plotdata)[4] <- 'Average'
      datamelted <- reshape2::melt(plotdata[,c("year", unique(lineloc$geo_name),"Average")], id.var='year')
      ggplot(datamelted, aes(x=year, y=as.numeric(value), col=variable,group=variable)) +
        geom_line(aes(linetype=variable, color=variable))+
        geom_point(size=2)+
        ggtitle(paste0(unique(lineloc$variables), " trend line comparison"))
    })
  })
  
  observeEvent(input$mymap_shape_click, {
    req(input$year)
    f_data <- year()
    f_data$geo_id = substr(f_data$geo_id,3,nchar(f_data$geo_id))
    names(f_data)[1] <- 'GEOID'
    mapdata_merged <- dplyr::left_join(f_data,test[,c("GEOID","geometry")], "GEOID", "GEOID")
    if (unique(mapdata_merged$dataset_id == 4)){
      new_filtered <- aggregate(as.numeric(value) ~ GEOID, data = mapdata_merged, FUN = mean)
      names(new_filtered)[1] <- 'GEOID'
      names(new_filtered)[2] <- 'value'
      new_merged <- dplyr::inner_join(new_filtered,test[,c("GEOID","geometry")], "GEOID","GEOID")
      mapdata_merged_sf <- new_merged
    }
    # transfer to spatial dataset
    mapdata_merged_sf <-st_as_sf(mapdata_merged)   
    
    #print("clicked map")
    p <- input$mymap_shape_click 
    if(!is.null(p$id)){
      selectlevel=filter(mapdata_merged_sf,geo_name == p$id)$geo_level
      updateSelectInput(session, "geo_level",choices = sort(unique(age()$geo_level),decreasing=TRUE),
                        selected = selectlevel)
      if (grepl("Washington|WA",p$id)){
        updateSelectInput(session, "line_state",choices = c("Washington","Maryland"),
                          selected = "Washington")
      }
      else
        updateSelectInput(session, "line_state",choices = c("Washington","Maryland"),
                          selected = "Maryland")
      updateSelectInput(session, "linelocation",choices = sort(unique(line_state()$geo_name)),
                        selected = p$id)
    }
  }) 
  
  ############ Map data ##########
  output$mapdatatitle <- renderText({ 
    #print("mapdatatitle")
    paste0(unique(linelocation()$geo_name)," Social Weather Map Data Table")
  })
  
  output$mytable = DT::renderDataTable({
    #print("mapdatatable")
    x <-linelocation()
    names(x)[5] <- 'values'
    names(x)[11] <- 'geo_names'
    names(x)[12] <- 'geo_levels'
    names(x)[13]<- "metro_areas"
    names(social_index_dataset_copy)[17] <- 'variables1'
    y <- dplyr::left_join(x,social_index_dataset_copy,by=c("dataset_id"="dataset_id","domain"="domain","subdomain"="subdomain",
                                                           "indicator"="indicator","variables"="variables1","year"="year","sex"="sex","race" ="race","age"="age",
                                                           "geo_id"="geo_id","NCESSCH" ="NCESSCH","SCHNAM"="SCHNAM","metro_areas"="metro_area"))
    y<- y[,c("domain","subdomain","indicator","variables","sex","race","age","year","value","geo_name","NCESSCH","SCHNAM")]
    datatable(y,
              rownames = F,
              extensions = 'Buttons',
              options = list(paging = TRUE,
                             scrollX=TRUE, 
                             ordering = TRUE,
                             dom = 'Bfrtip',
                             buttons = c('copy', 'csv', 'excel', 'pdf'),
                             pageLength=11, 
                             lengthMenu=c(5,10,15)),
              colnames=c("domain","subdomain","indicator","variables","sex","race","age","year","value","geo_name","NCESSCH","SCHNAM"))
    
  })
  
  
  #######################location profile view########################
  output$text <- renderText({ 
    #print("mapdatatable")
    paste0(input$linelocation," Demographics")
  })
  
  ############ race ##########
  racedata <- reactive({
    filter(tbl_race_geo,geo_name == input$linelocation)
  })
  
  observeEvent(racedata(),{
    #print("update race year slider")
    choices = sort(unique(racedata()$year))
    updateSliderInput(session,'yearperiod', value=range(choices),
                      min = min(as.numeric(choices)), max = max(as.numeric(choices)), step = 1)
  })
  yearperiod <- reactive({
    #print("year changed")
    filter(filter(racedata(), year == input$yearperiod),label != "estimate_total_population")
  })
  
  output$raceplot <- renderPlotly({
    #print("raceplot")
    racedataset <- yearperiod()
    all_race = sum(as.numeric(racedataset$estimate))
    
    
    
    color_r = c('rgb(253, 231, 37)','rgb(160, 218, 57)','rgb(74, 193, 109)',' rgb(31, 161, 135)','rgb(39, 127, 142)', 'rgb(54, 92, 141)', 'rgb(70, 50, 126)','rgb(68, 1, 84)')
    fig <- racedataset %>% plot_ly(labels = ~ factor(label), values = ~as.numeric(estimate),
                                   marker = list(colors = color_r,
                                                 line = list(color = '#FFFFFF', width = 1)))
    fig <- fig %>% add_pie(hole = 0.6)
    fig <- fig %>% layout(title = "Population by Race",  showlegend = F,
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    fig
  })
  
  ############ age ##########
  agedata <- reactive({
    filter(tbl_age_geo,geo_name == input$linelocation)
  })
  
  observeEvent(agedata(),{
    #print("update age year slider")
    choices = sort(unique(agedata()$year))
    updateSliderInput(session,'yearperiod2', value=range(choices),
                      min = min(as.numeric(choices)), max = max(as.numeric(choices)), step = 1)
  })
  yearperiod2 <- reactive({
    #print("year changed")
    filter(agedata(), year == input$yearperiod2)
  })
  
  output$ageplot <- renderPlotly({
    #print("ageplot")
    agedataset <- yearperiod2()
    all_age <- sum(as.numeric(agedataset$estimate))
    
    agedataset$label <- factor(agedataset$label, levels = agedataset$label)
    
    color_a = c('rgb(253, 231, 37)', 'rgb(216, 226, 25)', 'rgb(176, 221, 47)','rgb(137, 213, 72)','rgb(101, 203, 94)','rgb(70, 192, 111)','rgb(46, 179, 124)', 'rgb(33, 165, 133)',
                'rgb(31, 151, 139)', 'rgb(35, 137, 142)', 'rgb(41, 123, 142)','rgb(46, 109, 142)','rgb(53, 94, 141)', 'rgb(61, 78, 138)','rgb(67, 61, 132)', 'rgb(71, 42, 122)',
                'rgb(72, 23, 105)','rgb(68, 1, 84)')
    fig_a <- plot_ly(agedataset, x = ~as.numeric(estimate),y=agedataset$label, type = 'bar', orientation = 'h',
                     text = paste0(round(as.numeric(agedataset$estimate)/all_age*100,2),"%"), textposition = 'auto',
                     marker = list(color = color_a))
    
    fig_a <- fig_a %>% 
      layout(title ="Population by Age Group",  showlegend = F,
             xaxis = list(title = "Population",showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
             yaxis = list(title = "Age Group",showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE))
    fig_a
  })
  
  
  ############ population ##########
  popdata <- reactive({
    filter(tbl_pop_geo,geo_name == input$linelocation)
  })
  
  output$popplot <- renderPlotly({
    #print("poplot")
    popdataset <- popdata()
    
    color_p = c("rgb(253, 231, 37)"," rgb(181, 222, 43)", "rgb(110, 206, 88)", "rgb(53, 183, 121)","rgb(31, 158, 137)",' rgb(38, 130, 142)',
                'rgb(49, 104, 142)', 'rgb(62, 73, 137)','rgb(72, 40, 120)', 'rgb(68, 1, 84)')
    fig_p <- plot_ly(popdataset, x = ~year,y=popdataset$estimate, type = 'bar',
                     marker = list(color = color_p))
    
    fig_p <- fig_p %>% layout(title = "Population by Year",  showlegend = F,
                              xaxis = list(type = 'category', title = "Year",showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE,tickmode = 'linear'),
                              yaxis = list(title = "Population",showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE))
    fig_p
  })
  
  ############ life expectancy ##########
  ledata <- reactive({
    filter(tbl_le_geo,geo_name == input$linelocation)
  })
  
  observeEvent(ledata(),{
    ledata<-ledata()
    #print(ledata)
    output$leplot <- renderPlotly({
      ledataset <- ledata()
      
      color_l = c("rgb(253, 231, 37)"," rgb(181, 222, 43)", "rgb(110, 206, 88)", "rgb(53, 183, 121)","rgb(31, 158, 137)",' rgb(38, 130, 142)',
                  'rgb(49, 104, 142)', 'rgb(62, 73, 137)','rgb(72, 40, 120)', 'rgb(68, 1, 84)')
      fig_l <- plot_ly(ledataset, x = ~year,y=ledataset$estimate, type = 'bar',
                       marker = list(color = color_l))
      
      fig_l <- fig_l  %>% 
        layout(title = "Life Expectancy by Year",  showlegend = F,
               xaxis = list(type = 'category', title = "Year",showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE,tickmode = 'linear'),
               yaxis = list(title = "Population",showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE))
    })
  })
  
  
  ############## profile data ##################
  output$demogeotext <- renderText({ 
    paste0(input$linelocation," Demograhics Data Table")
  })
  
  selectdemo <- reactive({
    req(input$selectdemo)
    req(input$linelocation)
    #print("select selected demo changed")
    if (input$selectdemo=="Age"){
      filter(tbl_age_geo,geo_name == input$linelocation)
    }
    else if (input$selectdemo=="Race"){
      filter(tbl_race_geo,geo_name == input$linelocation)
    }
    else if (input$selectdemo=="Population"){
      filter(tbl_pop_geo,geo_name == input$linelocation)
    }
    else{
      filter(tbl_le_geo,geo_name == input$linelocation)
      
    }
  })
  
  observeEvent(selectdemo(),{
    #print("select demo")
    output$profiledata = DT::renderDataTable({
      datatable(selectdemo(),
                rownames = F,
                extensions = 'Buttons',
                options = list(paging = TRUE,
                               scrollX=TRUE, 
                               ordering = TRUE,
                               dom = 'Bfrtip',
                               buttons = c('copy', 'csv', 'excel', 'pdf'),
                               pageLength=19, 
                               lengthMenu=c(5,10,15)))
    })
  })
}

shinyApp(body, server)