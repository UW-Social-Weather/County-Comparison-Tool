################################ COUNTY COMPARISON TOOL DASHBOARD ################################ 

# Load packages  -----------------------------------------------------------------
library(shiny)
library(shinyjs)
library(shinythemes)
library(leaflet)
library(leafdown)
library(echarts4r)
library(plyr)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(shinydashboard)
library(shinyWidgets)
library(readr)
library(readxl)
library(data.table)
library(stringr)


# Resources -----------------------------------------------------------------

# This sourced the server/UI files when they were separate 
# source("healthdown/healthdown.R")
# source("healthdown/healthdown_ui.R")

source("healthdown/helpers/line_graph.R")
source("healthdown/helpers/line_graph_full.R")
source("healthdown/helpers/utils.R")
source("healthdown/helpers/table.R")
# unused plot function files
# source("healthdown/helpers/scatter_plot.R")
# source("healthdown/helpers/bar_chart.R")

states <- readRDS("shapes/us1.RDS")
counties <- readRDS("shapes/us2.RDS")
spdfs_list <- list(states, counties)


# Load data  -----------------------------------------------------------------

# load old data files to get state abbrevs
us_health_states_old <- readr::read_delim(
  "data/clean/us_health_states.csv", ";",
  escape_double = FALSE, trim_ws = TRUE,
  col_types = readr::cols(),
  locale = readr::locale(decimal_mark = ",", grouping_mark = ".")
)
us_health_counties_old <- readr::read_delim(
  "data/clean/us_health_counties.csv", ";",
  escape_double = FALSE, trim_ws = TRUE,
  col_types = readr::cols(),
  locale = readr::locale(decimal_mark = ",", grouping_mark = ".")
)
# get ST abbrevs
us_health_old <- rbind(us_health_states_old, us_health_counties_old)
st_fips <- us_health_old %>% select("FIPS","ST")
st_fips <- st_fips %>% distinct()

# load health measure WCHRR data
us_wchrr_all <- readr::read_delim(
  "data/clean/all_wchrr_updatedv2.csv", ";",
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
pop_grouping <- dplyr::rename(pop_grouping, ST = State)
pop_grouping <- pop_grouping %>% mutate(Classification = paste0(RUCC_2013, ": ", Description))
all_pop <- unique(na.omit(pop_grouping$Classification))
all_pop <- all_pop[!grepl("NA:", all_pop)]
all_pop <- str_sort(all_pop)
all_pop

# clean up SVI data
SVI_grouping_data <- dplyr::rename(SVI_grouping_data, ST_NUM = ST)
SVI_grouping_data <- dplyr::rename(SVI_grouping_data, ST = ST_ABBR)
SVI_grouping_data$FIPS <- sprintf("%05d",SVI_grouping_data$FIPS)
all_SVI <- unique(na.omit(SVI_grouping_data$SVI_Group))
all_SVI

#all_years <- unique(us_wchrr_all$year)

# clean up health measure data 
us_wchrr_all <- us_wchrr_all %>% mutate_if(is.numeric, round, digits = 1)
us_wchrr_all <- dplyr::rename(us_wchrr_all, NAME_2 = County)
# add ST abbrevs
us_wchrr_all <- merge(st_fips, us_wchrr_all, by = "FIPS", all.y=TRUE)
us_wchrr_all <- us_wchrr_all %>% arrange(FIPS, year)

# get missing years of state data 
us_health_old <- select(us_health_old, -contains("-Z"))
us_health_missing <- us_health_old %>% filter(is.na(NAME_2))
us_health_missing <- us_health_missing %>% filter(year %in% (2014:2019))
us_wchrr_all <- us_wchrr_all[, c(3,1,4,2,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,
                                 20,21,22,23,24,25,26,27,28,29,30,31,32,33)]
us_wchrr_all <- rbind.fill(us_wchrr_all, us_health_missing)
us_wchrr_all <- us_wchrr_all %>% arrange(FIPS, year)

# remove unnecessary vars 
us_wchrr_all <- us_wchrr_all[, !grepl("-CI", colnames(us_wchrr_all))]
us_wchrr_all <- us_wchrr_all[!grepl("Cases", colnames(us_wchrr_all))]
us_wchrr_all <- us_wchrr_all[!grepl("deaths", colnames(us_wchrr_all))]

# Merging data files -----------------------------------------------------------------

# add in SVI data
us_health_all <- merge(us_wchrr_all, SVI_grouping_data, by=c("FIPS","ST","NAME_2"), all.x = TRUE) 
us_health_all <- dplyr::rename(us_health_all, State = State.x)

# add in population data
us_health_all <- merge(us_health_all, pop_grouping, by=c("FIPS","ST"), all.x= TRUE)
us_health_all <- us_health_all %>% select(-ST_NUM, -State.y, -LOCATION, -AREA_SQMI, -County_Name, -Population_2010, -RUCC_2013, -Description)
us_health_all <- us_health_all %>% arrange(FIPS, year)

# state data only 
us_health_states <- us_health_all %>% filter(is.na(NAME_2))
# county data only; with SVI and Population data
us_health_counties1 <- us_health_all %>% filter(!is.na(NAME_2))

# clean up health outcome variable list & get vars in each year for reactive filter options 
health_vars <- sort(names(us_health_all)[6:15]) 

health_vars_nocovid <- health_vars[!grepl("COVID", health_vars)]
health_vars_nocovidlifeexp <- health_vars_nocovid[!grepl("expectancy", health_vars_nocovid)]
health_vars_2011 <- as.data.frame(health_vars_nocovidlifeexp)
colnames(health_vars_2011) <- "health_vars"
health_vars_2011$year <- "2011" 
health_vars_2012 <- as.data.frame(health_vars_nocovidlifeexp)
colnames(health_vars_2012) <- "health_vars"
health_vars_2012$year <- "2012" 
health_vars_2013 <- as.data.frame(health_vars_nocovidlifeexp)
colnames(health_vars_2013) <- "health_vars"
health_vars_2013$year <- "2013" 
health_vars_2014 <- as.data.frame(health_vars_nocovidlifeexp)
colnames(health_vars_2014) <- "health_vars"
health_vars_2014$year <- "2014" 
health_vars_2015 <- as.data.frame(health_vars_nocovidlifeexp)
colnames(health_vars_2015) <- "health_vars"
health_vars_2015$year <- "2015" 
health_vars_2016 <- as.data.frame(health_vars_nocovidlifeexp)
colnames(health_vars_2016) <- "health_vars"
health_vars_2016$year <- "2016" 
health_vars_2017 <- as.data.frame(health_vars_nocovidlifeexp)
colnames(health_vars_2017) <- "health_vars"
health_vars_2017$year <- "2017" 
health_vars_2018 <- as.data.frame(health_vars_nocovidlifeexp)
colnames(health_vars_2018) <- "health_vars"
health_vars_2018$year <- "2018" 
health_vars_2019 <- as.data.frame(health_vars_nocovid)
colnames(health_vars_2019) <- "health_vars"
health_vars_2019$year <- "2019" 
health_vars_2020 <- as.data.frame(health_vars_nocovid)
colnames(health_vars_2020) <- "health_vars"
health_vars_2020$year <- "2020" 
health_vars_2021 <- as.data.frame(health_vars_nocovid)
colnames(health_vars_2021) <- "health_vars"
health_vars_2021$year <- "2021" 
health_vars_2022 <- as.data.frame(health_vars)
health_vars_2022$year <- "2022"
health_var_yrs <- rbind(health_vars_2011, health_vars_2012, health_vars_2013, health_vars_2014, health_vars_2015,
                        health_vars_2016, health_vars_2017, health_vars_2018, health_vars_2019, health_vars_2020, 
                        health_vars_2021, health_vars_2022)


######################################   UI   ###################################### 

mod_healthdown_ui <- function(id) {
  ns <- NS(id)
  tagList(useShinyjs(),
          fluidPage(
                    collapsible = TRUE,
                    "",
                    ## all of the filtering dropdowns are in the sidebar panel 
                    sidebarPanel(width = 3,
                                 h3("Select Data"),
                                 #h4("SVI Group"),
                                 fluidRow(column(10,
                                                 div(class = "var-dropdown",
                                                     #selectInput("SVI_Group", "SVI Group",choices=c("low","mid-low","mid-high","high")),
                                                     pickerInput(
                                                       inputId = ns("SVI_Group"),
                                                       label = "SVI Group",
                                                       choices = c("low","mid-low","mid-high","high"),
                                                       selected = all_SVI[1],
                                                       multiple = TRUE,
                                                       #options = pickerOptions(
                                                       #noneSelectedText = NULL
                                                       #width = 'auto'
                                                       #)
                                     ))),
                                            column(2, 
                                                   dropMenu(
                                              circleButton(label = "What is SVI?","What is SVI?", inputId='sviinfo',icon = icon('info'), size="xs"),
                                              h4(strong('SVI')),
                                              h5('The Social Vulnerability Index (SVI) is a summary measure that ranks counties on 16 social factors 
                                                 to indicate community wellbeing and resilience.'),
                                              placement = "bottom",
                                              arrow = TRUE,
                                              style='text-align:center'))),
                                 
                                 #h4("Population Size"),
                                 div(class = "var-dropdown",
                                     pickerInput(
                                       inputId = ns("Classification"),
                                       label = "Population Size",
                                       choices = all_pop,
                                       selected = all_pop[1],
                                       multiple = TRUE
                                     )),
                                 #h4("Year"),
                                 h4("Map View Data"),
                                 div(class = "var-dropdown",
                                     #selectInput("year", "Year", choices=all_years, selected = max(all_years))),
                                     pickerInput(
                                       inputId = ns("year"),
                                       label = "Year",
                                       choices = sort(unique(health_var_yrs$year)),
                                       selected = max(unique(health_var_yrs$year))
                                     )),
                                 div(class = "var-dropdown",
                                     # selectInput("prim_var", "Primary Health Outcome",
                                     #             choices = sort(unique(health_var_yrs$health_vars)), 
                                     #             selected = sort(unique(health_var_yrs$health_vars))[1])),
                                     pickerInput(
                                       inputId = ns("prim_var"),
                                       label = "Primary Health Outcome",
                                       choices = sort(unique(health_var_yrs$health_vars)),
                                       selected = sort(unique(health_var_yrs$health_vars))[1]
                                     )),
                                 div(
                                   class = "var-dropdown",
                                   pickerInput(
                                     inputId = ns("sec_var"),
                                     label = "Secondary Health Outcome",
                                     choices = sort(unique(health_var_yrs$health_vars)),
                                     selected = sort(unique(health_var_yrs$health_vars))[2]
                                   ))
                                 # for when data table was in the side panel with filters - now in main panel 
                                 # fluidRow(
                                 #   box(
                                 #     width = 12,
                                 #     DT::dataTableOutput(ns("mytable"), height = "50vh")
                                 #   )),
                                 
                    ),
                    mainPanel(width = 9,
                              tabsetPanel(
                                ## County Comparison tab with full table only 
                                tabPanel("County Comparison Data", 
                                         fluidRow(column(width = 12, h3("County Comparison Tool - Data View",style='text-align:center'))),
                                         fluidRow(column(width = 12, "Use the left panel to filter the data. Click on a row in the table to see the county time trend in the line plot below. 
                                                         Please note that data are not currently available for every county in every year, and estimates may change as we process more data.",
                                                         style='font-family:Avenir, Helvetica;font-size:30;text-align:center')),
                                         fluidRow(column(width = 12, wellPanel(fluidRow(
                                                                                 box(
                                                                                   width = 12, 
                                                                                   DT::dataTableOutput(ns("fulltable"), 
                                                                                                       height = "45vh"))),
                                                                               ## line plot 
                                                                               fluidRow(
                                                                                 box(
                                                                                 width = 12,
                                                                                 closable = FALSE,
                                                                                 collapsible = FALSE,
                                                                                 echarts4rOutput(ns("lineFull"), height = "35vh")
                                                                               )) 
                                         )))
                                ), # close county comparison tab panel
                                
                                ## Map view tab 
                                tabPanel("Map View",
                                         fluidRow(column(width = 12, h3("County Comparison Tool - Map View",style='text-align:center'))),
                                         fluidRow(column(width = 12, "Use the left panel to filter data, and click on the map to switch between locations and trend comparisons.
                                                         Please note that data are not currently available for every county in every year, and estimates may change as we process more data.",
                                                         style='font-family:Avenir, Helvetica;font-size:30;text-align:center')),
                                         fluidRow(column(width = 12, wellPanel(fluidRow(
                                                                                 ## data table associated with map 
                                                                                 box(
                                                                                   width = 4,
                                                                                   DT::dataTableOutput(ns("mytable"), height = "55vh")
                                                                                 ),
                                                                                 column(
                                                                                   width = 8,
                                                                                   ## map 
                                                                                   box(
                                                                                     width = 12,
                                                                                     closable = FALSE,
                                                                                     collapsible = FALSE,
                                                                                     actionButton(ns("drill_down"), "Drill Down", icon = icon("arrow-down"), class = "drill-button healthdown-button"),
                                                                                     actionButton(ns("drill_up"), "Drill Up", icon = icon("arrow-up"), class = "drill-button healthdown-button"),
                                                                                     leafletOutput(ns("leafdown"), height = "35vh")
                                                                                   ),
                                                                                   ## line plot 
                                                                                   box(
                                                                                     width = 12,
                                                                                     closable = FALSE,
                                                                                     collapsible = FALSE,
                                                                                     echarts4rOutput(ns("line"), height = "30vh")
                                                                                   )
                                                                                 )
                                                                               )
                                         )))
                                         ), # close map view tab panel 
                                
                              ))))
}


###################################### SERVER ###################################### 

mod_healthdown <- function(input, output, session) {
  
  # Work in progress - trying to limit health vars to only those available in the selected year
  updatePickerInput(session, "year", choices = sort(unique(health_var_yrs$year)))
  observeEvent(input$year, {
    health_vars_in_year <- health_var_yrs %>%
      filter(year==input$year) %>% pull(health_vars)
    print(health_vars_in_year)
    updatePickerInput(session, "prim_var", choices = health_vars_in_year) # this line isn't working for some reason
  })
  
  
  # make leafdown map object 
  my_leafdown <- Leafdown$new(spdfs_list, "leafdown", input)
  
  rv <- reactiveValues()
  rv$update_leafdown <- 0
  
  # drill up/down buttons for map 
  observeEvent(input$drill_down, {
    my_leafdown$drill_down()
    rv$update_leafdown <- rv$update_leafdown + 1
  })
  observeEvent(input$drill_up, {
    my_leafdown$drill_up()
    rv$update_leafdown <- rv$update_leafdown + 1
  })
  
  # reactive data for map 
  data <- reactive({
    req(rv$update_leafdown)
    data <- my_leafdown$curr_data
    #print(head(data))
    
    # the commented out lines here will filter the map data to only display all applied filters
    # they were removed because there was too much grey space when incorporated - only used in comparison table now
    if (my_leafdown$curr_map_level == 2) {
      data$ST <- substr(data$HASC_2, 4, 5)
      us_health_counties_year <- subset(us_health_counties1, year == input$year)
      # us_health_counties_year_SVI <- subset(us_health_counties_year, SVI_Group == input$SVI_Group)
      # us_health_counties_year_SVI_pop <- subset(us_health_counties_year_SVI, Classification == input$Classification)
      # #there are counties with the same name in different states so we have to join on both
      data <- overwrite_join(data, us_health_counties_year, by = c("NAME_2", "ST"))
      # data <- data %>% arrange(FIPS, year)
    } else {
      data$ST <- substr(data$HASC_1, 4, 5)
      us_health_states_year <- subset(us_health_states, year == input$year)
      # us_health_states_year_SVI <- subset(us_health_states_year, SVI_Group == input$SVI_Group)
      # data <- overwrite_join(data, us_health_states_year_SVI, by = "ST")
      data <- overwrite_join(data, us_health_states_year, by = "ST")
      # data <- data %>% arrange(FIPS, year)
    }
    
    my_leafdown$add_data(data)
    #print(head(data)) #can uncomment this to check new data
    data
  })
  
  # draw map with data 
  output$leafdown <- renderLeaflet({
    req(spdfs_list)
    req(data)
    
    data <- data()
    data$y <- data[, input$prim_var]
    fillcolor <- leaflet::colorNumeric("Blues", data$y)
    legend_title <- input$prim_var
    
    labels <- create_labels(data, my_leafdown$curr_map_level, input$prim_var, input$sec_var)
    my_leafdown$draw_leafdown(
      fillColor = ~ fillcolor(data$y),
      weight = 3,
      fillOpacity = 1,
      color = "white",
      label = labels
    ) %>%
      # set the view to be center on the US
      setView(-95, 39, 3) %>%
      addLegend(
        pal = fillcolor,
        values = ~ data$y,
        title = legend_title,
        opacity = 1
      )
  })
  
  # map view tab table 
  output$mytable <- DT::renderDataTable({
    all_data <- data()
    sel_data <- my_leafdown$curr_sel_data()
    map_level <- my_leafdown$curr_map_level
    create_mytable(all_data, sel_data, map_level, input$prim_var)
  })
  
  # (Un)select shapes in map when click on table (map view) 
  observeEvent(input$mytable_row_last_clicked, {
    sel_row <- input$mytable_row_last_clicked
    sel_shape_id <- my_leafdown$curr_poly_ids[sel_row]
    my_leafdown$toggle_shape_select(sel_shape_id)
  })
  
  
  # reactive data for full county comparison table - this one filters by all selections 
  data_full <- reactive({
    dataFull <- us_health_counties1
    dataFull_year <- subset(dataFull, year == input$year)
    dataFull_year_SVI <- subset(dataFull_year, SVI_Group == input$SVI_Group)
    dataFull_year_SVI_pop <- subset(dataFull_year_SVI, Classification == input$Classification)
    # there are counties with the same name in different states so we have to join on both
    dataFull <- overwrite_join(dataFull, dataFull_year_SVI_pop, by = c("NAME_2", "ST"))
    dataFull = dataFull[, c('NAME_2', 'ST', input$prim_var)]
    dataFull <- dplyr::rename(dataFull, County = NAME_2)
    # remove any duplicate rows and rows with NA in the measure of interest
    dataFull <- dataFull %>% distinct()
    dataFull <- dataFull[complete.cases(dataFull[ , input$prim_var]),]
  })
  
  # full table for comparison tab 
  output$fulltable <- DT::renderDataTable({
    DT::datatable(data_full(), rownames = FALSE)
  })
  
  # get rows for comparison tab line plot 
  row_data <- eventReactive(input$fulltable_rows_selected, {
    sel_rows <- input$fulltable_rows_selected
    rowData <- data_full()[sel_rows,]
    rowData <- dplyr::rename(rowData, NAME_2 = County)
    rownames(rowData) <- NULL
    #print(rowData) #checking
    rowData
  })
  
  # line plot - map view tab 
  output$line <- renderEcharts4r({
    create_line_graph(us_health_all, my_leafdown$curr_sel_data(), input$prim_var) #, input$sec_var)
  })
  
  # line plot - full county comparison tab
  output$lineFull <- renderEcharts4r({
    create_line_graph_full(us_health_all, row_data(), input$prim_var) #, input$sec_var)
  })
  
  # # scatter plot - not used 
  # output$scatter <- renderEcharts4r({
  #   create_scatter_plot(my_leafdown$curr_sel_data(), input$prim_var, input$sec_var)
  # })
  # 
  # # bar plot - not used
  # output$bar <- renderEcharts4r({
  #   create_bar_chart(my_leafdown$curr_sel_data(), input$prim_var)
  # })
  
}

shinyApp(mod_healthdown_ui, mod_healthdown)
