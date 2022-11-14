mod_healthdown_ui <- function(id) {
  ns <- NS(id)
          tagList(useShinyjs(),
                  tags$head(tags$style(type = 'text/css','.navbar-brand{display:none;}'),
                            tags$style( type = 'text/css',
                                        ".selectize-input { word-wrap : break-word;}
                            .selectize-input { word-break: break-word;}
                            .selectize-dropdown {word-wrap : break-word;} "
                            )),
                  fluidPage(theme = shinytheme("flatly"),
                            collapsible = TRUE,
                            "",
                            sidebarPanel(width = 2,
                              h3("Select SVI Group"),
                              div(class = "var-dropdown",
                                  #selectInput("SVI_Group", "SVI Group",choices=c("low","mid-low","mid-high","high")),
                                  pickerInput(
                                    inputId = ns("SVI_Group"),
                                    label = "SVI Group",
                                    choices = c("low","mid-low","mid-high","high"),
                                    selected = all_SVI[1]
                                  )),

                              h3("Select Map Data"),
                              div(class = "var-dropdown",
                                  #selectInput("year", "Year", choices=all_years, selected = max(all_years))),
                                  pickerInput(
                                    inputId = ns("year"),
                                    label = "Year",
                                    choices = all_years,
                                    selected = max(all_years)
                                  )),
                              div(class = "var-dropdown",
                                  #selectInput("prim_var", "Primary Health Outcome",choices=all_vars, selected = all_vars[1]))
                                  pickerInput(
                                    inputId = ns("prim_var"),
                                    label = "Primary Health Outcome",
                                    choices = all_vars,
                                    selected = all_vars[1]
                                  )),
                              div(
                                class = "var-dropdown",
                                pickerInput(
                                  inputId = ns("sec_var"),
                                  label = "Select the Secondary Variable",
                                  choices = all_vars,
                                  selected = all_vars[2]
                                ))
                                # fluidRow(
                                #   box(
                                #     width = 12,
                                #     DT::dataTableOutput(ns("mytable"), height = "50vh")
                                #   )),

                            ),
                            mainPanel(width = 10,
                              tabsetPanel(
                                tabPanel("Map View",
                                         fluidRow(column(width = 12, h3("County Comparison Tool",style='text-align:center'))),
                                         fluidRow(column(width = 12, "Use the left panel to filter data, and click on the map to switch between locations, trend comparisons, and profile views.
                                                         Please note that data are not currently available for every county in every year, and estimates may change as we process more data.",
                                                         style='font-family:Avenir, Helvetica;font-size:30;text-align:center')),
                                         fluidRow(column(width = 12, wellPanel(tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar{
                                                                         background: #48C9B0;
                                                                         border-top: 1px solid #48C9B0 ; border-bottom: 1px solid #48C9B0}")),

                                          fluidRow(
                                               box(
                                                 width = 4,
                                                 DT::dataTableOutput(ns("mytable"), height = "60vh")
                                               ),
                                               column(
                                                 width = 9,
                                                   box(
                                                     width = 12,
                                                     closable = FALSE,
                                                     collapsible = FALSE,
                                                     actionButton(ns("drill_down"), "Drill Down", icon = icon("arrow-down"), class = "drill-button healthdown-button"),
                                                     actionButton(ns("drill_up"), "Drill Up", icon = icon("arrow-up"), class = "drill-button healthdown-button"),
                                                     leafletOutput(ns("leafdown"), height = "35vh")
                                                   ),
                                                 box(
                                                   width = 12,
                                                   closable = FALSE,
                                                   collapsible = FALSE,
                                                   echarts4rOutput(ns("line"), height = "35vh")
                                                   )
                                                 )
                                               )
                                         )))),

                                # tabPanel("Map Data", verbatimTextOutput("viewdata"),
                                #          fluidRow(column(width = 12, h3(textOutput("mapdatatitle"),style='text-align:center'))),
                                #          fluidRow(column(width = 12, "Use the left panel to filter map data.",
                                #                          style='font-family:Avenir, Helvetica;font-size:30;text-align:center')),
                                #          ),
                                                      ))))
}

# # # Harsha's OLD CODE:
# mod_healthdown_ui <- function(id) {
#   ns <- NS(id)
#   tagList(
#     # we need shinyjs for the leafdown map
#     useShinyjs(),
#     fluidRow(
#       class = "top-row",
#       box(
#         width = 2,
#         class = "card-hexagon",
#         img(src = "assets/images/hex-healthdown.png"),
#         div("Health Ranking", class = "card-hexagon-title")
#       ),
#       box(
#         width = 8,
#         height = "100px",
#         fluidRow(
#           div(
#             class = "spread3evenly",
#             div(
#               class = "var-dropdown",
#               pickerInput(
#                 inputId = ns("year"),
#                 label = "Select the Year",
#                 choices = all_years,
#                 selected = max(all_years)
#               )
#             ),
#             div(
#               class = "var-dropdown",
#               pickerInput(
#                 inputId = ns("prim_var"),
#                 label = "Select the Primary Variable",
#                 choices = all_vars,
#                 selected = all_vars[1]
#               )
#             ),
#             div(
#               class = "var-dropdown",
#               pickerInput(
#                 inputId = ns("sec_var"),
#                 label = "Select the Secondary Variable",
#                 choices = all_vars,
#                 selected = all_vars[2]
#               )
#             )
#             # div(class = "var-dropdown",
#             #     #selectInput("SVI_Group", "SVI Group",choices=c("low","mid-low","mid-high","high")),
#             #     pickerInput(
#             #       inputId = ns("SVI_Group"),
#             #       label = "SVI Group",
#             #       choices = c("low","mid-low","mid-high","high"),
#             #       selected = all_SVI[1]
#             #     ))
#           )
#         )
#       ),
#       box(
#         width = 2,
#         class = "card-hexagon",
#         div(style = "height: 100px; width: 100px",
#             img(src = "assets/images/hex-leafdown.png"),
#             div(
#               class = "card-hexagon-title",
#               tags$a(
#                 "Leafdown",
#                 tags$i(class = "fas fa-xs fa-external-link-alt"),
#                 href = "https://github.com/hoga-it/leafdown",
#                 target = "_blank",
#                 style = "color: white;"
#               )
#             )
#         )
#       )
#     ),
#     fluidRow(
#       box(
#         width = 2,
#         DT::dataTableOutput(ns("mytable"), height = "50vh")
#       ),
#       column(
#         width = 10,
#         fluidRow(
#           box(
#             width = 6,
#             closable = FALSE,
#             collapsible = FALSE,
#             actionButton(ns("drill_down"), "Drill Down", icon = icon("arrow-down"), class = "drill-button healthdown-button"),
#             actionButton(ns("drill_up"), "Drill Up", icon = icon("arrow-up"), class = "drill-button healthdown-button"),
#             leafletOutput(ns("leafdown"), height = "30vh")
#           ),
#           box(
#             width = 6,
#             closable = FALSE,
#             collapsible = FALSE,
#             echarts4rOutput(ns("bar"), height = "30vh")
#           )
#         ),
#         fluidRow(
#           height = "90%",
#           box(
#             width = 6,
#             closable = FALSE,
#             collapsible = FALSE,
#             echarts4rOutput(ns("line"), height = "30vh")
#           ),
#           box(
#             width = 6,
#             closable = FALSE,
#             collapsible = FALSE,
#             echarts4rOutput(ns("scatter"), height = "30vh")
#           )
#         )
#       )
#     )
#   )
# }
