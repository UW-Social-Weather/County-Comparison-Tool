mod_healthdown_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # we need shinyjs for the leafdown map
    useShinyjs(),
    fluidRow(
      class = "top-row",
      box(
        width = 4,
        class = "card-hexagon",
        # img(src = "assets/images/hex-healthdown.png"),
        div(" ", class = "card-hexagon-title")
      ),
      box(
        width = 8,
        height = "100px",
        fluidRow(
          div(
            class = "spread3evenly",
            div(
              class = "var-dropdown",
              pickerInput(
                inputId = ns("year"),
                label = "Select the Year",
                choices = all_years,
                selected = max(all_years)
              )
            ),
            div(
              class = "var-dropdown",
              pickerInput(
                inputId = ns("SVI_Group"),
                label = "Select the SVI group",
                choices = all_SVI,
                selected = all_SVI[2]
                )
            ),
            div(
              class = "var-dropdown",
              pickerInput(
                inputId = ns("prim_var"),
                label = "Social Vulnerability Index",
                choices = all_vars[1], all_vars[2], all_vars[6], all_vars[7], all_vars[13],
                selected = all_vars[1]
              )
            ),
            div(
              class = "var-dropdown",
              pickerInput(
                inputId = ns("sec_var"),
                label = "Outcome of Interest",
                choices = all_vars[1], all_vars[2], all_vars[6], all_vars[7], all_vars[13],
                selected = all_vars[2]
              )
            )
          )
        )
      ),
      box(
        width = 2,
        class = "card-hexagon",
        div(style = "height: 100px; width: 100px", 
           # img(src = "assets/images/hex-leafdown.png"),
            div(
              class = "card-hexagon-title",
            )
        )
      )
    ),
    fluidRow(
      box(
        width = 2,
        DT::dataTableOutput(ns("mytable"), height = "50vh")
      ),
      column(
        width = 10,
        fluidRow(
          box(
            width = 6,
            closable = FALSE,
            collapsible = FALSE,
            actionButton(ns("drill_down"), "Drill Down", icon = icon("arrow-down"), class = "drill-button healthdown-button"),
            actionButton(ns("drill_up"), "Drill Up", icon = icon("arrow-up"), class = "drill-button healthdown-button"),
            leafletOutput(ns(" "), height = "30vh")
          ),
          box(
            width = 6,
            closable = FALSE,
            collapsible = FALSE,
            echarts4rOutput(ns("bar"), height = "30vh")
          )
        ),
        fluidRow(
          height = "90%",
          box(
            width = 6,
            closable = FALSE,
            collapsible = FALSE,
            echarts4rOutput(ns("line"), height = "30vh")
          ),
          box(
            width = 6,
            closable = FALSE,
            collapsible = FALSE,
            echarts4rOutput(ns("scatter"), height = "30vh")
          )
        )
      )
    )
  )
}

