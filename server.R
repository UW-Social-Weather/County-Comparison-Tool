# Name:Harsha
# Date:10/10/2022
# Purpose: This is the server portion of the RShiny code

server <- function(input, output) {
  callModule(mod_healthdown, "Comparison Tool")
}
