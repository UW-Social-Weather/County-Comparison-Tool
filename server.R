#This is the server portion of the RShiny code

server <- function(input, output) {
  callModule(mod_healthdown, "Comparison Tool")
}
