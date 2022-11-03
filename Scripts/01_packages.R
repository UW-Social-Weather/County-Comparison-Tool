#Install Packages

#Function to check if you have a package installed and installs if you don't
inst.pkg <- function(package_name) {
  if (package_name %in% rownames(installed.packages()) == FALSE) # check to see if the package is already installed
  {install.packages(package_name)} # install package if missing
  if (package_name %in% rownames(.packages()) == FALSE) # check to see if the package is loaded
  {library(package_name, character.only = TRUE)} # load package into library
}


inst.pkg("shiny")
inst.pkg("shinythemes")
inst.pkg("DT")
inst.pkg("dplyr")
inst.pkg("formattable")
inst.pkg("tidyr")
inst.pkg("leaflet")
inst.pkg("plotly")
inst.pkg("readxl")
inst.pkg("shinycssloaders")
inst.pkg("shinyWidgets")
inst.pkg("shinyhelper")
inst.pkg("rintrojs")
inst.pkg("shinyBS")
inst.pkg("here")