# Name:Harsha
# Date:10/10/2022
# Purpose: This is the ui portion of the rshiny code is called out

ui <- htmlTemplate(
  filename = "www/index.html",
  what_if_ui = mod_healthdown_ui("Comparison Tool")
)
