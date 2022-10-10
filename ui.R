#This is the ui portion of the rshiny code is called out

ui <- htmlTemplate(
  filename = "www/index.html",
  what_if_ui = mod_healthdown_ui("Comparison Tool")
)
