#This is the ui portion of rshiny code

ui <- htmlTemplate(
  filename = "www/index.html",
  what_if_ui = mod_healthdown_ui("Comparison Tool")
)
