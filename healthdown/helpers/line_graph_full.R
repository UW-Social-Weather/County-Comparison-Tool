create_line_graph_full <- function(dataFull, row_data, prim_var) {
  # filter the data to get only the currently selected data
  df <- subset(us_health_all, NAME_2 %in% row_data$NAME_2 & ST %in% row_data$ST)
  
  validate(
    need(nrow(df) > 0, "Select regions in the table above to add their values to this graph.")
  )
  
  df$year <- as.Date(ISOdate(df$year, 1, 1))
  df$name <- ifelse(is.na(df$NAME_2), as.character(df$ST), as.character(df$NAME_2))
  
  df %>%
    group_by(name) %>%
    e_charts(year) %>%
    e_line_(prim_var) %>%
    e_axis_labels(y = prim_var) %>%
    e_y_axis(
      nameLocation = "center", nameGap = 50,
      min = floor(0.9 * min(df[[prim_var]]))
    ) %>%
    e_tooltip(trigger = "axis")
}
