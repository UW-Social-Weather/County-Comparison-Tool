create_line_graph_full <- function(us_health_all, row_data, prim_var) {
  # filter the data to get only the currently selected data
  #df_full <- NULL
  df_full <- subset(us_health_all, NAME_2 %in% row_data$NAME_2 & ST %in% row_data$ST)
  
  validate(
    need(nrow(df_full) > 0, "Select regions in the table above to add their values to this graph.")
  )
  
  df_full$year <- as.Date(ISOdate(df_full$year, 1, 1))
  df_full$name <- ifelse(is.na(df_full$NAME_2), as.character(df_full$ST), as.character(df_full$NAME_2))
  
  df_full %>%
    group_by(name) %>%
    e_charts(year) %>%
    e_line_(prim_var) %>%
    e_axis_labels(y = prim_var) %>%
    e_y_axis(
      nameLocation = "center", nameGap = 50,
      min = floor(0.9 * min(df_full[[prim_var]]))
    ) %>%
    e_tooltip(trigger = "axis")
}
