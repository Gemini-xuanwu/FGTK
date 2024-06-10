stat_ui <- fluidPage(
  fluidRow(
    # compare_result_dir setting
    directoryInput(
      "stat_compare_result_dir",
      label = "compare result folder:",
      value = paste0(dirname("~"), "/", "Desktop")
    ),
  ),
  fluidRow(column(
    6,
    checkboxInput(
      "stat_calculate_percentage",
      "whether to calculate the percentage",
      value = FALSE
    )
  ), column(
    6,
    selectInput(
      "stat_calculate_type",
      "calculate type:",
      c("standard deviation", "standard error of mean"),
      multiple = FALSE
    )
  )),
  actionButton("run_stat", "Run", icon = icon("gear", class = "fa-spin")),
  actionButton("clear_stat_notifications", "Clear stat notifications")
)