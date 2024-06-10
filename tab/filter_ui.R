filter_ui <- fluidPage(
  fluidRow(
    # compare_result_dir setting
    directoryInput(
      "filter_compare_result_dir",
      label = "compare result folder:",
      value = paste0(dirname("~"), "/", "Desktop")
    ),
  ),
  fluidRow(column(
    6,
    checkboxInput(
      "cut_ck",
      "whether or not to remove impurities shown in the control group",
      value = FALSE
    )
  )),
  fluidRow(column(
    6,
    numericInput(
      "frequency_within_group_thr",
      label = "frequency of compounds in each group (Recommended setting is 0.2-0.6):",
      value = 0.4,
      min = 0.2,
      max = 0.6
    )
  ), column(
    6,
    numericInput(
      "frequency_within_overall_thr",
      label = "frequency of compounds in overall (Recommended setting is 0.1-0.5):",
      value = 0.2,
      min = 0.1,
      max = 0.5
    )
  )),
  actionButton("run_filter", "Run", icon = icon("gear", class = "fa-spin")),
  actionButton("clear_filter_notifications", "Clear filter notifications")
)