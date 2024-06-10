classification_summaries_ui <- fluidPage(
  fluidRow(
    # compare_result_dir setting
    directoryInput(
      "classification_summaries_compare_result_dir",
      label = "compare result folder:",
      value = paste0(dirname("~"), "/", "Desktop")
    ),
  ),
  fluidRow(column(
    6,
    selectInput(
      "classification_summaries_calculate_type",
      "calculate type:",
      c("standard deviation", "standard error of mean"),
      multiple = FALSE
    )
  )),
  actionButton(
    "run_classification_summaries",
    "Run",
    icon = icon("gear", class = "fa-spin")
  ),
  actionButton(
    "clear_classification_summaries_notifications",
    "Clear classification summaries notifications"
  )
)