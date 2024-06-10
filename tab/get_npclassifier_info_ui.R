get_npclassifier_info_ui <- fluidPage(fluidRow(
  # compare_result_dir setting
  directoryInput(
    "npclassifier_compare_result_dir",
    label = "compare result folder:",
    value = paste0(dirname("~"), "/", "Desktop")
  ),
  actionButton(
    "run_get_npclassifier_info",
    "Run",
    icon = icon("gear", class = "fa-spin")
  ),
  actionButton(
    "clear_npclassifier_notifications",
    "Clear npclassifier notifications"
  )
))