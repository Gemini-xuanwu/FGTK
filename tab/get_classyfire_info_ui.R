get_classyfire_info_ui <- fluidPage(fluidRow(
  # compare_result_dir setting
  directoryInput(
    "classyfire_compare_result_dir",
    label = "compare result folder:",
    value = paste0(dirname("~"), "/", "Desktop")
  ),
  actionButton(
    "run_get_classyfire_info",
    "Run",
    icon = icon("gear", class = "fa-spin")
  ),
  actionButton(
    "clear_classyfire_notifications",
    "Clear classyfire notifications"
  )
))