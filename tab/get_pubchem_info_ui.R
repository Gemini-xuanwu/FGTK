get_pubchem_info_ui <- fluidPage(fluidRow(
  # compare_result_dir setting
  directoryInput(
    "pubchem_compare_result_dir",
    label = "compare result folder:",
    value = paste0(dirname("~"), "/", "Desktop")
  ),
  actionButton("run_get_pubchem_info", "Run", icon = icon("gear", class = "fa-spin")),
  actionButton("clear_pubchem_notifications", "Clear pubchem notifications")
))