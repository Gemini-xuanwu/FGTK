multivariate_data_analysis_ui <- fluidPage(
  fluidRow(
    # compare_result_dir setting
    directoryInput(
      "mda_compare_result_dir",
      label = "compare result folder:",
      value = paste0(dirname("~"), "/", "Desktop")
    ),
  ),
  fluidRow(column(
    6,
    checkboxInput(
      "mda_calculate_percentage",
      "whether to calculate the percentage",
      value = FALSE
    )
  )),
  fluidRow(
    column(
      4,
      numericInput(
        "mda_font_size",
        label = "font size:",
        value = 16,
        min = 4,
        max = 30
      )
    ),
    column(
      4,
      numericInput(
        "mda_width",
        label = "diagram width(cm):",
        value = 15,
        min = 4,
        max = 30
      )
    ),
    column(
      4,
      numericInput(
        "mda_height",
        label = "diagram height(cm):",
        value = 12,
        min = 4,
        max = 30
      )
    )
  ),
  actionButton(
    "run_multivariate_data_analysis",
    "Run",
    icon = icon("gear", class = "fa-spin")
  ),
  actionButton(
    "clear_mda_notifications",
    "Clear multivariate data analysis notifications"
  )
)