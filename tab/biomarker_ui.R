biomarker_ui <- fluidPage(
  fluidRow(
    # compare_result_dir setting
    directoryInput(
      "biomarker_compare_result_dir",
      label = "compare result folder:",
      value = paste0(dirname("~"), "/", "Desktop")
    ),
  ),
  fluidRow(column(
    6,
    checkboxInput(
      "biomarker_calculate_percentage",
      "whether to calculate the percentage",
      value = FALSE
    )
  )),
  fluidRow(
    column(
      4,
      numericInput(
        "biomarker_font_size",
        label = "font size:",
        value = 16,
        min = 4,
        max = 30
      )
    ),
    column(
      4,
      numericInput(
        "biomarker_width",
        label = "diagram width(cm):",
        value = 15,
        min = 4,
        max = 30
      )
    ),
    column(
      4,
      numericInput(
        "biomarker_height",
        label = "diagram height(cm):",
        value = 12,
        min = 4,
        max = 30
      )
    )
  ),
  actionButton("run_biomarker", "Run", icon = icon("gear", class = "fa-spin")),
  actionButton(
    "clear_biomarker_notifications",
    "Clear biomarker notifications"
  )
)