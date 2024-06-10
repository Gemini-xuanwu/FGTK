compare_gcms_results_ui <- fluidPage(
  fluidRow(
    # input_dir_path setting
    directoryInput(
      "input_dir_path",
      label = "input folder path:",
      value = paste0(dirname("~"), "/", "Desktop")
    ),
    # output_dir_path setting
    directoryInput(
      "output_dir_path",
      label = "output folder path:",
      value = paste0(dirname("~"), "/", "Desktop")
    )
  ),
  fluidRow(column(
    6,
    numericInput(
      "min_quality",
      label = "minimum quality (Recommended setting is 40-80):",
      value = 60,
      min = 40,
      max = 80
    )
  ), column(
    6,
    numericInput(
      "max_rt",
      label = "max Retention Time (Recommended setting is 20-30min):",
      value = 25,
      min = 10,
      max = 60
    )
  )),
  fluidRow(column(
    6,
    numericInput(
      "within_thr",
      label = "data-within Threshold for RT difference (Recommended setting is 0.001-0.01min):",
      value = 0.005,
      min = 0,
      max = 0.010
    )
  ), column(
    6,
    numericInput(
      "between_thr",
      label = "data-between Threshold for RT difference (Recommended setting is 0.005-0.02min):",
      value = 0.01,
      min = 0.005,
      max = 0.020
    )
  )),
  fluidRow(column(
    6,
    numericInput(
      "min_comparative_quality",
      label = "similarity between the two compound's results (Recommended setting is 0.2-0.7):",
      value = 0.5,
      min = 0.2,
      max = 0.7
    )
  )),
  actionButton(
    "run_compare_gcms_results",
    "Run",
    icon = icon("gear", class = "fa-spin")
  ),
  actionButton("clear_compare_notifications", "Clear compare notifications")
)