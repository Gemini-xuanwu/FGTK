upset_ui <- fluidPage(
  box(
    title = "Folder path & settings",
    width = NULL,
    status = "info",
    fluidRow(column(
      12,
      # compare_result_dir setting
      directoryInput(
        "upset_compare_result_dir",
        label = "compare result folder:",
        value = paste0(dirname("~"), "/", "Desktop")
      )
    )),
    fluidRow(column(
      6,
      numericInput(
        "max_intersections",
        label = "max intersections:",
        value = 20,
        min = 5,
        max = 50
      )
    ), column(
      6,
      selectInput(
        "set_size_position",
        "set size position:",
        c("right", "left"),
        multiple = FALSE
      )
    )),
    fluidRow(column(
      6,
      numericInput(
        "intersection_matrix_height_ratio",
        label = "intersection matrix height ratio:",
        value = 0.2,
        min = 0.1,
        max = 0.8
      )
    ), column(
      6,
      numericInput(
        "set_size_width_ratio",
        label = "set size width ratio:",
        value = 0.3,
        min = 0.1,
        max = 0.8
      )
    )),
    fluidRow(column(
      6,
      selectInput(
        "sort_intersections_by",
        "sort method:",
        c("degree", "ratio"),
        multiple = FALSE
      )
    ), column(
      6,
      selectInput(
        "sort_intersections",
        "sort:",
        c("ascending", "descending"),
        multiple = FALSE
      )
    )),
    fluidRow(column(
      6,
      numericInput(
        "upset_font_size",
        label = "font size:",
        value = 14,
        min = 6,
        max = 24
      )
    ), column(
      6,
      numericInput(
        "upset_annotate_font_size",
        label = "annotate font size:",
        value = 4,
        min = 1,
        max = 10
      )
    )),
    actionButton("plot_upset", "Plot", icon = icon("gear", class = "fa-spin"))
  ),
  box(
    title = "Upset Diagram",
    width = NULL,
    status = "success",
    plotOutput("upset_plot", width = "100%", height = "450px"),
    box(
      title = "Save settings",
      width = NULL,
      status = "warning",
      fluidRow(column(
        6,
        numericInput(
          "upset_width",
          label = "upset diagram width(cm):",
          value = 7,
          min = 4,
          max = 20
        )
      ), column(
        6,
        numericInput(
          "upset_height",
          label = "upset diagram height(cm):",
          value = 7,
          min = 4,
          max = 20
        )
      )),
      radioButtons(
        inputId = "upset_filetype",
        label = "Choose file type to save:",
        inline = TRUE,
        choices = list("pdf", "png")
      ),
      actionButton("save_upset", "Save to compare result folder", icon = icon("download"))
    )
  )
)