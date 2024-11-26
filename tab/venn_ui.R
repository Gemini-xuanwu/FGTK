venn_ui <- fluidPage(
  box(
    title = "Folder path & settings",
    width = NULL,
    status = "info",
    fluidRow(column(
      12,
      # compare_result_dir setting
      directoryInput(
        "venn_compare_result_dir",
        label = "compare result folder:",
        value = paste0(dirname("~"), "/", "Desktop")
      )
    )),
    fluidRow(column(
      6,
      numericInput(
        "group_label_font_size",
        label = "group label font size:",
        value = 2,
        min = 0.5,
        max = 4
      )
    ), column(
      6,
      numericInput(
        "number_label_font_size",
        label = "number label font size:",
        value = 2,
        min = 0.5,
        max = 4
      )
    )),
    fluidRow(column(
      6,
      numericInput(
        "venn_margin",
        label = "margin:",
        value = 0.05,
        min = 0,
        max = 0.4
      )
    ), column(
      6,
      numericInput(
        "label_offset",
        label = "label offset:",
        value = 0,
        min = 0,
        max = 1
      )
    )),
    actionButton("plot_venn", "Plot", icon = icon("gear", class = "fa-spin"))
  ),
  box(
    title = "Venn Diagram",
    width = NULL,
    status = "success",
    plotOutput("venn_plot", width = "100%", height = "450px"),
    box(
      title = "Save settings",
      width = NULL,
      status = "warning",
      fluidRow(column(
        6,
        numericInput(
          "venn_width",
          label = "Venn diagram width(cm):",
          value = 5,
          min = 2,
          max = 10
        )
      ), column(
        6,
        numericInput(
          "venn_height",
          label = "Venn diagram height(cm):",
          value = 5,
          min = 2,
          max = 10
        )
      )),
      radioButtons(
        inputId = "venn_filetype",
        label = "Choose file type to save:",
        inline = TRUE,
        choices = list("pdf", "png")
      ),
      actionButton("save_venn", "Save to compare result folder", icon = icon("download"))
    )
  )
)