TIC_3D_ui <- fluidPage(
  h2("This function is still in beta!!!", style = "color: red;"),
  box(
    title = "Folder path & settings",
    width = NULL,
    status = "info",
    fluidRow(column(
      8,
      # curve_dir_path setting
      directoryInput(
        "3D_TIC_curve_dir_path",
        label = "curve folder:",
        value = paste0(dirname("~"), "/", "Desktop")
      )
    ), column(
      4,
      checkboxInput(
        "3D_TIC_calculate_percentage",
        "whether to calculate the percentage",
        value = FALSE
      )
    )),
    fluidRow(
      column(
        3,
        numericInput(
          "3D_TIC_line_width",
          label = "line width:",
          value = 2,
          min = 0.5,
          max = 10
        )
      ),
      column(
        3,
        numericInput(
          "3D_TIC_font_size",
          label = "font size:",
          value = 12,
          min = 6,
          max = 20
        )
      ),
      column(3, textInput("3D_TIC_x_title", "x title:", value = "")),
      column(3, textInput("3D_TIC_z_title", "z title:", value = ""))
    ),
    fluidRow(
      column(
        3,
        numericInput(
          "3D_TIC_x_min",
          label = "time min:",
          value = NULL,
          min = 0,
          max = 10
        )
      ),
      column(
        3,
        numericInput(
          "3D_TIC_x_max",
          label = "time max:",
          value = NULL,
          min = 10,
          max = 60
        )
      ),
      column(
        3,
        numericInput(
          "3D_TIC_z_min",
          label = "abundance min:",
          value = NULL,
          min = 0,
          max = 10
        )
      ),
      column(
        3,
        numericInput(
          "3D_TIC_z_max",
          label = "abundance max:",
          value = NULL,
          min = 100,
          max = 100000
        )
      )
    ),
    actionButton("plot_3D_TIC", "Plot", icon = icon("gear", class = "fa-spin"))
  ),
  box(
    title = "3D TIC Diagram",
    width = NULL,
    status = "success",
    plotly::plotlyOutput("3D_TIC_plot", width = "100%", height = "450px")
  )
)