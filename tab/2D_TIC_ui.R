TIC_2D_ui <- fluidPage(
  box(
    title = "Folder path & settings",
    width = NULL,
    status = "info",
    fluidRow(column(
      8,
      # curve_dir_path setting
      directoryInput(
        "2D_TIC_curve_dir_path",
        label = "curve folder:",
        value = paste0(dirname("~"), "/", "Desktop")
      )
    ), column(
      4,
      checkboxInput(
        "2D_TIC_calculate_percentage",
        "whether to calculate the percentage",
        value = FALSE
      )
    )),
    fluidRow(
      column(
        3,
        numericInput(
          "2D_TIC_line_width",
          label = "line width:",
          value = 0.5,
          min = 0.1,
          max = 4
        )
      ),
      column(
        3,
        numericInput(
          "2D_TIC_font_size",
          label = "font size:",
          value = 14,
          min = 1,
          max = 30
        )
      ),
      column(
        3,
        numericInput(
          "2D_TIC_x_offset",
          label = "time offset:",
          value = 0,
          min = 0,
          max = 1
        )
      ),
      column(
        3,
        numericInput(
          "2D_TIC_y_offset",
          label = "abundance offset:",
          value = 0,
          min = 0,
          max = 100
        )
      )
    ),
    fluidRow(
      column(
        3,
        numericInput(
          "2D_TIC_x_min",
          label = "time min:",
          value = NULL,
          min = 0,
          max = 10
        )
      ),
      column(
        3,
        numericInput(
          "2D_TIC_x_max",
          label = "time max:",
          value = NULL,
          min = 10,
          max = 60
        )
      ),
      column(
        3,
        numericInput(
          "2D_TIC_y_min",
          label = "abundance min:",
          value = NULL,
          min = 0,
          max = 10
        )
      ),
      column(
        3,
        numericInput(
          "2D_TIC_y_max",
          label = "abundance max:",
          value = NULL,
          min = 100,
          max = 100000
        )
      )
    ),
    actionButton("plot_2D_TIC", "Plot", icon = icon("gear", class = "fa-spin"))
  ),
  box(
    title = "Result",
    width = NULL,
    status = "success",
    tabBox(
      height = "100%",
      width = "100%",
      tabPanel(
        "2D TIC diagram",
        plotOutput("2D_TIC_plot", width = "100%", height = "450px"),
        box(
          title = "Save settings",
          width = NULL,
          status = "warning",
          fluidRow(column(
            6,
            numericInput(
              "2D_TIC_width",
              label = "2D TIC diagram width(cm):",
              value = 7,
              min = 4,
              max = 20
            )
          ), column(
            6,
            numericInput(
              "2D_TIC_height",
              label = "2D TIC diagram height(cm):",
              value = 5,
              min = 3,
              max = 20
            )
          )),
          radioButtons(
            inputId = "2D_TIC_filetype",
            label = "Choose file type to save:",
            inline = TRUE,
            choices = list("pdf", "png")
          ),
          actionButton("save_2D_TIC", "Save to curve folder", icon = icon("download"))
        )
      ),
      tabPanel(
        "Data table",
        div(
          style = 'overflow-x: scroll',
          DT::dataTableOutput("2D_TIC_data_table", width = "100%")
        ),
        br(),
        actionButton(
          "save_2D_TIC_df",
          "Save to curve folder",
          icon = icon("download")
        )
      )
    )
  )
)