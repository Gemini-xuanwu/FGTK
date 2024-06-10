TIC_2D_server <- function(input, output, session) {
  # compare_result_dir setting
  observeEvent(ignoreNULL = TRUE,
               eventExpr = {
                 input$"2D_TIC_curve_dir_path"
               },
               handlerExpr = {
                 if (input$"2D_TIC_curve_dir_path" > 0) {
                   # condition prevents handler execution on initial app launch
                   
                   # launch the directory selection dialog with initial path read from the widget
                   path <- choose.dir(default = readDirectoryInput(session, "2D_TIC_curve_dir_path"))
                   
                   # update the widget value
                   updateDirectoryInput(session, "2D_TIC_curve_dir_path", value = path)
                 }
               })
  
  # create a reactiveVal to store result
  TIC_data <- reactiveVal(NULL)
  
  # run plot_2D_TIC function
  observeEvent(input$plot_2D_TIC, {
    TIC_data(NULL)
    
    TIC_result<-plot_2D_TIC(
      curve_dir_path = readDirectoryInput(session, "2D_TIC_curve_dir_path"),
      calculate_percentage = input$"2D_TIC_calculate_percentage",
      line_width = input$"2D_TIC_line_width",
      font_size = input$"2D_TIC_font_size",
      x_offset = input$"2D_TIC_x_offset",
      y_offset = input$"2D_TIC_y_offset",
      x_min = input$"2D_TIC_x_min",
      x_max = input$"2D_TIC_x_max",
      y_min = input$"2D_TIC_y_min",
      y_max = input$"2D_TIC_y_max"
    )
    
    TIC_data(TIC_result)
    
    output$"2D_TIC_plot" <- renderPlot({
      TIC_data()$plot
    })
    
    output$"2D_TIC_data_table" <- DT::renderDT({
      DT::datatable(TIC_data()$df)
    })
  })
  
  # Save 2D_TIC Diagram to compare result folder
  observeEvent(input$save_2D_TIC, {
    if (input$"2D_TIC_filetype" == "pdf") {
      ggsave(
        paste0(
          readDirectoryInput(session, "2D_TIC_curve_dir_path"),
          "/2D-",
          ifelse(input$"2D_TIC_calculate_percentage"==TRUE,"relative","absolute"),
          "-TIC.pdf"
        ),
        plot = TIC_data()$plot,
        width = input$"2D_TIC_width",
        height = input$"2D_TIC_height"
      )
    }
    else if (input$"2D_TIC_filetype" == "png") {
      ggsave(
        paste0(
          readDirectoryInput(session, "2D_TIC_curve_dir_path"),
          "/2D-",
          ifelse(input$"2D_TIC_calculate_percentage"==TRUE,"relative","absolute"),
          "-TIC.png"
        ),
        plot = TIC_data()$plot,
        dpi = 600,
        width = input$"2D_TIC_width",
        height = input$"2D_TIC_height"
      )
    }
    else{
      ggsave(
        paste0(
          readDirectoryInput(session, "2D_TIC_curve_dir_path"),
          "/2D-",
          ifelse(input$"2D_TIC_calculate_percentage"==TRUE,"relative","absolute"),
          "-TIC.pdf"
        ),
        plot = TIC_data()$plot,
        width = input$"2D_TIC_width",
        height = input$"2D_TIC_height"
      )
    }
  })
  
  observeEvent(input$save_2D_TIC_df, {
    openxlsx::write.xlsx(
      TIC_data()$df,
      paste0(
        readDirectoryInput(session, "2D_TIC_curve_dir_path"),
        "/",
        ifelse(input$"2D_TIC_calculate_percentage"==TRUE,"relative","absolute"),
        "-TIC.xlsx"
      )
    )
  })
}