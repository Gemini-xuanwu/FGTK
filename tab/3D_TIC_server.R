TIC_3D_server <- function(input, output, session) {
  # curve_dir_path setting
  observeEvent(ignoreNULL = TRUE,
               eventExpr = {
                 input$"3D_TIC_curve_dir_path"
               },
               handlerExpr = {
                 if (input$"3D_TIC_curve_dir_path" > 0) {
                   # condition prevents handler execution on initial app launch
                   
                   # launch the directory selection dialog with initial path read from the widget
                   path <- choose.dir(default = readDirectoryInput(session, "3D_TIC_curve_dir_path"))
                   
                   # update the widget value
                   updateDirectoryInput(session, "3D_TIC_curve_dir_path", value = path)
                 }
               })
  
  # run plot_3D_TIC function
  observeEvent(input$plot_3D_TIC, {
    output$"3D_TIC_plot" <- plotly::renderPlotly({
      plot_3D_TIC(
        curve_dir_path = readDirectoryInput(session, "3D_TIC_curve_dir_path"),
        calculate_percentage = input$"3D_TIC_calculate_percentage",
        line_width = input$"3D_TIC_line_width",
        font_size = input$"3D_TIC_font_size",
        x_min = input$"3D_TIC_x_min",
        x_max = input$"3D_TIC_x_max",
        z_min = input$"3D_TIC_z_min",
        z_max = input$"3D_TIC_z_max",
        x_title = input$"3D_TIC_x_title",
        z_title = input$"3D_TIC_z_title"
      )
    })
  })
  
}