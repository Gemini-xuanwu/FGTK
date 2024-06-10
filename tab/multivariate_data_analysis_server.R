multivariate_data_analysis_server <- function(input, output, session) {
  # input_dir_path setting
  observeEvent(ignoreNULL = TRUE,
               eventExpr = {
                 input$mda_compare_result_dir
               },
               handlerExpr = {
                 if (input$mda_compare_result_dir > 0) {
                   # condition prevents handler execution on initial app launch
                   
                   # launch the directory selection dialog with initial path read from the widget
                   path <- choose.dir(default = readDirectoryInput(session, "mda_compare_result_dir"))
                   
                   # update the widget value
                   updateDirectoryInput(session, "mda_compare_result_dir", value = path)
                 }
               })
  
  # Create a list of stored notifications
  mda_notification_list <- list()
  
  # run multivariate_data_analysis function
  observeEvent(input$run_multivariate_data_analysis, {
    # Show function starts running
    mda_notification_start <- showNotification(
      "The multivariate data analysis function starts running!",
      type = "default",
      duration = 10
    )
    mda_notification_list <<- c(mda_notification_list, mda_notification_start)
  })
  
  # run multivariate_data_analysis function
  observeEvent(input$run_multivariate_data_analysis, {
    mda_result <- multivariate_data_analysis(
      compare_result_dir = readDirectoryInput(session, "mda_compare_result_dir"),
      calculate_percentage = input$mda_calculate_percentage,
      font_size = input$mda_font_size,
      width = input$mda_width,
      height = input$mda_height
    )
    
    # Show function completion and error messages
    mda_notification_return_result <- showNotification(mda_result$text_message,
                                                       type = mda_result$type,
                                                       duration = 10)
    mda_notification_list <<- c(mda_notification_list, mda_notification_return_result)
  })
  
  # Empty notification list
  observeEvent(input$clear_mda_notifications, {
    # Remove notification messages one by one
    for (notification_alone in mda_notification_list) {
      removeNotification(notification_alone)
    }
    
    # Empty notification list
    mda_notification_list <<- list()
  })
}