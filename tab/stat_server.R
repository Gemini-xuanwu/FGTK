stat_server <- function(input, output, session) {
  # compare_result_dir setting
  observeEvent(ignoreNULL = TRUE,
               eventExpr = {
                 input$stat_compare_result_dir
               },
               handlerExpr = {
                 if (input$stat_compare_result_dir > 0) {
                   # condition prevents handler execution on initial app launch
                   
                   # launch the directory selection dialog with initial path read from the widget
                   path <- choose.dir(default = readDirectoryInput(session, "stat_compare_result_dir"))
                   
                   # update the widget value
                   updateDirectoryInput(session, "stat_compare_result_dir", value = path)
                 }
               })
  
  # Create a list of stored notifications
  stat_notification_list <- list()
  
  # run stat function
  observeEvent(input$run_stat, {
    # Show function starts running
    stat_notification_start <- showNotification("The stat function starts running!",
                                                type = "default",
                                                duration = 10)
    stat_notification_list <<- c(stat_notification_list, stat_notification_start)
  })
  
  # run stat function
  observeEvent(input$run_stat, {
    stat_result <- stat(
      compare_result_dir = readDirectoryInput(session, "stat_compare_result_dir"),
      calculate_percentage = input$stat_calculate_percentage,
      calculate_type = input$stat_calculate_type
    )
    
    # Show function completion and error messages
    stat_notification_return_result <- showNotification(stat_result$text_message,
                                                        type = stat_result$type,
                                                        duration = 10)
    stat_notification_list <<- c(stat_notification_list, stat_notification_return_result)
  })
  
  # Empty notification list
  observeEvent(input$clear_stat_notifications, {
    # Remove notification messages one by one
    for (notification_alone in stat_notification_list) {
      removeNotification(notification_alone)
    }
    
    # Empty notification list
    stat_notification_list <<- list()
  })
}