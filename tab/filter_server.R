filter_server <- function(input, output, session) {
  # compare_result_dir setting
  observeEvent(ignoreNULL = TRUE,
               eventExpr = {
                 input$filter_compare_result_dir
               },
               handlerExpr = {
                 if (input$filter_compare_result_dir > 0) {
                   # condition prevents handler execution on initial app launch
                   
                   # launch the directory selection dialog with initial path read from the widget
                   path <- choose.dir(default = readDirectoryInput(session, "filter_compare_result_dir"))
                   
                   # update the widget value
                   updateDirectoryInput(session, "filter_compare_result_dir", value = path)
                 }
               })
  
  # Create a list of stored notifications
  filter_notification_list <- list()
  
  # run filter function
  observeEvent(input$run_filter, {
    # Show function starts running
    filter_notification_start <- showNotification("The filter function starts running!",
                                                  type = "default",
                                                  duration = 10)
    filter_notification_list <<- c(filter_notification_list, filter_notification_start)
  })
  
  # run filter function
  observeEvent(input$run_filter, {
    filter_result <- fgtk_filter(
      compare_result_dir = readDirectoryInput(session, "filter_compare_result_dir"),
      cut_ck = input$cut_ck,
      frequency_within_group_thr = input$frequency_within_group_thr,
      frequency_within_overall_thr = input$frequency_within_overall_thr
    )
    
    # Show function completion and error messages
    filter_notification_return_result <- showNotification(filter_result$text_message,
                                                          type = filter_result$type,
                                                          duration = 10)
    filter_notification_list <<- c(filter_notification_list,
                                   filter_notification_return_result)
  })
  
  # Empty notification list
  observeEvent(input$clear_filter_notifications, {
    # Remove notification messages one by one
    for (notification_alone in filter_notification_list) {
      removeNotification(notification_alone)
    }
    
    # Empty notification list
    filter_notification_list <<- list()
  })
}