classification_summaries_server <- function(input, output, session) {
  # compare_result_dir setting
  observeEvent(ignoreNULL = TRUE,
               eventExpr = {
                 input$classification_summaries_compare_result_dir
               },
               handlerExpr = {
                 if (input$classification_summaries_compare_result_dir > 0) {
                   # condition prevents handler execution on initial app launch
                   
                   # launch the directory selection dialog with initial path read from the widget
                   path <- choose.dir(default = readDirectoryInput(
                     session,
                     "classification_summaries_compare_result_dir"
                   ))
                   
                   # update the widget value
                   updateDirectoryInput(session,
                                        "classification_summaries_compare_result_dir",
                                        value = path)
                 }
               })
  
  # Create a list of stored notifications
  classification_summaries_notification_list <- list()
  
  # run classification_summaries function
  observeEvent(input$run_classification_summaries, {
    # Show function starts running
    classification_summaries_notification_start <- showNotification(
      "The classification summaries function starts running!",
      type = "default",
      duration = 10
    )
    classification_summaries_notification_list <<- c(
      classification_summaries_notification_list,
      classification_summaries_notification_start
    )
  })
  
  # run classification_summaries function
  observeEvent(input$run_classification_summaries, {
    classification_summaries_result <- classification_summaries(
      compare_result_dir = readDirectoryInput(session, "classification_summaries_compare_result_dir"),
      calculate_type = input$classification_summaries_calculate_type
    )
    
    # Show function completion and error messages
    classification_summaries_notification_return_result <- showNotification(
      classification_summaries_result$text_message,
      type = classification_summaries_result$type,
      duration = 10
    )
    classification_summaries_notification_list <<- c(
      classification_summaries_notification_list,
      classification_summaries_notification_return_result
    )
  })
  
  # Empty notification list
  observeEvent(input$clear_classification_summaries_notifications, {
    # Remove notification messages one by one
    for (notification_alone in classification_summaries_notification_list) {
      removeNotification(notification_alone)
    }
    
    # Empty notification list
    classification_summaries_notification_list <<- list()
  })
}