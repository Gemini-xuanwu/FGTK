biomarker_server <- function(input, output, session) {
  # input_dir_path setting
  observeEvent(ignoreNULL = TRUE,
               eventExpr = {
                 input$biomarker_compare_result_dir
               },
               handlerExpr = {
                 if (input$biomarker_compare_result_dir > 0) {
                   # condition prevents handler execution on initial app launch
                   
                   # launch the directory selection dialog with initial path read from the widget
                   path <- choose.dir(default = readDirectoryInput(session, "biomarker_compare_result_dir"))
                   
                   # update the widget value
                   updateDirectoryInput(session, "biomarker_compare_result_dir", value = path)
                 }
               })
  
  # Create a list of stored notifications
  biomarker_notification_list <- list()
  
  # run biomarker function
  observeEvent(input$run_biomarker, {
    # Show function starts running
    biomarker_notification_start <- showNotification("The biomarker function starts running!",
                                                     type = "default",
                                                     duration = 10)
    biomarker_notification_list <<- c(biomarker_notification_list,
                                      biomarker_notification_start)
  })
  
  # run biomarker function
  observeEvent(input$run_biomarker, {
    biomarker_result <- biomarker(
      compare_result_dir = readDirectoryInput(session, "biomarker_compare_result_dir"),
      calculate_percentage = input$biomarker_calculate_percentage,
      font_size = input$biomarker_font_size,
      width = input$biomarker_width,
      height = input$biomarker_height
    )
    
    # Show function completion and error messages
    biomarker_notification_return_result <- showNotification(biomarker_result$text_message,
                                                             type = biomarker_result$type,
                                                             duration = 10)
    biomarker_notification_list <<- c(biomarker_notification_list,
                                      biomarker_notification_return_result)
  })
  
  # Empty notification list
  observeEvent(input$clear_biomarker_notifications, {
    # Remove notification messages one by one
    for (notification_alone in biomarker_notification_list) {
      removeNotification(notification_alone)
    }
    
    # Empty notification list
    biomarker_notification_list <<- list()
  })
}