migration_ui <- fluidPage(
  fluidRow(
    # input_dir_path setting
    directoryInput(
      "migration_raw_dir_path",
      label = "raw data folder path:",
      value = paste0(dirname("~"), "/", "Desktop")
    ),
    # output_dir_path setting
    directoryInput(
      "migration_data_dir_path",
      label = "migrated data folder path:",
      value = paste0(dirname("~"), "/", "Desktop")
    ),
    selectInput(
      "migration_file_type",
      "type of files to be migrated:",
      c("peak table", "TIC"),
      multiple = FALSE
    )
  ),
  actionButton("run_migration", "Run", icon = icon("gear", class = "fa-spin")),
  actionButton(
    "clear_migration_notifications",
    "Clear migration notifications"
  )
)