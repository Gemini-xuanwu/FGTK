plot_3D_TIC <- function(curve_dir_path,
                        calculate_percentage = FALSE,
                        line_width = 3,
                        font_size = 12,
                        x_min = 5,
                        x_max = 30,
                        z_min = 0,
                        z_max = 1000000,
                        x_title,
                        z_title) {
  # Import the necessary libraries
  if (!("package:tidyverse" %in% search())) {
    library(tidyverse)
  }
  if (!("package:ggplot2" %in% search())) {
    library(ggplot2)
  }
  
  # Check if the curve directory exists
  if (!exists("curve_dir_path")) {
    stop("The curve directory parameter does not set. Please provide the necessary parameter.")
  } else {
    if (!dir.exists(curve_dir_path)) {
      stop("The curve directory does not exist. Please check the path.")
    }
  }
  
  # Create empty vectors of samples, paths, groups, x_offsets, y_offsets
  samples <- NULL
  paths <- NULL
  groups <- NULL
  
  # Get a list of subdirectories (group names)
  dirs <- list.dirs(curve_dir_path,
                    full.names = F,
                    recursive = F)
  # Get a list of data path, sample name and sample group
  for (dir in dirs) {
    dir_path <- paste0(curve_dir_path, "/", dir)
    files <- list.files(path = dir_path, pattern = "(.CSV)$")
    # Report an error if the CSV file is not detected
    if (length(files) == 0) {
      stop(
        crayon::red(
          "No .CSV files found in this directory. Please delete the empty folder so that it does",
          "not affect the next calculation."
        )
      )
    }
    samples_paths <- paste0(dir_path, "/", files)
    samples <- append(samples, sub(".CSV$", "", files))
    paths <- append(paths, samples_paths)
    groups <- append(groups, rep(dir, times = length(files)))
  }
  group_xlsx <- data.frame(sample = samples, group = groups)
  
  df_samples <- data.frame()
  ### Read all datas
  for (i in seq(1, length(paths))) {
    # Read a data
    df <- data.frame(All = scan(paths[i], what = "", sep = "\t")[-1:-3]) %>%
      mutate(
        Time = str_sub(All, 1, str_locate(All, ",")[, "start"] - 1),
        Height = str_sub(All, str_locate(All, ",")[, "start"] + 1, -1)
      ) %>%
      select(-1)
    colnames(df)[2] <- samples[i]
    df <- df %>% mutate(across(c(1, 2), as.numeric))
    # Integrate all datas
    if (ncol(df_samples) == 0) {
      df_samples <- df
    } else{
      df_samples <- full_join(df_samples, df, by = "Time")
    }
  }
  
  # Calculate relative abundance
  if (calculate_percentage == TRUE) {
    df_samples <- cbind(df_samples[1], apply(df_samples[-1], 2, function(x) {
      x <- x / max(x, na.rm = TRUE) * 100
    }))
  }
  
  df_melt <- reshape2::melt(df_samples, id = "Time") %>%
    arrange(Time) %>%
    na.omit()
  df_last <- left_join(df_melt, group_xlsx, by = c("variable" = "sample")) %>%
    mutate(time = Time, abundance = value)
  
  TIC_plot <- df_last %>% group_by(variable) %>% plotly::plot_ly(
    x = ~ group,
    y = ~ Time,
    z = ~ value,
    color = ~ group,
    type = "scatter3d",
    mode = "lines",
    line = list(width = line_width)
  )
  if (calculate_percentage == TRUE) {
    TIC_plot <- TIC_plot %>%
      plotly::layout(
        font = list(family = "Times New Roman", size = font_size),
        showlegend = FALSE,
        scene = list(
          aspectmode = 'manual',
          aspectratio = list(x = 1, y = 1, z = 1),
          xaxis = list(
            title = "",
            zeroline = FALSE,
            gridcolor = "black",
            gridwidth = 2
          ),
          yaxis = list(
            title = ifelse(nchar(x_title) == 0, "Retention time (min)", x_title),
            range = c(x_min, x_max),
            tickangle = 0,
            zeroline = FALSE,
            gridcolor = "black",
            gridwidth = 2
          ),
          zaxis = list(
            title = ifelse(nchar(z_title) == 0, "Relative abundance (%)", z_title),
            range = c(z_min, z_max),
            # tickformat = ".0e",
            zeroline = FALSE,
            gridcolor = "black",
            gridwidth = 2
          ),
          camera = list(
            up = list(x = 0, y = 0, z = 1),
            center = list(x = 0, y = 0, z = 0),
            eye = list(x = 2, y = 1, z = 0.1)
          )
        )
      ) %>%
      plotly::config(
        toImageButtonOptions = list(
          format = "jpeg",
          filename = "3D_TIC",
          height = 600,
          width = 800,
          scale = 10
        )
      )
  } else{
    TIC_plot <- TIC_plot %>%
      plotly::layout(
        font = list(family = "Times New Roman", size = font_size),
        showlegend = FALSE,
        scene = list(
          aspectmode = 'manual',
          aspectratio = list(x = 1, y = 1, z = 1),
          xaxis = list(
            title = "",
            zeroline = FALSE,
            gridcolor = "black",
            gridwidth = 2
          ),
          yaxis = list(
            title = ifelse(nchar(x_title) == 0, "Retention time (min)", x_title),
            range = c(x_min, x_max),
            tickangle = 0,
            zeroline = FALSE,
            gridcolor = "black",
            gridwidth = 2
          ),
          zaxis = list(
            title = ifelse(nchar(z_title) == 0, "Abundance", z_title),
            range = c(z_min, z_max),
            tickformat = ".0e",
            zeroline = FALSE,
            gridcolor = "black",
            gridwidth = 2
          ),
          camera = list(
            up = list(x = 0, y = 0, z = 1),
            center = list(x = 0, y = 0, z = 0),
            eye = list(x = 2, y = 1, z = 0.1)
          )
        )
      ) %>%
      plotly::config(
        toImageButtonOptions = list(
          format = "jpeg",
          filename = "3D_TIC",
          height = 600,
          width = 800,
          scale = 10
        )
      )
  }
  
  return(TIC_plot)
}