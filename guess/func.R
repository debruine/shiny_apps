saveData <- function(data, stats, id, outputDir = "responses") {
  # Create a unique file name
  dataName <- sprintf("%s_data.csv", id)
  statsName <- sprintf("%s_stats.csv", id)
  
  # Write the files to the local system
  readr::write_csv(
    x = data,
    path = file.path(outputDir, dataName)
  )
  
  readr::write_csv(
    x = stats,
    path = file.path(outputDir, statsName)
  )
}


loadData <- function(outputDir = "responses", 
                     pattern = "*_data.csv") {
  # read all the files into a list
  files <- list.files(outputDir, 
                      pattern = pattern, 
                      full.names = TRUE)
  
  if (length(files) == 0) {
    # create empty data frame with correct columns
    data <- data.frame()
  } else {
    data <- lapply(files, function(f) {
      readr::read_csv(f) %>%
        mutate(session_id = gsub("responses/", "", f))
    })
    
    # Concatenate all data together into one data.frame
    data <- do.call(rbind, data)
  }
  
  data
}