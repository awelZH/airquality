
download_zip <- function(download_url, destination_path){
  
  temp <- tempfile(tmpdir = destination_path, fileext = ".zip")
  command <- paste0("curl ", download_url, " --output ", temp)
  system(command, intern = TRUE)
  
  # unzip
  files <- unzip(temp, list = TRUE)
  unzip(temp, exdir = destination_path)
  
  # delete temp zip
  unlink(temp)
  
}

download_statpop_data <- function(year, destination_path){

  download_url <- get_bfs_metadata(year)
  download_zip(download_url, destination_path)
  
}


download_bafu_data <- function(id, destination_path){
  
  download_url <- get_swisstopo_metadata(id)$download_url
  download_zip(download_url, destination_path)
  
}


