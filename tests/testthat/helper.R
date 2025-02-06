#' Check if Ollama is available
#'
#' @return TRUE if Ollama is running, FALSE otherwise
#' @keywords internal
is_ollama_available <- function() {
  tryCatch({
    response <- httr::GET("http://localhost:11434/api/version")
    httr::status_code(response) == 200
  }, error = function(e) FALSE)
}

#' Create temporary knowledge base for testing
#'
#' @param content Named list of file contents
#' @return Path to temporary directory
#' @keywords internal
create_temp_kb <- function(content = list()) {
  temp_dir <- tempfile()
  dir.create(temp_dir)

  for (name in names(content)) {
    writeLines(content[[name]], file.path(temp_dir, paste0(name, ".md")))
  }

  temp_dir
}

#' Clean up temporary directory
#'
#' @param dir Path to directory to remove
#' @keywords internal
cleanup_temp_dir <- function(dir) {
  unlink(dir, recursive = TRUE)
}
