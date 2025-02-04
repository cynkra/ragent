#' Create an Ollama client
#'
#' @param model Character string specifying the model to use
#' @param system_prompt Character string with the system prompt
#' @param format Optional list specifying the JSON schema for structured output
#' @return A list containing the client's methods
#' @importFrom httr POST add_headers content
#' @importFrom jsonlite toJSON fromJSON
ollama_client <- function(model, system_prompt = NULL, format = NULL) {
  base_url <- "http://localhost:11434/api"

  chat <- function(prompt) {
    body <- list(
      model = model,
      messages = list(
        list(role = "system", content = system_prompt),
        list(role = "user", content = prompt)
      ),
      stream = FALSE
    )

    # Add format if specified
    if (!is.null(format)) {
      body$format <- format
    }

    response <- httr::POST(
      url = paste0(base_url, "/chat"),
      body = jsonlite::toJSON(body, auto_unbox = TRUE),
      httr::add_headers("Content-Type" = "application/json")
    )

    result <- jsonlite::fromJSON(rawToChar(httr::content(response, as = "raw")))

    if (!is.null(format)) {
      # If format is specified, parse the content as JSON
      result$message$content <- jsonlite::fromJSON(result$message$content)
    }

    result$message$content
  }

  list(chat = chat)
}

#' Get Embeddings from Ollama
#'
#' Gets embeddings for text using the Ollama API.
#'
#' @param text Text to get embeddings for
#' @param model Model to use for embeddings (default: "nomic-embed-text")
#' @return List containing the embedding vector
#' @export
ollama_embedding <- function(text, model = "nomic-embed-text") {
  response <- httr::POST(
    url = "http://localhost:11434/api/embeddings",
    body = list(
      model = model,
      prompt = text
    ),
    encode = "json"
  )

  if (response$status_code != 200) {
    stop("Error getting embeddings from Ollama API")
  }

  content <- jsonlite::fromJSON(
    rawToChar(response$content)
  )

  content
}