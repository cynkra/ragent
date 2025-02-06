#' Get Embeddings from LLM
#'
#' Get vector embeddings for text using the LLM.
#'
#' @param text Text to get embeddings for
#' @param model Model to use for embeddings (default: "nomic-embed-text")
#' @return Numeric vector containing the embedding
#' @importFrom httr POST content
#' @importFrom jsonlite fromJSON
#' @export
llm_embedding <- function(text, model = "nomic-embed-text") {
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
    rawToChar(httr::content(response, as = "raw"))
  )

  content$embedding
}
