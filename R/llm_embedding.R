#' Get Embeddings from LLM
#'
#' Get vector embeddings for text using the LLM.
#'
#' @param text Text to get embeddings for
#' @param model Model to use for embeddings (default: "nomic-embed-text")
#' @return Numeric vector containing the embedding
#' @importFrom httr2 request req_body_json req_perform resp_body_json
#' @importFrom jsonlite fromJSON
#' @export
llm_embedding <- function(text, model = "nomic-embed-text") {
  response <- httr2::request("http://localhost:11434/api/embeddings") |>
    httr2::req_body_json(list(
      model = model,
      prompt = text
    )) |>
    httr2::req_perform()

  content <- httr2::resp_body_json(response)
  content$embedding
}
