#' Chat with LLM
#'
#' Send a prompt to the LLM and get a response.
#'
#' @param prompt Character string with the prompt
#' @param model Character string specifying the model to use
#' @param system_prompt Character string with the system prompt
#' @param format Optional list specifying the JSON schema for structured output.
#'        See https://ollama.com/blog/structured-outputs for details and examples.
#' @return If format is specified, returns a parsed R object.
#'         Otherwise, returns a character string containing the model's response.
#' @importFrom httr2 request req_headers req_body_json req_perform resp_body_json
#' @importFrom jsonlite toJSON fromJSON
#' @export
#' @examples
#' # Free text response
#' llm_chat(prompt = "What is 2 + 2?")
#'
#' # Structured output using JSON schema
#' llm_chat(
#'   prompt = "Extract the value and unit: 42 kilometers",
#'   format = list(
#'     type = "object",
#'     properties = list(
#'       value = list(type = "number"),
#'       unit = list(type = "string")
#'     ),
#'     required = c("value", "unit")
#'   )
#' )
llm_chat <- function(prompt,
                    model = default_chat_model(),
                    system_prompt = NULL,
                    format = NULL) {

  # Input validation
  if (is.null(prompt) || nchar(prompt) == 0) {
    stop("Prompt cannot be empty")
  }

  # Build messages list
  messages <- list(list(role = "user", content = prompt))
  if (!is.null(system_prompt)) {
    messages <- c(list(list(role = "system", content = system_prompt)), messages)
  }

  # Prepare request body
  body <- list(
    model = model,
    messages = messages,
    stream = FALSE
  )

  # Add format if specified
  if (!is.null(format)) {
    body$format <- format
  }

  # Make request using httr2
  response <- httr2::request("http://localhost:11434/api/chat") |>
    httr2::req_headers("Content-Type" = "application/json") |>
    httr2::req_body_json(body, auto_unbox = TRUE) |>
    httr2::req_perform()

  # Parse the response
  result <- httr2::resp_body_json(response)

  # Extract the message content
  content <- result$message$content
  if (is.null(content)) {
    stop("No content in Ollama response")
  }

  # If format was specified, parse the JSON response
  if (!is.null(format)) {
    content <- tryCatch({
      jsonlite::fromJSON(content)
    }, error = function(e) {
      stop("Failed to parse structured output: ", e$message)
    })
  }

  content
}
