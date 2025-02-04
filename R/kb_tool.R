#' Knowledge Base Tool
#'
#' A tool for querying a knowledge base using vector embeddings.
#'
#' @param dir Directory containing markdown or text files
#' @param n_context Number of similar texts to use as context (default: 3)
#' @param update If TRUE, force update of vector store
#' @param show_chunks Whether to display the chunks used for the answer
#' @return A function that can be used as a tool with create_agent()
#' @export
kb_tool <- function(dir = "~/knowledgebase", n_context = 3,
                   update = FALSE, show_chunks = FALSE) {

  dir <- normalizePath(dir, mustWork = TRUE)
  store_file <- file.path(dir, "_vector_store.rds")

  # Function that will be called by the agent
  tool <- function(query) {
    # Update vector store if needed
    if (update || !file.exists(store_file)) {
      update_vector_store(dir)
    }

    # Load vector store
    store <- readRDS(store_file)

    # Get embedding for query
    query_embedding <- get_embedding(query)

    # Calculate similarities
    similarities <- sapply(store$embeddings, function(e) {
      sum(e * query_embedding) / (sqrt(sum(e^2)) * sqrt(sum(query_embedding^2)))
    })

    # Get top n_context most similar chunks
    top_indices <- order(similarities, decreasing = TRUE)[1:min(n_context, length(store$chunks))]
    context <- store$chunks[top_indices]
    context_sources <- basename(store$sources[top_indices])
    top_similarities <- similarities[top_indices]

    # Format response
    chunks_info <- mapply(
      function(txt, src, sim) {
        sprintf("Source: %s (similarity: %.2f)\n%s", src, sim, txt)
      },
      context, context_sources, top_similarities,
      SIMPLIFY = TRUE
    )

    paste(chunks_info, collapse = "\n\n---\n\n")
  }

  # Add description attribute
  attr(tool, "description") <- paste(
    "Searches through a knowledge base of markdown/text files using semantic search.",
    "Use this tool to find information in documentation, policies, or any text files.",
    "Returns the most relevant text chunks with source information and similarity scores."
  )

  tool
}

#' Update Vector Store
#'
#' Updates the vector store with embeddings from documents in the specified directory.
#'
#' @param dir Directory containing markdown or text files
#' @return Invisibly returns TRUE on success
#' @export
update_vector_store <- function(dir) {
  dir <- normalizePath(dir, mustWork = TRUE)
  store_file <- file.path(dir, "_vector_store.rds")

  # Get all markdown and text files
  files <- list.files(dir, pattern = "\\.((md)|(txt))$", full.names = TRUE)

  # Read and chunk files
  chunks <- list()
  sources <- character()

  for (file in files) {
    content <- readLines(file, warn = FALSE)
    content <- paste(content, collapse = "\n")

    # Simple chunking by paragraphs (can be made more sophisticated)
    file_chunks <- strsplit(content, "\n\n+")[[1]]
    chunks <- c(chunks, file_chunks)
    sources <- c(sources, rep(file, length(file_chunks)))
  }

  # Get embeddings for all chunks
  embeddings <- lapply(chunks, get_embedding)

  # Save store
  store <- list(
    chunks = chunks,
    sources = sources,
    embeddings = embeddings
  )

  saveRDS(store, store_file)
  invisible(TRUE)
}

#' Get Embedding
#'
#' Gets an embedding vector for a text using the Ollama API.
#'
#' @param text Text to get embedding for
#' @return Numeric vector containing the embedding
#' @keywords internal
get_embedding <- function(text) {
  # Use the ollama_api to get embeddings
  response <- ollama_embedding(text)
  response$embedding
}