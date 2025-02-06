# Cache environment to store vector stores
.rag_cache <- new.env(parent = emptyenv())

#' RAG Tool
#'
#' A tool for searching through documentation using vector embeddings.
#'
#' @param query The search query
#' @param dir Directory containing markdown or text files
#' @param n_context Number of similar texts to use as context
#' @param update If TRUE, force update of vector store
#' @param format_context If TRUE, returns formatted context string instead of raw results
#' @return If format_context is FALSE (default), returns a list of relevant text chunks with sources and similarity scores.
#'         If format_context is TRUE, returns a formatted string with context and citations.
#' @export
tool_rag <- function(query, dir = "~/knowledgebase", n_context = 3, update = FALSE, format_context = FALSE) {
  dir <- normalizePath(dir, mustWork = TRUE)
  store_file <- file.path(dir, "_vector_store.rds")

  # Get cache key based on directory and file modification time
  cache_key <- dir
  if (file.exists(store_file)) {
    cache_key <- paste0(cache_key, "_", file.info(store_file)$mtime)
  }

  # Try to get store from cache first
  store <- if (!update && exists(cache_key, envir = .rag_cache)) {
    get(cache_key, envir = .rag_cache)
  } else {
    # Update vector store if needed
    if (update || !file.exists(store_file)) {
      update_vector_store(dir)
    }
    # Load and cache store
    store <- readRDS(store_file)
    assign(cache_key, store, envir = .rag_cache)
    store
  }

  # Get embedding for query
  query_embedding <- llm_embedding(query)

  # Calculate similarities using cosine similarity
  similarities <- sapply(store$embeddings, function(e) {
    sum(e * query_embedding) / (sqrt(sum(e^2)) * sqrt(sum(query_embedding^2)))
  })

  # Get top n_context most similar chunks
  top_indices <- order(similarities, decreasing = TRUE)[seq_len(min(n_context, length(store$chunks)))]

  # Prepare results
  results <- list(
    chunks = store$chunks[top_indices],
    sources = basename(store$sources[top_indices]),
    similarities = similarities[top_indices]
  )

  # Format context if requested
  if (format_context && length(results$chunks) > 0) {
    context <- mapply(
      function(txt, src, sim) {
        sprintf("Source: %s (similarity: %.2f)\n%s", src, sim, txt)
      },
      results$chunks,
      results$sources,
      results$similarities,
      SIMPLIFY = TRUE
    )
    return(paste(context, collapse = "\n\n---\n\n"))
  }

  results
}

#' Update Vector Store
#'
#' Updates or creates a vector store for the given directory. The vector store contains
#' text chunks from markdown and text files, along with their embeddings and source
#' information. The store is saved as an RDS file in the directory.
#'
#' @param dir Directory containing markdown or text files
#' @return Invisibly returns TRUE on success
#' @keywords internal
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

    # Simple chunking by paragraphs
    file_chunks <- strsplit(content, "\n\n+")[[1]]
    chunks <- c(chunks, file_chunks)
    sources <- c(sources, rep(file, length(file_chunks)))
  }

  # Get embeddings for all chunks
  embeddings <- lapply(chunks, llm_embedding)

  # Save store
  store <- list(
    chunks = chunks,
    sources = sources,
    embeddings = embeddings
  )
  saveRDS(store, store_file)

  invisible(TRUE)
}

#' Clear RAG Cache
#'
#' Clears the cached vector stores to free up memory.
#'
#' @export
clear_rag_cache <- function() {
  rm(list = ls(envir = .rag_cache), envir = .rag_cache)
  invisible(TRUE)
}

