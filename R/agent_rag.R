#' RAG Agent
#'
#' Answer questions using Retrieval Augmented Generation (RAG). The function takes a
#' natural language query, searches through a knowledge base of documents, and provides
#' a contextually relevant answer based on the retrieved information. It uses a multi-step
#' process that includes generating follow-up queries to gather additional context.
#'
#' @param query A character string containing the question to answer
#' @param dir Directory containing markdown or text files (default: "~/knowledgebase")
#' @param n_context Number of similar texts to use as context (default: 3)
#' @param model Model to use for chat completion (default: default_chat_model())
#'
#' @return A character string containing the answer with source citations
#' @export
#'
#' @examples
#' \dontrun{
#' agent_rag("How do I submit a receipt?")
#' agent_rag("What are the office hours?")
#' agent_rag("How to plan my vacation?", model = "deepseek-r1:8b")
#' }
agent_rag <- function(query,
                     dir = "~/knowledgebase",
                     n_context = 3,
                     model = default_chat_model()) {

  # STEP 1: Initial RAG query ---------------------------------------------
  # Get initial context from knowledge base
  initial_context <- tool_rag(
    query = query,
    dir = dir,
    n_context = n_context,
    format_context = TRUE
  )

  if (length(initial_context) == 0) {
    return("I could not find any relevant information in the knowledge base.")
  }

  # STEP 2: Generate follow-up queries -----------------------------------
  # Ask LLM to suggest additional queries based on initial context
  followup_prompt <- sprintf(
"Based on the question '%s' and these document excerpts:

%s

What additional questions should we ask to get more relevant context?
Return exactly 2 questions that would help provide a more complete answer.
Format as a JSON array of strings.",
    query,
    initial_context
  )

  followup_queries <- llm_chat(
    prompt = followup_prompt,
    system_prompt =
"You are a helpful assistant generating follow-up questions.
Focus on questions that would provide complementary information to what we already have.
Generate questions that:
- Are specific and well-defined
- Cover different aspects than the original results
- Would help fill gaps in the current context",
    format = list(
      type = "array",
      items = list(type = "string"),
      minItems = 2,
      maxItems = 2
    )
  )

  # STEP 3: Additional RAG queries ---------------------------------------
  # Get additional context from follow-up queries
  additional_contexts <- lapply(followup_queries, function(q) {
    tool_rag(
      query = q,
      dir = dir,
      n_context = n_context,
      format_context = TRUE
    )
  })

  # Combine all contexts
  all_contexts <- c(
    list(
      original = sprintf("Original query '%s':\n%s", query, initial_context)
    ),
    stats::setNames(
      lapply(seq_along(followup_queries), function(i) {
        sprintf("Follow-up query '%s':\n%s",
                followup_queries[[i]],
                additional_contexts[[i]])
      }),
      paste0("followup_", seq_along(followup_queries))
    )
  )

  # STEP 4: Generate final response -------------------------------------
  # Create final prompt combining all contexts
  final_prompt <- sprintf(
"Answer this question: %s

Based on all these search results:

%s",
    query,
    paste(unlist(all_contexts), collapse = "\n\n---\n\n")
  )

  system_prompt <-
"You are a knowledgeable assistant that answers questions based on provided documents.
Follow these guidelines:
- Synthesize information from all provided sources
- Always cite your sources when providing information
- If the information is not in the documents, say so clearly
- Keep answers concise but complete
- Use bullet points for multi-part answers
- Include relevant quotes when appropriate"

  # Generate the final response using the LLM
  answer <- llm_chat(
    prompt = final_prompt,
    system_prompt = system_prompt,
    model = model
  )

  answer
}
