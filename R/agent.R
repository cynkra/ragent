#' Create a Simple R Agent
#'
#' Creates an agent that can process tasks using a local Ollama model.
#'
#' @param model Character string specifying the Ollama model to use (default: "llama3.2:1b")
#' @param reasoning_model Character string specifying the model for complex reasoning tasks (default: "deepseek-r1:1.5b")
#' @param system_prompt Character string with the system prompt (optional)
#' @param tools List of tool functions the agent can use (optional)
#' @return A list containing the agent's configuration and methods
#' @export
#'
#' @examples
#' \dontrun{
#' agent <- create_agent()
#' response <- agent$ask("What is 2 + 2?")
#' }
create_agent <- function(model = "llama3.2:1b",
                        reasoning_model = "deepseek-r1:1.5b",
                        system_prompt = NULL,
                        tools = list()) {

  if (is.null(system_prompt)) {
    system_prompt <- "You are a helpful AI assistant. Answer questions directly and concisely."
  }

  # Store tools in the environment
  available_tools <- tools

  # Initialize the Ollama clients
  client <- ollama_client(
    model = model,
    system_prompt = system_prompt
  )

  reasoning_client <- ollama_client(
    model = reasoning_model,
    system_prompt = system_prompt
  )

  # Function to process a user query
  ask <- function(query) {
    # Use reasoning model for complex queries, basic model for simple ones
    selected_client <- if (is_complex_query(query)) reasoning_client else client

    response <- selected_client$chat(query)
    return(response)
  }

  # Helper function to determine if a query needs the reasoning model
  is_complex_query <- function(query) {
    # Keywords that suggest complex reasoning
    complex_patterns <- c(
      "why", "how", "explain", "analyze", "compare",
      "what if", "could you", "would you", "should",
      "relationship", "difference between"
    )

    query_lower <- tolower(query)
    any(sapply(complex_patterns, function(pattern) grepl(pattern, query_lower)))
  }

  # Helper function to extract JSON from response
  extract_json <- function(text) {
    # Find any JSON-like structure in the text
    json_pattern <- "\\{[^\\{\\}]*\"use_tool\"[^\\{\\}]*\\}"
    matches <- regmatches(text, gregexpr(json_pattern, text, perl = TRUE))[[1]]

    if (length(matches) > 0) {
      # Take the last match (in case there are multiple)
      json_text <- matches[length(matches)]

      # Clean up any remaining non-JSON content
      json_text <- gsub("^[^{]*\\{", "{", json_text)  # Clean start
      json_text <- gsub("\\}[^}]*$", "}", json_text)  # Clean end

      return(json_text)
    }

    # If no JSON found, return the original text
    return(text)
  }

  # Helper function to get tool description
  get_tool_description <- function(tool) {
    desc <- attr(tool, "description")
    if (is.null(desc)) {
      # Default description based on the function's documentation
      doc <- attr(tool, "srcref")
      if (!is.null(doc)) {
        desc <- attr(doc, "srcfile")$lines[1]
        desc <- gsub("^#'\\s*", "", desc)
      }
    }
    if (is.null(desc)) desc <- "No description available"
    return(desc)
  }

  # Function to process a task that might require tools
  process_task <- function(task) {
    cat("\n=== Task Received ===\n")
    cat(task, "\n")

    # Always use reasoning model for tool-based tasks
    tool_names <- paste(names(available_tools), collapse = ", ")
    tool_descriptions <- paste(
      sapply(names(available_tools),
             function(name) {
               tool <- available_tools[[name]]
               desc <- get_tool_description(tool)
               paste0("- ", name, ": ", desc)
             }),
      collapse = "\n"
    )

    cat("\n=== Available Tools ===\n")
    cat(tool_descriptions, "\n")

    # Define the schema for tool responses
    format <- list(
      type = "object",
      properties = list(
        use_tool = list(type = "boolean"),
        tool_name = list(type = "string"),
        tool_args = list(type = "string"),
        direct_response = list(type = "string")
      ),
      required = c("use_tool")
    )

    tool_prompt <- paste0(
      system_prompt,
      "\nAvailable tools:\n", tool_descriptions,
      "\nPlease determine if you need to use a tool to answer the query.",
      "\nIf you need to use a tool, set use_tool to true and provide the tool name and arguments.",
      "\nIf you don't need a tool, set use_tool to false and provide a direct response.",
      "\nExamples:",
      '\n- For calculations: {"use_tool": true, "tool_name": "calculator", "tool_args": "2 + 2"}',
      '\n- For direct answers: {"use_tool": false, "direct_response": "The answer is..."}'
    )

    cat("\n=== System Prompt ===\n")
    cat(system_prompt, "\n")

    task_client <- ollama_client(
      model = reasoning_model,
      system_prompt = tool_prompt,
      format = format  # Use structured output
    )

    cat("\n=== Using Model ===\n")
    cat(reasoning_model, "\n")

    # Get structured response directly
    cat("\n=== Getting Structured Response ===\n")
    response <- task_client$chat(task)
    cat(str(response), "\n")

    if (isTRUE(response$use_tool) && !is.null(response$tool_name)) {
      tool_name <- response$tool_name
      args <- response$tool_args

      cat("\n=== Tool Execution ===\n")
      cat("Tool:", tool_name, "\n")
      cat("Arguments:", args, "\n")
      cat("Available tools:", tool_names, "\n")

      if (tool_name %in% names(available_tools)) {
        cat("Tool found, executing...\n")
        # Execute tool and return its result
        result <- do.call(available_tools[[tool_name]], list(args))
        cat("\n=== Final Result ===\n")
        cat("Type:", typeof(result), "\n")
        cat("Value:", result, "\n")

        # Get a natural language response using structured output
        response_format <- list(
          type = "object",
          properties = list(
            response = list(
              type = "string",
              description = "A natural language response incorporating the calculation result"
            )
          ),
          required = c("response")
        )

        final_client <- ollama_client(
          model = reasoning_model,
          system_prompt = paste0(
            "You are a helpful assistant. The calculation result is: ", result,
            ". Please provide a natural response incorporating this result."
          ),
          format = response_format
        )

        final_response <- final_client$chat("Generate response")
        cat("\n=== Final Response ===\n")
        cat(final_response$response, "\n")
        return(final_response$response)
      } else {
        msg <- paste("Error: Invalid tool name. Available tools are:", tool_names)
        cat("\nError:", msg, "\n")
        return(msg)
      }
    }

    # Return direct response if no tool was used
    cat("\n=== Direct Response ===\n")
    cat(response$direct_response, "\n")
    return(response$direct_response)
  }

  # Return the agent interface
  list(
    ask = ask,
    process_task = process_task,
    model = model,
    reasoning_model = reasoning_model,
    system_prompt = system_prompt
  )
}

#' Example Calculator Tool
#'
#' A simple example tool that can perform basic calculations
#' @param expr A string containing a mathematical expression
#' @return The result of the calculation
#' @export
calc_tool <- function(expr) {
  cat("Calculator tool called with expression:", expr, "\n")
  result <- try(eval(parse(text = expr)), silent = TRUE)
  if (inherits(result, "try-error")) {
    msg <- paste("Error:", attr(result, "condition")$message)
    cat(msg, "\n")
    return(msg)
  }
  cat("Result:", result, "\n")
  result
}

# Add description to calculator tool
attr(calc_tool, "description") <- "Evaluates mathematical expressions. Examples: '2 + 2', 'sqrt(144)', '123 * 456', '144^0.5'."