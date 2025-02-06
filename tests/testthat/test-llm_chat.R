test_that("llm_chat works with structured output", {
  skip_if_not(is_ollama_available(), "Ollama is not available")

  result <- llm_chat(
    "Extract the number and unit: 42 kilometers",
    model = default_chat_model(),
    system_prompt = paste(
      "You are a helpful assistant that provides structured output.",
      "ONLY respond with a valid JSON object, no other text.",
      "Example response: {\"value\": 42, \"unit\": \"kilometers\"}"
    ),
    format = list(
      type = "object",
      properties = list(
        value = list(type = "number"),
        unit = list(type = "string")
      )
    )
  )

  expect_type(result, "list")
  expect_true(all(c("value", "unit") %in% names(result)))
  expect_type(result$value, "integer")
  expect_type(result$unit, "character")
})

test_that("llm_chat works with free text", {
  skip_if_not(is_ollama_available(), "Ollama is not available")

  result <- llm_chat(
    "What is 2 + 2?",
    model = default_chat_model(),
    system_prompt = "You are a helpful math assistant. Keep answers short and direct."
  )

  expect_type(result, "character")
  expect_true(nchar(result) > 0)
})

test_that("llm_chat handles errors gracefully", {
  skip_if_not(is_ollama_available(), "Ollama is not available")

  # Test empty prompt
  expect_error(
    llm_chat(""),
    "Prompt cannot be empty"
  )

  # Test invalid schema
  expect_error(
    llm_chat("test", format = list(invalid = "schema")),
    "No content in Ollama response"
  )
})
