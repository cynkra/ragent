test_that("rag agent works", {
  skip_if_not(is_ollama_available(), "Ollama is not available")

  # Create test knowledge base
  kb <- create_temp_kb(list(
    office = "The office hours are from 9 AM to 5 PM Monday through Friday."
  ))
  on.exit(cleanup_temp_dir(kb))

  # Test basic RAG query
  result <- agent_rag(
    "What are the office hours?",
    dir = kb,
    n_context = 1
  )

  expect_type(result, "character")
  expect_true(grepl("9 AM to 5 PM", result, fixed = TRUE))
})
