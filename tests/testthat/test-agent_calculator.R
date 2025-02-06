test_that("calculator agent handles various inputs", {
  skip_if_not(is_ollama_available(), "Ollama is not available")

  # Test basic arithmetic
  result <- agent_calculator("What is 2 plus 2?")
  expect_type(result, "character")
  expect_true(grepl("4", result))

  # Test mathematical functions
  result <- agent_calculator("What is the square root of 16?")
  expect_type(result, "character")
  expect_true(grepl("4", result))

  # Test trigonometry
  result <- agent_calculator("What is the sine of 0?")
  expect_type(result, "character")
  expect_true(grepl("0", result))

  # Test constants
  result <- agent_calculator("What is pi times 2?")
  expect_type(result, "character")
  expect_true(grepl("6.28", result) || grepl("6.283", result))

  # Test non-mathematical queries
  expect_error(
    agent_calculator("How are you today?"),
    "No calculation to perform"
  )
})

