test_that("rag tool works", {
  # Create test knowledge base
  kb <- create_temp_kb(list(
    sky = "The sky is blue because of Rayleigh scattering.",
    water = "Water boils at 100 degrees Celsius at sea level."
  ))
  on.exit(cleanup_temp_dir(kb))

  # Test basic search
  results <- tool_rag(
    "Why is the sky blue?",
    dir = kb,
    n_context = 1,
    format_context = FALSE
  )

  expect_type(results, "list")
  expect_true(any(grepl("Rayleigh scattering", results$chunks)))

  # Test formatted output
  formatted <- tool_rag(
    "Why is the sky blue?",
    dir = kb,
    n_context = 1,
    format_context = TRUE
  )

  expect_type(formatted, "character")
  expect_true(grepl("Rayleigh scattering", formatted))
})

