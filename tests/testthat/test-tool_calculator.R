test_that("calculator tool handles edge cases", {
  # Test invalid expressions
  expect_error(tool_calculator("1 + "), "Invalid mathematical expression")
  expect_error(tool_calculator("x + 1"), "Could not evaluate")

  # Test non-numeric results
  expect_error(tool_calculator("TRUE"), "did not result in a number")
  expect_error(tool_calculator("'text'"), "did not result in a number")

  # Test input validation
  expect_error(tool_calculator(c("1 + 1", "2 + 2")), "must be a single string")
  expect_error(tool_calculator(NULL), "must be a single string")
})
