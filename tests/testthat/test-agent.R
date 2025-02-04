test_that("agent creation works", {
  agent <- create_agent()
  expect_type(agent, "list")
  expect_true(all(c("ask", "process_task", "model", "system_prompt") %in% names(agent)))
})

test_that("calculator tool works", {
  result <- calc_tool("2 + 2")
  expect_equal(result, 4)

  result <- calc_tool("sqrt(16)")
  expect_equal(result, 4)

  result <- calc_tool("invalid")
  expect_true(grepl("^Error:", result))
})