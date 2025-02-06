#' Internal calculator tool
#'
#' A helper function for performing mathematical calculations.
#'
#' @param query The calculation to perform as a string
#' @return The result of the calculation
#' @noRd
tool_calculator <- function(query) {
  # Input validation
  if (!is.character(query) || length(query) != 1) {
    stop("Calculator query must be a single string")
  }

  # Parse the expression
  expr <- try(parse(text = query), silent = TRUE)
  if (inherits(expr, "try-error")) {
    stop(sprintf("Invalid mathematical expression: '%s'", query))
  }

  # Evaluate with safety checks
  result <- try(eval(expr), silent = TRUE)
  if (inherits(result, "try-error")) {
    stop(sprintf("Could not evaluate expression: '%s'", query))
  }

  # Check result type
  if (!is.numeric(result) && !is.complex(result)) {
    stop(sprintf("Expression '%s' did not result in a number", query))
  }

  result
}
