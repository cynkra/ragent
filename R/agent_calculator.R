#' Calculator Agent
#'
#' Performs calculations and explains the results in natural language. The function takes a
#' natural language query, converts it to a mathematical expression, evaluates it, and
#' provides a human-friendly explanation of the result.
#'
#' @param prompt A character string containing the calculation query in natural language
#' @param model Model to use for chat completion (default: default_chat_model())
#'
#' @return A character string with a formatted explanation of the calculation result
#' @export
#'
#' @examples
#' \dontrun{
#' agent_calculator("2 + 2")
#' agent_calculator("What is the square root of 16?")
#' }
agent_calculator <- function(prompt, model = default_chat_model()) {

  # STEP 1: Parse natural language into mathematical expression ---------------------
  system_prompt <-
"You are a calculator agent that can perform calculations.
Available tools: calculator

Your task is to return a JSON object with two fields:
- is_calc: Set to true if there is a calculation to perform, false otherwise
- calc_args: A valid R mathematical expression as a string, WITHOUT any brackets []

Examples of valid responses:
For 'What is 2 plus 2?':
{
  'is_calc': true,
  'calc_args': '2 + 2'
}

For 'What is the square root of 16?':
{
  'is_calc': true,
  'calc_args': 'sqrt(16)'
}

For 'What is pi times 5?':
{
  'is_calc': true,
  'calc_args': 'pi * 5'
}

For 'How are you today?':
{
  'is_calc': false,
  'calc_args': ''
}

Important:
- calc_args must be a valid R expression
- Do NOT wrap the expression in square brackets []
- Use standard R operators: +, -, *, /, ^, sqrt(), log(), exp(), etc.
- For non-calculation queries, set is_calc to false and calc_args to an empty string"

  ans_expression <- llm_chat(
    prompt = prompt,
    system_prompt = system_prompt,
    format = list(
      type = "object",
      properties = list(
        is_calc = list(type = "boolean"),
        calc_args = list(type = "string")
      ),
      required = c("is_calc", "calc_args")
    )
  )

  if (!ans_expression$is_calc) {
    stop("No calculation to perform")
  }

  # STEP 2: Evaluate the mathematical expression ---------------------------------
  ans_tool <- tool_calculator(ans_expression$calc_args)

  # STEP 3: Generate human-friendly explanation ---------------------------------
  prompt_explanation <- paste(
    "Original question:", prompt, "\n",
    "The calculation '", ans_expression$calc_args, "' resulted in: ", ans_tool, "\n",
    "Please format this nicely and provide any relevant explanation."
  )

  system_prompt_explanation <-
"You are a helpful math assistant that explains calculation results clearly.

Your task is to take a calculation and its result, then provide a clear, concise explanation.
- Format numbers appropriately (e.g., round very long decimals if appropriate)
- Explain the result in natural language
- Add relevant context if helpful
- Keep the explanation brief and to the point

Examples:

For 'sqrt(16) = 4':
The square root of 16 is 4, which means 4 x 4 = 16.

For 'pi * 5 = 15.70796':
5 times pi is approximately 15.71 (rounded to 2 decimal places).

For 'log(100) = 4.60517':
The natural logarithm of 100 is approximately 4.61.

Important:
- Focus on explaining the result, not performing new calculations
- Keep explanations concise but clear
- Round very long decimals to a reasonable number of places
- Use proper mathematical terminology"

  ans_explanation <- llm_chat(
    prompt = prompt_explanation,
    system_prompt = system_prompt_explanation
  )

  ans_explanation
}
