% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/agent_calculator.R
\name{agent_calculator}
\alias{agent_calculator}
\title{Calculator Agent}
\usage{
agent_calculator(prompt, model = default_chat_model())
}
\arguments{
\item{prompt}{A character string containing the calculation query in natural language}

\item{model}{Model to use for chat completion (default: default_chat_model())}
}
\value{
A character string with a formatted explanation of the calculation result
}
\description{
Performs calculations and explains the results in natural language. The function takes a
natural language query, converts it to a mathematical expression, evaluates it, and
provides a human-friendly explanation of the result.
}
\examples{
\dontrun{
agent_calculator("2 + 2")
agent_calculator("What is the square root of 16?")
}
}
