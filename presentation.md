# Agentic Models with Local LLMs
## A Simple Example in R

---

# Slide 1: What is an Agentic Model?

- An AI assistant that can:
  - Process user requests
  - Use specialized tools
  - Combine information
  - Provide natural language responses

```r
agent <- create_agent(
  tools = list(calculator = calc_tool),
  system_prompt = "You are a helpful assistant..."
)
```

---

# Slide 2: How It Works

1. User asks a question
2. Agent analyzes request
3. Agent uses appropriate tool(s)
4. Tool returns result
5. LLM formulates friendly response

Example Flow:
```
User: "What is 15% of 850?"
↓
Agent recognizes calculation
↓
Calculator: 850 * 0.15 = 127.5
↓
LLM formats response:
"15% of 850 equals 127.50 CHF..."
```

---

# Slide 3: Advanced Example: Knowledge Base Agent

1. Multiple Tools & Queries
```r
agent <- create_agent(
  tools = list(
    kb = kb_tool,          # Search docs
    calculator = calc_tool  # Do math
  )
)
```

2. Complex Task:
```
"What's lunch cost for 5 people?"
↓
Search policy (30 CHF/person)
↓
Calculate: 5 * 30 = 150
↓
"According to policy, total is 150 CHF"
```

---

# Key Benefits

- Local LLMs (privacy, speed)
- Extensible tool system
- Natural language interface
- Combines multiple sources
- Real-world applications