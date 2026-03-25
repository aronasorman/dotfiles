---
name: codemap
description: Use when debugging errors, understanding feature implementations, or tracing execution flow through unfamiliar code - generates hierarchical maps with inline code snippets (5-10 lines), file:line references, and execution traces. Supports Mermaid diagrams when explicitly requested. Triggers: debug, trace, explain, understand.
---

# CodeMap Skill

Generate AI-annotated codebase maps with inline code context for debugging and understanding.

## When to Use This Skill

Activate when the user wants to:

**Debugging:**
- Trace the cause of an error or bug
- Understand why something is failing
- Analyze error paths and edge cases
- Find where exceptions originate

**Feature Explanation:**
- Understand how a feature is implemented
- Learn execution flow through the system
- See how components interact
- Trace data transformations

**Codebase Understanding:**
- Onboard to unfamiliar code
- Plan refactoring or modifications
- Identify entry points for changes
- Document complex flows

## Core Instructions

### 1. Gather Context
Ask clarifying questions:
- **For debugging**: What error? When does it occur? Error messages?
- **For features**: Which feature? What's the entry point (URL, function)?
- **For understanding**: What specific aspect? What's the use case?

### 2. Systematic Analysis
Use the Explore agent (Task tool with subagent_type=Explore) to:
- Find entry points (routes, main functions, CLI commands, error locations)
- Trace execution flow through function calls
- Identify data transformations and state changes
- Map dependencies between components
- Read actual code at each critical point

### 3. Extract Code Snippets
For each critical step:
- Read the file using the Read tool
- Extract 5-10 lines of relevant code
- Include the containing function or class signature
- Show the exact line where key operations occur

### 4. Generate Hierarchical Map
Create a map showing:

**For Debugging:**
- 🐛 Error context (exception, location, trigger)
- 📍 Error location with code snippet
- 🔄 Execution trace leading to error
- 🔧 Root cause analysis
- 💡 Recommended fixes with code diffs

**For Feature Explanation:**
- 📍 Entry point with code snippet
- 🔄 Execution flow (numbered steps)
- Each step includes:
  - File:line reference
  - Containing function/class
  - 5-10 line code snippet
  - 💡 Explanation of what/why
  - ⚠️  Edge cases or error conditions
- 📊 Component summary
- 🔍 Critical decisions
- 💡 Modification entry points

### 5. Code Snippet Format
Use markdown code blocks with proper language identifiers for syntax highlighting.
**IMPORTANT**: Code fences MUST start at column 0 (no indentation) for proper syntax highlighting.

**Format:**
1. Show file:line reference above the code block (can use └─ prefix)
2. Code fence starts at column 0 with language identifier (typescript, python, javascript, etc.)
3. Include function/class signature as a comment at the top
4. Show 5-10 lines of relevant code
5. Mark error lines with ❌ in a comment
6. Add explanations below the code block with emoji bullets

**Example:**

**Step 1: User Lookup**
└─ src/services/auth.service.ts:89 - findUserByEmail()

```typescript
// class AuthService
async findUserByEmail(email: string): Promise<User | null> {
  const user = await this.db.query(
    'SELECT * FROM users WHERE email = $1',
    [email.toLowerCase()]
  );
  return user.rows[0] || null;
}
```

💡 Normalizes email to lowercase before query
💡 Uses parameterized query to prevent SQL injection
⚠️  Returns null if user not found → triggers 401 error

**For nested calls, use indentation (spaces) with arrow (→):**

  **→ Calls:** src/database/users.ts:156 - query()

  ```typescript
  // class DatabaseConnection
  async query(sql: string, params: any[]): Promise<QueryResult> {
    const client = await this.pool.connect();
    try {
      return await client.query(sql, params);
    } finally {
      client.release();
    }
  }
  ```

  💡 Uses connection pooling for performance

**For error locations, mark with ❌ in comment:**

```typescript
// class SpeechProcessor
async analyzeSpeech(speechId: string): Promise<AnalysisResult> {
  const speech = await this.getSpeechData(speechId);

  // ❌ ERROR: speech.audio is undefined
  const duration = speech.audio.duration;
  return { metrics, duration };
}
```

### 6. Format for Terminal
Use these conventions:

**Structure:**
- 📍 Entry points
- 🔄 Execution flow header
- **Bold step numbers** for main flow steps (e.g., **Step 1: Request Validation**)
- Horizontal rules (---) to separate major steps
- 📊 Component summary
- 🔍 Key decisions
- 🐛 Error/bug context
- 🔧 Root cause analysis
- Separator line: ===...=== (80 characters)

**Code Blocks:**
- Code fences MUST start at column 0 (no indentation before ```)
- Use language identifiers for syntax highlighting (typescript, python, javascript, etc.)
- Function/class context as comment at top of code block
- ❌ Error markers in code comments

**Hierarchy:**
- file:line notation with └─ prefix
- Nested calls: Indent with spaces, use **→ Calls:** prefix
- Explanations: 💡 for insights, ⚠️ for warnings/edge cases

**Component Lists:**
- Use bullet points (•) instead of tree chars for flat lists

### 7. Output Directly to Terminal
- Display the complete map inline in the conversation
- Do NOT save to files
- Keep output under 300 lines when possible (pageable in terminal)
- End with a footer suggesting follow-up actions

### 8. Follow-Up Interactions
Support deepening:
- "Expand step 3" → show more code context for that step
- "Show more of that function" → full function definition
- "What calls this?" → reverse trace to callers
- "Show error handling" → map error paths
- "Show the fix" → generate code diff for recommended changes
- "Create a mermaid graph" → generate visual diagram (see section 9)

### 9. Mermaid Diagram Generation
When the user explicitly asks to "create a mermaid graph", "generate a diagram", or "visualize this":

**Use appropriate diagram types:**
- **Flowchart** (graph TD/LR): For execution flow, decision trees
- **Sequence diagram**: For inter-component communication
- **Class diagram**: For object relationships
- **State diagram**: For state machines

**Flowchart Best Practices:**
- Use `graph TD` (top-down) or `graph LR` (left-right)
- Clear node labels with context
- Show decision points with diamond shapes
- Color-code: blue for entry, red for errors, green for success
- Keep it focused (8-15 nodes maximum)

**Example patterns:**
See [mermaid-examples.md](templates/mermaid-examples.md) for complete patterns including:
- Authentication flow (with error paths)
- Data pipeline (with transformations)
- API request flow (with middleware)
- State transitions
- Sequence diagrams for service communication

**After generating:**
- Provide links to Mermaid viewers
- Offer to modify (simplify, add detail, different type)
- Suggest related views

### 10. Quality Standards

✅ **DO:**
- Include actual code snippets (5-10 lines) for every critical step
- Start code fences at column 0 (no indentation) for proper syntax highlighting
- Use markdown code blocks with proper language identifiers (typescript, python, javascript, etc.)
- Show containing function/class signature as comment at top of code block
- Use **bold step numbers** for main flow steps
- Use horizontal rules (---) to separate major steps
- Use **→ Calls:** with indentation for nested calls
- Start from actual entry points (not assumed)
- Trace through real code (not guessed architecture)
- Include file:line for every reference
- Explain "why" not just "what" at decision points
- Mark error locations clearly with ❌ in code comments
- Provide actionable fix recommendations for bugs
- Show data flow with concrete examples from code
- Output directly to terminal

❌ **DON'T:**
- Save to files (output inline only)
- Create diagrams without code grounding
- Guess at implementation details
- Include every function (focus on critical path)
- Show snippets without explaining their purpose
- Assume user knows the codebase
- Create disconnected lists of files
- Skip error handling paths when debugging
- Provide vague "check for null" advice (show actual code fix)
- Generate Mermaid diagrams unless explicitly requested

## Token Efficiency

To stay under 500 lines in SKILL.md:
- Keep core instructions concise
- Reference template and example files for detailed patterns
- Progressive disclosure: main instructions here, detailed examples separate

## Supporting Files

- [mermaid-examples.md](templates/mermaid-examples.md) - Mermaid diagram patterns
- [debug-example.md](examples/debug-example.md) - Complete debugging scenario
- [auth-flow-example.md](examples/auth-flow-example.md) - Feature explanation example
