# Mermaid Diagram Examples for CodeMap

## When to Generate Diagrams

Generate Mermaid diagrams ONLY when user explicitly requests:
- "Create a mermaid graph"
- "Generate a diagram"
- "Visualize this flow"
- "Show me a diagram"

## 1. Flowchart - Execution Flow with Error Paths

```mermaid
graph TD
    A[POST /api/auth/login] -->|req.body| B{Validate Input}
    B -->|invalid format| C[Return 400]
    B -->|valid| D[Find User by Email]
    D -->|not found| E[Return 401]
    D -->|found| F{Verify Password}
    F -->|mismatch| E
    F -->|match| G[Generate JWT]
    G --> H[Create Redis Session]
    H -->|success| I[Return Token]
    H -->|Redis error| J[Return 500]

    style A fill:#e1f5ff
    style C fill:#ffe1e1
    style E fill:#ffe1e1
    style J fill:#ffe1e1
    style I fill:#e1ffe1
```

**Usage notes:**
- `graph TD` = top-down flow
- `{}` for decision nodes (diamonds)
- `[]` for process nodes (rectangles)
- `|label|` for edge labels
- Color success endpoints green, errors red, entry blue

## 2. Flowchart - Data Pipeline

```mermaid
graph LR
    A[Raw Speech Audio] --> B[Transcription Service]
    B --> C[Text Normalization]
    C --> D[Filler Word Detection]
    D --> E[Pace Analysis]
    E --> F[Aggregated Metrics]
    F --> G[Store in Firestore]

    B -.->|error| H[Retry Queue]
    H -.-> B

    style A fill:#e1f5ff
    style G fill:#e1ffe1
    style H fill:#fff5e1
```

**Usage notes:**
- `graph LR` = left-right flow (good for pipelines)
- `-.->` for dashed lines (error paths, async)
- Yellow for queues/intermediate states

## 3. Sequence Diagram - Service Interaction

```mermaid
sequenceDiagram
    participant Client
    participant API
    participant AuthService
    participant Database
    participant Redis

    Client->>API: POST /api/auth/login
    API->>AuthService: validateAndLogin(credentials)
    AuthService->>Database: SELECT user WHERE email=?
    Database-->>AuthService: user data
    AuthService->>AuthService: comparePassword()
    AuthService->>Redis: SETEX session:token
    Redis-->>AuthService: OK
    AuthService-->>API: { token, user }
    API-->>Client: 200 OK
```

**Usage notes:**
- `->>` for synchronous calls
- `-->>` for returns
- `participant` declares actors
- Good for showing timing and interaction

## 4. Simplified Main-Path Only

```mermaid
graph TD
    A[Login Request] --> B[Validate]
    B --> C[Find User]
    C --> D[Verify Password]
    D --> E[Generate Token]
    E --> F[Create Session]
    F --> G[Return Success]

    style A fill:#e1f5ff
    style G fill:#e1ffe1
```

**Usage notes:**
- Omit error paths for simplicity
- Focus on happy path only
- Useful for high-level overview

## 5. State Diagram - Speech Processing States

```mermaid
stateDiagram-v2
    [*] --> Draft
    Draft --> Uploading: Upload started
    Uploading --> Processing: Upload complete
    Uploading --> Failed: Upload error
    Processing --> Analyzing: Transcription done
    Analyzing --> Complete: Analysis done
    Analyzing --> Failed: Analysis error
    Failed --> Draft: Retry
    Complete --> [*]
```

**Usage notes:**
- Good for state machines
- Shows valid transitions
- Helps understand lifecycle

## 6. Class Diagram - Service Relationships

```mermaid
classDiagram
    AuthService --> DatabaseConnection
    AuthService --> CryptoUtils
    AuthService --> JWTService
    AuthService --> SessionStore
    SessionStore --> Redis

    class AuthService {
        +findUserByEmail(email)
        +generateAuthToken(user)
        +validateCredentials(email, password)
    }

    class CryptoUtils {
        +comparePassword(plain, hash)
        +hashPassword(plain)
    }
```

**Usage notes:**
- Shows component dependencies
- Good for architecture overview
- Can include method signatures

## Template Selection Guide

| User Request | Diagram Type | Pattern |
|-------------|--------------|---------|
| "Show the flow" | Flowchart | Example 1 or 4 |
| "Include errors" | Flowchart with errors | Example 1 |
| "Data pipeline" | Left-right flowchart | Example 2 |
| "Service calls" | Sequence diagram | Example 3 |
| "Simplify" | Simple flowchart | Example 4 |
| "State machine" | State diagram | Example 5 |
| "Architecture" | Class diagram | Example 6 |

## After Generation

Always provide:
1. The Mermaid code block (renders in GitHub, many terminals)
2. Link to https://mermaid.live for editing
3. Offer to modify: "simplify", "add error paths", "show as sequence diagram"
