# CodeMap Example: Feature Explanation

This example shows how to format a feature explanation CodeMap with proper syntax highlighting.

```
CodeMap: User Authentication Flow
================================================================================

📍 ENTRY POINT
src/routes/auth.ts:15 - POST /api/auth/login

```typescript
// express.Router
async loginHandler(req: Request, res: Response) {
  const { email, password } = req.body;

  // Validate request
  const validation = validateLoginRequest(req.body);
  if (!validation.isValid) {
    return res.status(400).json({ error: validation.error });
  }
}
```

🔄 EXECUTION FLOW

**Step 1: Request Validation**
└─ src/middleware/validate.ts:42 - validateLoginRequest()

```typescript
// function validateLoginRequest(data: LoginData): ValidationResult
function validateLoginRequest(data: LoginData): ValidationResult {
  const emailRegex = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
  if (!emailRegex.test(data.email)) {
    return { isValid: false, error: 'Invalid email format' };
  }
  if (data.password.length < 8) {
    return { isValid: false, error: 'Password too short' };
  }
}
```

💡 Checks email format via regex and minimum password length
⚠️  Returns 400 error if validation fails - flow stops here

---

**Step 2: User Lookup**
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
  💡 Always releases connection in finally block

---

**Step 3: Password Verification**
└─ src/utils/crypto.ts:23 - comparePassword()

```typescript
// class CryptoUtils
async comparePassword(plain: string, hash: string): Promise<boolean> {
  // Timing-safe comparison prevents timing attacks
  const result = await bcrypt.compare(plain, hash);

  // Constant-time delay even on mismatch
  await this.constantTimeDelay();
  return result;
}
```

💡 Uses bcrypt with 10 rounds (configured in crypto.config.ts:8)
💡 Timing-safe to prevent timing attack vulnerabilities
⚠️  Returns false on mismatch → triggers 401 error

---

**Step 4: Token Generation**
└─ src/services/auth.service.ts:145 - generateAuthToken()

```typescript
// class AuthService
async generateAuthToken(user: User): Promise<string> {
  const payload = {
    userId: user.id,
    role: user.role,
    iat: Date.now()
  };
  const token = jwt.sign(payload, this.jwtSecret, {
    expiresIn: '24h'
  });
  return token;
}
```

💡 Token includes user ID and role for authorization
💡 24-hour expiration configured here

  **→ Calls:** src/utils/jwt.ts:67 - sign()

  ```typescript
  // function sign(payload: object, secret: string, options: SignOptions)
  function sign(payload: object, secret: string, options: SignOptions) {
    return jsonwebtoken.sign(payload, secret, {
      algorithm: 'HS256',
      ...options
    });
  }
  ```

  💡 Uses HMAC SHA-256 algorithm
  💡 Secret rotates monthly via ops/rotate-secrets.sh

  **→ Calls:** src/database/sessions.ts:89 - createSession()

  ```typescript
  // class SessionStore
  async createSession(userId: string, token: string): Promise<void> {
    await this.redis.setex(
      `session:${token}`,
      86400, // 24 hours in seconds
      JSON.stringify({ userId, createdAt: Date.now() })
    );
  }
  ```

  💡 Stored in Redis with 24h TTL matching JWT expiration
  💡 Session key format: "session:{token}" for quick lookup

================================================================================

📊 KEY COMPONENTS
• Entry Point:    src/routes/auth.ts:15 (Express route handler)
• Validation:     src/middleware/validate.ts:42 (Input sanitization)
• Business Logic: src/services/auth.service.ts:89, :145 (Core auth operations)
• Data Access:    src/database/users.ts:156 (PostgreSQL queries)
• Session Store:  src/database/sessions.ts:89 (Redis operations)
• Utilities:      src/utils/crypto.ts:23, src/utils/jwt.ts:67 (Security)

🔍 CRITICAL DECISIONS
• Password hashing: bcrypt (10 rounds) with timing-safe comparison
  └─ config/crypto.config.ts:8
• Session storage: Redis with 24h TTL matching JWT expiration
  └─ config/redis.ts:12
• Token algorithm: JWT with HS256, secret rotation monthly
  └─ config/jwt.ts:8
• Email normalization: Lowercase before DB query to prevent duplicates
  └─ src/services/auth.service.ts:92

⚠️  ERROR PATHS
• Invalid email/password format → 400 (Step 1)
• User not found → 401 (Step 2)
• Password mismatch → 401 (Step 3)
• Redis connection failure → 500 (Step 4, logged to error service)

💡 ENTRY POINTS FOR MODIFICATION
• Add MFA support → Insert after Step 3 (password verification)
  └─ Create new service at src/services/mfa.service.ts
• Change session duration → Update both:
  └─ src/services/auth.service.ts:149 (JWT expiry)
  └─ src/database/sessions.ts:92 (Redis TTL)
• Add rate limiting → Insert before Step 1
  └─ Create middleware at src/middleware/rate-limit.ts

================================================================================

💬 Ask me to:
• "Expand step 3" - Show more code context for password verification
• "Show error handling" - Map all error paths in detail
• "Create a mermaid graph" - Generate visual diagram of this flow
```
