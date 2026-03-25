# CodeMap Example: Debugging Null Pointer Error

This example shows how to format a debugging-focused CodeMap with proper syntax highlighting.

```
CodeMap: Debug - NullPointerException in Speech Analysis
================================================================================

🐛 ERROR CONTEXT
Exception: TypeError: Cannot read property 'duration' of undefined
Location: src/analysis/speech-processor.ts:234
Triggered by: User speech analysis request (speech ID: abc123)

📍 ERROR LOCATION
└─ src/analysis/speech-processor.ts:234 - analyzeSpeech()

```typescript
// class SpeechProcessor
async analyzeSpeech(speechId: string): Promise<AnalysisResult> {
  const speech = await this.getSpeechData(speechId);
  const metrics = this.calculateMetrics(speech);

  // ❌ ERROR HERE: speech.audio is undefined
  const duration = speech.audio.duration;
  return { metrics, duration };
}
```

⚠️  ERROR: speech.audio is undefined when audio upload failed

🔄 EXECUTION TRACE LEADING TO ERROR

**Step 1: API Request Received**
└─ src/routes/speech.ts:45 - POST /api/speech/analyze

```typescript
// async function handleAnalysis(req: Request, res: Response)
async function handleAnalysis(req: Request, res: Response) {
  const { speechId } = req.params;

  // No validation that speech exists
  const result = await speechProcessor.analyzeSpeech(speechId);
  res.json(result);
}
```

🐛 ISSUE: Missing speech existence check before processing

---

**Step 2: Fetch Speech Data**
└─ src/services/speech.service.ts:89 - getSpeechData()

```typescript
// class SpeechService
async getSpeechData(speechId: string): Promise<Speech> {
  const doc = await firestore
    .collection('speeches')
    .doc(speechId)
    .get();

  return doc.data() as Speech;
}
```

🐛 ISSUE: No check if doc.exists() - returns undefined for missing docs
🐛 ISSUE: Unsafe type assertion (as Speech) hides the undefined

---

**Step 3: Calculate Metrics (ERROR OCCURS HERE)**
└─ src/analysis/speech-processor.ts:234 - analyzeSpeech()

```typescript
// class SpeechProcessor
async analyzeSpeech(speechId: string): Promise<AnalysisResult> {
  const speech = await this.getSpeechData(speechId);

  // speech is undefined or has no 'audio' property
  // ❌ ERROR HERE
  const duration = speech.audio.duration;
  const wordsPerMinute = calculateWPM(speech.transcript, duration);
  return { metrics: { wordsPerMinute }, duration };
}
```

🐛 ROOT CAUSE: No validation that speech or speech.audio exists
🐛 ASSUMPTION: Code assumes all speeches have audio, but:
   • Speeches in 'draft' state may not have uploaded audio yet
   • Failed uploads leave audio field as undefined
   • Race condition if analysis triggered before upload completes

================================================================================

🔧 ROOT CAUSE ANALYSIS

PRIMARY ISSUE:
Missing null/undefined checks at multiple levels:
1. No validation that speech document exists (Step 2)
2. Unsafe type casting hides undefined (Step 2)
3. No validation that audio field is populated (Step 3)

CONTRIBUTING FACTORS:
• API endpoint doesn't verify speech is in 'completed' state before analysis
• Frontend can trigger analysis on draft speeches
• No TypeScript strictNullChecks enabled (would catch this at compile time)

💡 RECOMMENDED FIXES

**[Priority 1] Add null checks in getSpeechData():**
└─ src/services/speech.service.ts:89

```typescript
async getSpeechData(speechId: string): Promise<Speech> {
  const doc = await firestore.collection('speeches').doc(speechId).get();

  // ADD THESE CHECKS:
  if (!doc.exists) {
    throw new NotFoundError(`Speech ${speechId} not found`);
  }

  const data = doc.data();
  if (!data) {
    throw new Error(`Speech ${speechId} has no data`);
  }

  return data as Speech;
}
```

**[Priority 2] Validate audio exists before processing:**
└─ src/analysis/speech-processor.ts:234

```typescript
async analyzeSpeech(speechId: string): Promise<AnalysisResult> {
  const speech = await this.getSpeechData(speechId);

  // ADD THIS CHECK:
  if (!speech.audio || !speech.audio.duration) {
    throw new ValidationError(
      'Speech audio not available. Upload may be incomplete.'
    );
  }

  const duration = speech.audio.duration;
  // ... rest of analysis
}
```

**[Priority 3] Add state validation in API endpoint:**
└─ src/routes/speech.ts:45

```typescript
async function handleAnalysis(req: Request, res: Response) {
  const { speechId } = req.params;

  // ADD THIS CHECK:
  const speech = await speechService.getSpeech(speechId);
  if (speech.state !== 'completed') {
    return res.status(400).json({
      error: 'Speech must be in completed state for analysis'
    });
  }

  const result = await speechProcessor.analyzeSpeech(speechId);
  res.json(result);
}
```

**[Priority 4] Enable TypeScript strict mode:**
└─ tsconfig.json

```json
{
  "compilerOptions": {
    "strictNullChecks": true,
    // ... other options
  }
}
```

================================================================================

💬 Ask me to:
• "Implement priority 1 fix" - Apply the null check fix
• "Show me test cases" - Generate test cases for these edge cases
• "Create a mermaid graph" - Visualize the error flow
• "Check for similar issues" - Find other places with this pattern
```
