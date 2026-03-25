---
name: using-datastar
description: Use when building hypermedia UIs with Datastar v1.0 - covers attribute syntax ($signals, @actions), SSE response format, Go SDK patterns, and common mistakes from HTMX/Alpine mental models
---

# Using Datastar

## Overview

Datastar is a hypermedia framework (10KB) combining frontend reactivity (like Alpine) with backend-driven updates (like HTMX) via Server-Sent Events. Key insight: the server pushes DOM updates through SSE streams, not request/response JSON.

## Installation

```html
<script type="module" src="https://cdn.jsdelivr.net/gh/starfederation/[email protected]/bundles/datastar.js"></script>
```

## Core Syntax

### Signals (State)

Prefix with `$` in expressions. Define with `data-signals`:

```html
<div data-signals:count="0">
  <span data-text="$count"></span>
  <button data-on:click="$count++">+1</button>
</div>
```

Nested: `data-signals:user.name="''"` or `data-signals="{user: {name: '', age: 0}}"`.

### Actions (Backend Calls)

Use `@verb(url)` syntax:

```html
<button data-on:click="@get('/api/data')">Load</button>
<button data-on:click="@post('/api/save')">Save</button>
<button data-on:click="@delete('/api/item')">Delete</button>
```

All signals automatically sent. Filter with options: `@get('/api', {filterSignals: {include: /^form/}})`.

### Event Modifiers

Double-underscore syntax with dot parameters:

```html
<input data-on:input__debounce.300ms="@get('/search')">
<button data-on:click__once="@post('/submit')">
<div data-on:click__outside="$open = false">
```

Common: `__debounce.Xms`, `__throttle.Xms`, `__once`, `__prevent`, `__stop`, `__window`, `__outside`.

## Attributes Quick Reference

| Attribute | Purpose | Example |
|-----------|---------|---------|
| `data-signals` | Define reactive state | `data-signals:foo="1"` |
| `data-computed` | Derived state (read-only) | `data-computed:full="$first+' '+$last"` |
| `data-text` | Bind text content | `data-text="$foo"` |
| `data-bind` | Two-way form binding | `data-bind:query` |
| `data-show` | Toggle visibility (CSS) | `data-show="$loading"` |
| `data-class` | Conditional classes | `data-class:active="$selected"` |
| `data-on` | Event handlers | `data-on:click="@post('/x')"` |
| `data-indicator` | Loading state signal | `data-indicator:fetching` |
| `data-attr` | Dynamic attributes | `data-attr:disabled="$busy"` |
| `data-ref` | DOM element reference | `data-ref:myEl` then `$myEl.focus()` |

## SSE Response Format (Critical)

Datastar expects `text/event-stream`, not HTML or JSON. Server pushes events:

**Patch elements:**
```
event: datastar-patch-elements
data: elements <div id="results">New content</div>

```

**Patch signals:**
```
event: datastar-patch-signals
data: signals {"count": 5}

```

**Remove elements:**
```
event: datastar-patch-elements
data: mode remove
data: selector #old-item

```

**Patch modes:** `morph` (default), `inner`, `outer`, `prepend`, `append`, `before`, `after`, `remove`.

Note: blank line after each event block.

## Go SDK

```go
import "github.com/starfederation/datastar-go/datastar"

func handler(w http.ResponseWriter, r *http.Request) {
    // Read signals from request
    var store MyStore
    if err := datastar.ReadSignals(r, &store); err != nil {
        http.Error(w, err.Error(), http.StatusBadRequest)
        return
    }

    // Create SSE writer
    sse := datastar.NewSSE(w, r)

    // Send HTML fragment (patches by ID match)
    sse.PatchElements(`<div id="result">Updated!</div>`)

    // Update signals
    sse.MarshalAndPatchSignals(map[string]any{"count": 42})

    // Execute script
    sse.ExecuteScript(`console.log('done')`)
}
```

### With Templ

```go
func handler(w http.ResponseWriter, r *http.Request) {
    sse := datastar.NewSSE(w, r)
    sse.PatchElementTempl(MyComponent(data))
}
```

## Complete Example: Counter

**HTML:**
```html
<div data-signals:count="0">
  <p>Count: <span data-text="$count"></span></p>
  <button data-on:click="@post('/increment')">+1</button>
</div>
```

**Go:**
```go
type Store struct {
    Count int `json:"count"`
}

func incrementHandler(w http.ResponseWriter, r *http.Request) {
    var store Store
    datastar.ReadSignals(r, &store)
    store.Count++

    sse := datastar.NewSSE(w, r)
    sse.MarshalAndPatchSignals(store)
}
```

## Complete Example: Search with Debounce

**HTML:**
```html
<div data-signals:query="''">
  <input
    data-bind:query
    data-on:input__debounce.300ms="@get('/search')"
    data-indicator:searching
  >
  <span data-show="$searching">Loading...</span>
  <div id="results"></div>
</div>
```

**Go:**
```go
func searchHandler(w http.ResponseWriter, r *http.Request) {
    var store struct {
        Query string `json:"query"`
    }
    datastar.ReadSignals(r, &store)

    results := search(store.Query)

    sse := datastar.NewSSE(w, r)
    sse.PatchElements(renderResults(results))
}
```

## Common Mistakes

| Wrong | Right | Why |
|-------|-------|-----|
| `data-store` | `data-signals` | Different framework |
| `data-on-click` | `data-on:click` | Colon separates event |
| `count` in expr | `$count` | Signals need $ prefix |
| `$post('/x')` | `@post('/x')` | Actions use @ prefix |
| `__debounce_300` | `__debounce.300ms` | Dot separates param, include unit |
| Return JSON | Return SSE | Must use text/event-stream |
| `http.ResponseWriter` | `datastar.NewSSE(w, r)` | Need SSE helper |
| `MergeFragments` | `PatchElements` | SDK uses "Patch" not "Merge" |
| `MergeSignals` | `MarshalAndPatchSignals` | SDK uses "Patch" terminology |
