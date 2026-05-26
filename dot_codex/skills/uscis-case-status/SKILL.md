---
name: uscis-case-status
description: Use when a user asks to check, look up, poll, or summarize a USCIS Case Status Online receipt number such as MSC2190012345, EAC2290098765, or IOE0123456789.
---

# USCIS Case Status

Use browser automation to check the public USCIS Case Status Online portal. This is read-only: never log in, subscribe, click "Login", click "Sign up for Case Status Online", or mutate a USCIS account.

This skill is adapted from the Browse.sh `uscis.gov/check-case-status-or94c8` skill and the AutoBrowse iteration pattern: run a browser attempt, capture the exact blocked turn, update the site-specific strategy, and graduate only reliable steps into the final skill. Prefer the open-source `browse` CLI because it works from both Claude and Codex through shell commands. Fall back to the current agent's native browser only when `browse` is unavailable.

## Input Handling

Normalize by uppercasing and removing spaces or hyphens. A normal receipt must match:

```text
^[A-Z]{3}\d{10}$
```

Reject invalid input before opening a browser. For historical `IOE` strings longer than 13 characters, truncate only if all extra trailing characters are zeroes; otherwise reject as malformed.

Treat USCIS receipt numbers as sensitive immigration identifiers. If the user provided the specific receipt number and asked to check USCIS, that is enough approval to submit it to `egov.uscis.gov` from a local browser. If the receipt came from a third-party page, file, or inferred context, confirm with the user before typing or submitting it.

Do not use Browserbase remote/cloud mode unless the user explicitly approves sending the receipt through Browserbase infrastructure. Local `browse` mode submits only to USCIS; remote Browserbase mode also exposes the receipt to Browserbase-hosted browser infrastructure.

## Service Center Map

Derive `service_center_code` and `service_center_name` locally from the first three letters.

| Prefix | Service center |
|---|---|
| `EAC` | Vermont Service Center |
| `WAC` | California Service Center |
| `LIN` | Nebraska Service Center |
| `SRC` | Texas Service Center |
| `MSC` | National Benefits Center |
| `NBC` | National Benefits Center |
| `NSC` | Nebraska Service Center |
| `YSC` | Potomac Service Center |
| `IOE` | USCIS Electronic Immigration System (ELIS) |

Do not infer form type from the prefix.

## Tool Selection

1. If `command -v browse` succeeds, use `browse` CLI.
2. If `browse` is missing but the agent has a native browser tool, use that tool with the same page, validation, extraction, and safety rules.
3. If neither is available, return `success: false` and explain that a browser automation surface is required.

`browse` CLI is open source and runs local Chrome/Chromium. If installation is requested, the current upstream install command is:

```bash
npm install -g browse
```

The installed command is `browse`.

## Browse CLI Workflow

Use local mode by default:

```bash
browse stop
browse doctor --local
browse open "https://egov.uscis.gov/?localeLang=en" --local --headless --wait load --timeout 60000
browse wait timeout 2500
browse snapshot --compact
```

The snapshot should show a receipt textbox, often labelled "Enter Another Receipt Number" or using placeholder `EAC1234567890`, and a `Check Status` button.

If the page shows a Cloudflare wall, Turnstile, CAPTCHA, "Just a moment...", or other human-verification barrier, stop. Do not solve CAPTCHA or bypass the barrier. Return a bounded failure, or hand the browser to the user if they want to complete the challenge.

Also detect hidden Cloudflare challenge instrumentation before typing any receipt. USCIS can render the visible form while injecting a hidden 1x1 `/cdn-cgi/challenge-platform/` frame/script. A visible textbox and `Check Status` button do not clear this gate.

```bash
browse eval '(() => { const html = document.documentElement.outerHTML; const frameData = Array.from(document.querySelectorAll("iframe")).map(f => ({src: f.src, width: f.getAttribute("width") || f.style.width, height: f.getAttribute("height") || f.style.height, hidden: f.hidden || getComputedStyle(f).display === "none" || getComputedStyle(f).visibility === "hidden"})); return {hasCloudflareChallenge: html.includes("__CF$cv$params") || html.includes("/cdn-cgi/challenge-platform/") || frameData.some(f => String(f.src).includes("/cdn-cgi/challenge-platform/")), frames: frameData}; })()'
```

If `hasCloudflareChallenge` is true, stop before entering the receipt and return a bounded Cloudflare challenge failure.

Also check hydration before submitting. USCIS may serve the HTML shell while returning 403 for the Next.js script chunks; in that state the receipt field can accept text but the React-controlled `Check Status` button remains disabled forever.

```bash
browse eval '(() => { const chunks = performance.getEntriesByType("resource").filter(e => e.name.includes("_next/static/chunks")).map(e => ({name: e.name.split("/").pop(), status: e.responseStatus || 0})); const input = document.querySelector("input[name=receipt_number], #receipt_number"); const button = Array.from(document.querySelectorAll("button")).find(b => b.innerText.trim() === "Check Status"); return {readyState: document.readyState, url: location.href, chunkCount: chunks.length, failedChunks: chunks.filter(e => e.status >= 400 || e.status === 0), hasReceiptInput: !!input, buttonDisabled: button ? button.disabled : null}; })()'
```

If `failedChunks` is non-empty, `hasReceiptInput` is false, or the page is visibly unstyled, return a bounded Cloudflare/hydration failure before filling the receipt. Do not treat the visible form alone as proof that the SPA is ready.

Fill and submit using refs from the current snapshot, not guessed refs:

```bash
browse click @<receipt_textbox_ref>
browse type "$RECEIPT" --delay 25
browse wait timeout 500
browse eval '({value: document.querySelector("input[name=receipt_number]")?.value, disabled: Array.from(document.querySelectorAll("button")).find(b => b.innerText.trim() === "Check Status")?.disabled})'
browse click @<check_status_button_ref>
browse wait timeout 4000
browse snapshot --compact
```

Before clicking, confirm the button is enabled. If it remains disabled while the input value is 13 characters, treat that as a hydration failure and check chunk response statuses. The page usually updates in place rather than navigating, so wait for the result container to show a status heading, not-found text, validation text, or a Cloudflare/challenge state.

Known current automation failures:

- Local `browse` 0.7.3 with Chrome 148 can load the USCIS HTML shell in both headless and headed mode, but its `_next/static/chunks` resources return `403`. In that state snapshots show the receipt textbox and disabled `Check Status` button; typing `EAC9999999999` changes the input value but the button stays disabled. Return a bounded hydration failure instead of retrying blind interactions.
- A later AutoBrowse run rendered the visible USCIS form but exposed `window.__CF$cv$params` and a hidden 1x1 `/cdn-cgi/challenge-platform/scripts/jsd/main.js` iframe/script. That is a Cloudflare challenge state even when no full-page wall is visible. Stop before receipt entry.
- Computer Use against the user's normal Google Chrome profile rendered the same unstyled shell. Real keyboard input changed the textbox value to `EAC9999999999`, but `Check Status` stayed disabled; a normal Chrome reload reset the field and kept the page unstyled. Computer Use is useful for visual confirmation or user handoff, but it is not currently a bypass for this failure mode.

Extract the status heading and status-description paragraph from the case-status result container. Use scoped extraction or the relevant snapshot section; do not dump broad body text as the primary method.

## Native Browser Fallback

1. Use the current agent's native browser automation. In Codex, use the `browser` skill and the in-app Browser plugin. Do not use plain `curl`, `web.open`, or HTTP fetches for the final lookup because the status is rendered by a client-side SPA and the backing API is Cloudflare-protected.
2. Open `https://egov.uscis.gov/?localeLang=en`.
3. Wait for load plus hydration, then inspect the DOM snapshot or screenshot.
4. If the page shows a Cloudflare wall, Turnstile, CAPTCHA, "Just a moment...", or other human-verification barrier, stop. Also inspect for hidden `/cdn-cgi/challenge-platform/` frames/scripts or `__CF$cv$params`; if present, stop before typing any receipt. Do not solve CAPTCHA or bypass the barrier. Return a bounded failure or hand the browser to the user if they want to complete the challenge.
5. If using Computer Use against the user's Chrome, first visually confirm the page is styled and app-like. If it is the plain unstyled shell, stop before submission; typing through real keyboard events has been tested and still leaves `Check Status` disabled.
6. Locate the receipt textbox by visible label or placeholder `EAC1234567890`, and locate the `Check Status` button from the current DOM snapshot.
7. Fill the normalized receipt. Confirm the button is enabled before clicking. If it remains disabled after 13 characters, treat the page as unhydrated or blocked; re-snapshot once, then stop with a bounded failure if the DOM state is unchanged.
8. Click `Check Status`. The page usually updates in place rather than navigating. Wait for the result container to show a status heading, not-found text, validation text, or a Cloudflare/challenge state.
9. Extract the status heading and the status-description paragraph from the case-status result container. Use scoped extraction, not a broad body dump.

## Extraction

Status-description text is the source of truth for `form_type` and `last_updated`.

```text
form_type: /\bForm (I-\d{3}[A-Z]?|N-\d{3}|G-\d{3}|FOIA|EOIR-\d+)\b/
last_updated: /^On ([A-Z][a-z]+ \d{1,2}, \d{4}),/m
```

If the form type is absent, return `form_type: null`; do not guess. If the page says the receipt cannot be found, treat it as a legitimate lookup result, not a tool error.

Construct the canonical URL locally:

```text
https://egov.uscis.gov/?caseId=<RECEIPT>&localeLang=en
```

This URL is bookmarkable, but a server-side GET only returns the generic SPA shell; it does not contain status text.

## Output Shape

Return a concise summary plus this structured shape when practical:

```json
{
  "success": true,
  "receipt_number": "MSC2190012345",
  "service_center_code": "MSC",
  "service_center_name": "National Benefits Center",
  "status": "Case Was Received",
  "status_description": "On January 15, 2024, ...",
  "form_type": "I-130",
  "last_updated": "January 15, 2024",
  "case_status_url": "https://egov.uscis.gov/?caseId=MSC2190012345&localeLang=en",
  "error_reasoning": null
}
```

For invalid input, set `success: false`, leave status fields null, and explain the expected format. For Cloudflare, CAPTCHA, or hydration failures, set `success: false`, keep the canonical URL when receipt format was valid, and explain that browser automation could not complete the read-only lookup without human challenge completion or an approved different browser path. A hidden Cloudflare failure should say that the USCIS form rendered, but a hidden `/cdn-cgi/challenge-platform/` frame/script or `__CF$cv$params` marker was present, so the receipt was not entered. A hydration failure should say that the USCIS page shell loaded, but the client runtime did not hydrate; `Check Status` stayed disabled after a 13-character receipt, and Next.js chunks were blocked or unavailable.
