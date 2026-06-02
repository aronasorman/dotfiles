#!/usr/bin/env node
import { execFileSync } from "node:child_process";
import fs from "node:fs";

const databaseUrl =
  process.env.TURSO_PROJECT_IDEAS_URL ||
  "libsql://project-ideas-tracker-aronasorman.aws-us-west-2.turso.io";
const onePasswordRef =
  process.env.TURSO_PROJECT_IDEAS_1P_REF ||
  "op://LocalAgents/Project Ideas Tracker Turso/credential";
const serviceAccountEnvFile =
  process.env.OP_SERVICE_ACCOUNT_ENV_FILE ||
  "/Users/aron/.config/local-agents/eclub-prod-smoke.env";
let cachedAuthToken;

function loadServiceAccountEnv(env) {
  if (env.OP_SERVICE_ACCOUNT_TOKEN || !fs.existsSync(serviceAccountEnvFile)) {
    return env;
  }
  const text = fs.readFileSync(serviceAccountEnvFile, "utf8");
  const match = text.match(/(?:export\s+)?OP_SERVICE_ACCOUNT_TOKEN=(['"]?)([^'"\n]+)\1/);
  if (!match) return env;
  return { ...env, OP_SERVICE_ACCOUNT_TOKEN: match[2] };
}

function getAuthToken() {
  if (cachedAuthToken) return cachedAuthToken;
  if (process.env.TURSO_PROJECT_IDEAS_TOKEN) {
    cachedAuthToken = process.env.TURSO_PROJECT_IDEAS_TOKEN;
    return cachedAuthToken;
  }
  const env = loadServiceAccountEnv(process.env);
  cachedAuthToken = execFileSync("op", ["read", onePasswordRef], {
    encoding: "utf8",
    env,
    stdio: ["ignore", "pipe", "pipe"],
  }).trim();
  return cachedAuthToken;
}

function httpUrl() {
  return databaseUrl.replace(/^libsql:\/\//, "https://").replace(/\/$/, "") + "/v2/pipeline";
}

function sqlValue(value) {
  if (value === null || value === undefined) return { type: "null" };
  if (typeof value === "number" && Number.isInteger(value)) {
    return { type: "integer", value: String(value) };
  }
  if (typeof value === "number") return { type: "float", value: String(value) };
  if (typeof value === "boolean") return { type: "integer", value: value ? "1" : "0" };
  return { type: "text", value: String(value) };
}

async function pipeline(requests) {
  const response = await fetch(httpUrl(), {
    method: "POST",
    headers: {
      Authorization: `Bearer ${getAuthToken()}`,
      "Content-Type": "application/json",
    },
    body: JSON.stringify({ requests: [...requests, { type: "close" }] }),
  });
  const body = await response.text();
  if (!response.ok) {
    throw new Error(`Turso HTTP ${response.status}: ${body}`);
  }
  const json = JSON.parse(body);
  for (const result of json.results ?? []) {
    if (result.type !== "ok") {
      throw new Error(`Turso query failed: ${JSON.stringify(result)}`);
    }
  }
  return json.results ?? [];
}

async function execute(sql, args = []) {
  const results = await pipeline([{ type: "execute", stmt: { sql, args: args.map(sqlValue) } }]);
  return results[0]?.response?.result;
}

function decodeValue(value) {
  if (!value || value.type === "null") return null;
  if (value.type === "integer") return Number(value.value);
  if (value.type === "float") return Number(value.value);
  if (value.type === "blob") return value.base64;
  return value.value;
}

function decodeRows(result) {
  const cols = (result?.cols ?? []).map((col) => col.name);
  return (result?.rows ?? []).map((row) =>
    Object.fromEntries(row.map((value, index) => [cols[index], decodeValue(value)])),
  );
}

async function ensureSchema() {
  await pipeline([
    {
      type: "execute",
      stmt: {
        sql: `
          CREATE TABLE IF NOT EXISTS suggestions (
            slug TEXT PRIMARY KEY,
            title TEXT NOT NULL,
            bucket TEXT NOT NULL,
            canonical_question TEXT NOT NULL,
            pitch TEXT NOT NULL,
            hook TEXT NOT NULL,
            tracks TEXT NOT NULL,
            data_spine TEXT NOT NULL,
            why_now TEXT NOT NULL,
            sources_json TEXT NOT NULL,
            source_headlines_json TEXT NOT NULL DEFAULT '[]',
            run_id TEXT,
            suggested_at TEXT NOT NULL,
            created_at TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ', 'now'))
          )
        `,
      },
    },
    {
      type: "execute",
      stmt: { sql: "CREATE UNIQUE INDEX IF NOT EXISTS suggestions_canonical_question_idx ON suggestions(canonical_question)" },
    },
  ]);
}

async function listSuggestions() {
  await ensureSchema();
  const result = await execute(`
    SELECT slug, title, bucket, canonical_question, sources_json, suggested_at
    FROM suggestions
    ORDER BY suggested_at DESC, title ASC
  `);
  console.log(JSON.stringify(decodeRows(result), null, 2));
}

function slugify(text) {
  return String(text)
    .toLowerCase()
    .replace(/[^a-z0-9]+/g, "-")
    .replace(/^-+|-+$/g, "")
    .slice(0, 96);
}

async function readStdinJson() {
  const chunks = [];
  for await (const chunk of process.stdin) chunks.push(chunk);
  const text = Buffer.concat(chunks).toString("utf8").trim();
  if (!text) throw new Error("record expects a JSON object or array on stdin");
  return JSON.parse(text);
}

async function recordSuggestions() {
  await ensureSchema();
  const input = await readStdinJson();
  const ideas = Array.isArray(input) ? input : [input];
  let inserted = 0;
  let skipped = 0;
  for (const idea of ideas) {
    const slug = idea.slug || slugify(idea.title);
    const result = await execute(
      `
        INSERT OR IGNORE INTO suggestions (
          slug, title, bucket, canonical_question, pitch, hook, tracks,
          data_spine, why_now, sources_json, source_headlines_json, run_id, suggested_at
        )
        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
      `,
      [
        slug,
        idea.title,
        idea.bucket,
        idea.canonical_question,
        idea.pitch,
        idea.hook,
        idea.tracks,
        idea.data_spine,
        idea.why_now,
        JSON.stringify(idea.sources ?? []),
        JSON.stringify(idea.source_headlines ?? []),
        idea.run_id ?? null,
        idea.suggested_at ?? new Date().toISOString(),
      ],
    );
    if ((result?.affected_row_count ?? 0) > 0) inserted += 1;
    else skipped += 1;
  }
  console.log(JSON.stringify({ inserted, skipped }, null, 2));
}

async function main() {
  const command = process.argv[2];
  if (command === "init") {
    await ensureSchema();
    console.log("TURSO_LEDGER_INIT_OK");
  } else if (command === "list") {
    await listSuggestions();
  } else if (command === "record") {
    await recordSuggestions();
  } else {
    console.error("Usage: turso-ledger.mjs init | list | record < ideas.json");
    process.exit(2);
  }
}

main().catch((error) => {
  console.error(error.message);
  process.exit(1);
});
