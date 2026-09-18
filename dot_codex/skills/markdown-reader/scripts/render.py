#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.11"
# dependencies = [
#     "markdown-it-py==3.0.0",
#     "mdit-py-plugins==0.4.2",
# ]
# ///
"""Render an existing Markdown file into the self-contained reader shell.

Faithful by design: the parser decides structure, this script only adds anchors,
a contents list, frontmatter-as-metadata, table-to-labeled-entries, and local
link resolution. Nothing is reworded, reordered, summarised, or dropped.

    uv run --script scripts/render.py SOURCE.md --output OUT.html [--force]
"""

import argparse
import hashlib
import html
import json
import re
import sys
from pathlib import Path
from urllib.parse import unquote, urlsplit

from markdown_it import MarkdownIt
from markdown_it.token import Token
from mdit_py_plugins.footnote import footnote_plugin

TEMPLATE = Path(__file__).resolve().parent.parent / "assets" / "reader.html"
PLACEHOLDER = re.compile(r"\{\{[A-Z_]+\}\}")


# --- frontmatter -------------------------------------------------------------


def split_frontmatter(text):
    """Return (frontmatter lines, body). Frontmatter never reaches the body."""
    lines = text.split("\n")
    if not lines or lines[0].strip() != "---":
        return [], text
    for i in range(1, len(lines)):
        if lines[i].strip() in ("---", "..."):
            return lines[1:i], "\n".join(lines[i + 1 :])
    return [], text


def parse_frontmatter(lines):
    """Order-preserving key -> value lines. Deliberately not a YAML loader:
    unparsed structure is kept verbatim under its key rather than reshaped."""
    entries = []
    for raw in lines:
        if not raw.strip():
            continue
        match = re.match(r"^([A-Za-z0-9_.\-]+):\s*(.*)$", raw)
        if match and not raw[:1].isspace():
            value = match.group(2).strip()
            entries.append((match.group(1), [value] if value else []))
        elif entries:
            entries[-1][1].append(raw.rstrip())
        else:
            entries.append(("", [raw.rstrip()]))
    return entries


def frontmatter_html(entries):
    if not entries:
        return ""
    rows = []
    for key, values in entries:
        label = html.escape(key) if key else "&mdash;"
        rows.append(f"<dt>{label}</dt><dd>{html.escape(chr(10).join(values))}</dd>")
    return (
        '<details class="meta"><summary>Source frontmatter</summary><dl>'
        + "".join(rows)
        + "</dl></details>"
    )


# --- headings ----------------------------------------------------------------


def heading_text(inline):
    return "".join(
        child.content
        for child in (inline.children or [])
        if child.type in ("text", "code_inline")
    ).strip()


def slugify(text):
    """GitHub-style heading slug, so links already written against this document
    keep working: lowercase, punctuation dropped (not hyphenated), whitespace to
    hyphens, non-ASCII letters preserved. "API: HTTP/2" -> "api-http2"."""
    slug = re.sub(r"[^\w\s-]", "", text.strip().lower(), flags=re.UNICODE)
    return re.sub(r"\s", "-", slug) or "section"


def add_anchors(tokens):
    """Give every heading a unique id and collect the contents list."""
    toc = []
    used = set()
    for i, token in enumerate(tokens):
        if token.type != "heading_open":
            continue
        text = heading_text(tokens[i + 1])
        base = slugify(text)
        slug, n = base, 0
        while slug in used:
            n += 1
            slug = f"{base}-{n}"
        used.add(slug)
        token.attrSet("id", slug)
        level = int(token.tag[1])
        if level <= 4 and text:
            toc.append((level, slug, text))
    return toc


# --- local links and images --------------------------------------------------


def resolve_local_targets(tokens, base_dir):
    """Point relative links/images at resolved file:// paths so they still work
    from the generated page. External URLs are never rewritten."""
    count = 0
    for token in tokens:
        if token.type != "inline":
            continue
        for child in token.children or []:
            attr = {"link_open": "href", "image": "src"}.get(child.type)
            if attr:
                count += resolve_attr(child, attr, base_dir)
    return count


def resolve_attr(token, attr, base_dir):
    value = token.attrGet(attr)
    if not value:
        return 0
    parts = urlsplit(value)
    if parts.scheme or parts.netloc or not parts.path:
        return 0  # absolute URL, protocol-relative, or in-page anchor
    path = Path(unquote(parts.path))
    target = path if path.is_absolute() else base_dir / path
    uri = target.resolve().as_uri()
    if parts.query:
        uri += "?" + parts.query
    if parts.fragment:
        uri += "#" + parts.fragment
    token.attrSet(attr, uri)
    return 1


# --- tables as labeled entries ----------------------------------------------


def transform_tables(md, tokens, env):
    """Replace table tokens with labeled header/value entries. Every header and
    every cell is kept, and each value stays attached to its column name."""
    out = []
    i = 0
    while i < len(tokens):
        if tokens[i].type != "table_open":
            out.append(tokens[i])
            i += 1
            continue
        j = i
        while tokens[j].type != "table_close":
            j += 1
        out.append(table_entries_token(md, tokens[i : j + 1], env))
        i = j + 1
    return out


def table_entries_token(md, block, env):
    rows, section, current = [], "body", None
    for token in block:
        if token.type == "thead_open":
            section = "head"
        elif token.type == "tbody_open":
            section = "body"
        elif token.type == "tr_open":
            current = []
        elif token.type in ("th_open", "td_open") and current is not None:
            current.append("")
        elif token.type == "inline" and current:
            current[-1] = md.renderer.renderInline(token.children or [], md.options, env)
        elif token.type == "tr_close" and current is not None:
            rows.append((section, current))
            current = None

    headers = next((cells for section, cells in rows if section == "head"), [])
    body = [cells for section, cells in rows if section != "head"]

    def field(index, value):
        label = headers[index] if index < len(headers) and headers[index].strip() else f"Column {index + 1}"
        return (
            f'<div class="field"><div class="key">{label}</div>'
            f'<div class="val">{value.strip() or "&mdash;"}</div></div>'
        )

    parts = ['<div class="table-entries">']
    if body:
        for cells in body:
            parts.append('<div class="entry">')
            parts.extend(field(index, cell) for index, cell in enumerate(cells))
            parts.append("</div>")
    elif headers:
        # Header-only table: keep the column names rather than dropping the block.
        parts.append('<div class="entry">')
        parts.extend(
            f'<div class="field"><div class="key">Column {index + 1}</div>'
            f'<div class="val">{cell.strip() or "&mdash;"}</div></div>'
            for index, cell in enumerate(headers)
        )
        parts.append("</div>")
    parts.append("</div>")

    return Token("html_block", "", 0, content="".join(parts), block=True)


# --- page assembly -----------------------------------------------------------


def document_title(entries, tokens, source):
    for key, values in entries:
        if key.lower() == "title" and values:
            return values[0].strip().strip("\"'")
    for i, token in enumerate(tokens):
        if token.type == "heading_open" and token.tag == "h1":
            text = heading_text(tokens[i + 1])
            if text:
                return text
    return source.stem


def fill(template, values):
    names = {match.group(0)[2:-2] for match in PLACEHOLDER.finditer(template)}
    missing = names - set(values)
    if missing:
        sys.exit(f"template placeholder(s) without a value: {sorted(missing)}")
    # Single pass: replacements are not rescanned, so document text that happens
    # to look like a placeholder is left alone.
    return PLACEHOLDER.sub(lambda m: values[m.group(0)[2:-2]], template)


def main():
    parser = argparse.ArgumentParser(description="Render Markdown into the reader shell.")
    parser.add_argument("source", type=Path, help="existing Markdown file to render")
    parser.add_argument("--output", type=Path, required=True, help="HTML file to write")
    parser.add_argument(
        "--force",
        action="store_true",
        help="overwrite an existing output file (only when explicitly requested)",
    )
    args = parser.parse_args()

    source = args.source.expanduser().resolve()
    output = args.output.expanduser()
    if not source.is_file():
        sys.exit(f"source not found: {source}")
    if output.resolve() == source:
        # Not overridable by --force: the source must survive every render.
        sys.exit(f"refusing to write over the source file: {source}")
    if output.exists() and not args.force:
        sys.exit(
            f"refusing to overwrite existing file: {output}\n"
            "Re-run with --force only if replacing it was explicitly requested."
        )

    text = source.read_text(encoding="utf-8")
    front_lines, body_text = split_frontmatter(text)
    entries = parse_frontmatter(front_lines)

    md = MarkdownIt("commonmark", {"html": False, "linkify": False, "typographer": False})
    md.enable(["table", "strikethrough"])
    md.use(footnote_plugin)

    env = {}
    tokens = md.parse(body_text, env)
    toc = add_anchors(tokens)
    local_targets = resolve_local_targets(tokens, source.parent)
    body_html = md.renderer.render(transform_tables(md, tokens, env), md.options, env)

    page = fill(
        TEMPLATE.read_text(encoding="utf-8"),
        {
            "DOC_TITLE": html.escape(document_title(entries, tokens, source)),
            "PREF_KEY": json.dumps(
                "mdr:" + hashlib.sha256(str(source).encode("utf-8")).hexdigest()[:16] + ":"
            ),
            "TOC_ITEMS": "".join(
                f'<li class="lvl-{level}"><a href="#{slug}">{html.escape(text)}</a></li>'
                for level, slug, text in toc
            ),
            "META_BLOCK": frontmatter_html(entries),
            "BODY": body_html,
            "SOURCE_NOTE": html.escape(source.name),
        },
    )

    output.parent.mkdir(parents=True, exist_ok=True)
    output.write_text(page, encoding="utf-8")
    print(f"wrote {output} ({len(page)} bytes, {len(toc)} contents entries)")
    if local_targets:
        print(
            f"note: {local_targets} relative link/image target(s) resolved to local file:// paths. "
            "They load only when this HTML file is opened directly from disk on a machine that has "
            "those files; serving the page over HTTP blocks file:// subresources."
        )


if __name__ == "__main__":
    main()
