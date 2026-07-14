#!/usr/bin/env python3
"""Validate fix-explainer evidence before rendering a static page."""

import argparse
import hashlib
import html
import json
import os
import re
import tempfile
from html.parser import HTMLParser
from pathlib import Path
from typing import Any, Dict, List, Optional, Sequence, Set, Tuple


MANIFEST_VERSION = 1
ROOT_REQUIRED_FIELDS = (
    "version",
    "title",
    "problem",
    "context",
    "evidence",
    "proposed_fix",
    "verification",
    "takeaway",
)
VERIFICATION_STATUSES = ("verified", "not_run", "unsupported")
MIN_EVIDENCE_ITEMS = 1
MAX_EVIDENCE_ITEMS = 5
MIN_LINE_NUMBER = 1
MAX_EXCERPT_LINES = 24
MAX_SMIG_WORDS = 90

PROBLEM_FIELDS = ("symptom", "expected", "diagnosis")
PROBLEM_OPTIONAL_FIELDS = ("bridges",)
PROBLEM_BRIDGE_FIELDS = ("label", "body")
MAX_PROBLEM_BRIDGES = 3
CONTEXT_FIELDS = ("file_role", "flow")
EVIDENCE_FIELDS = (
    "path",
    "source_sha256",
    "start_line",
    "end_line",
    "focus_lines",
    "situation",
    "mechanism",
    "implication",
    "gotcha",
)
VERIFICATION_FIELDS = ("claim", "source", "result", "status")
PROPOSED_FIX_BASE_FIELDS = (
    "path",
    "start_line",
    "end_line",
    "rationale",
    "unchanged_behavior",
    "risks",
)
PROPOSED_FIX_CODE_FIELDS = ("replacement", "unified_diff")
SHA256_PATTERN = re.compile(r"^[0-9a-f]{64}$")
TEMPLATE_PLACEHOLDERS = (
    "TITLE",
    "PROBLEM",
    "CONTEXT",
    "EVIDENCE",
    "PROPOSED_FIX",
    "VERIFICATION",
    "TAKEAWAY",
)
TEMPLATE_TOKEN_PATTERN = re.compile(r"{{([^{}\r\n]+)}}")
EXPECTED_CSP = (
    "default-src 'none'; style-src 'unsafe-inline'; img-src data:; "
    "base-uri 'none'; form-action 'none'"
)
PROHIBITED_TEMPLATE_TAGS = {"script", "iframe", "object", "embed", "link"}
STATUS_LABELS = {
    "verified": "verified",
    "not_run": "not run",
    "unsupported": "unsupported",
}


class ValidationError(ValueError):
    """Raised when a manifest cannot be tied safely to observed source."""


class _TemplateSafetyParser(HTMLParser):
    def __init__(self) -> None:
        super().__init__()
        self.csp_values: List[Optional[str]] = []
        self.in_head = False

    def _validate_start_tag(
        self, tag: str, attrs: List[Tuple[str, Optional[str]]]
    ) -> None:
        if tag in PROHIBITED_TEMPLATE_TAGS:
            raise ValidationError(f"template: {tag} elements are not allowed")

        for name, value in attrs:
            if name.startswith("on"):
                raise ValidationError(
                    f"template: event-handler attribute {name} is not allowed"
                )
            if name == "src" and not (value or "").lower().startswith("data:"):
                raise ValidationError("template: src attributes must use data: URLs")
            if name == "href" and not (value or "").startswith("#"):
                raise ValidationError(
                    "template: href attributes must be fragment-only"
                )

        http_equiv_values = [
            value for name, value in attrs if name == "http-equiv"
        ]
        if http_equiv_values:
            if len(http_equiv_values) != 1 or tag != "meta":
                raise ValidationError(
                    "template: http-equiv is allowed only once on a meta element"
                )
            if (http_equiv_values[0] or "").lower() != (
                "content-security-policy"
            ):
                raise ValidationError(
                    "template: only Content-Security-Policy http-equiv is allowed"
                )
            if not self.in_head:
                raise ValidationError(
                    "template: Content-Security-Policy meta must be inside head"
                )
            content_values = [value for name, value in attrs if name == "content"]
            if len(content_values) != 1:
                raise ValidationError(
                    "template: Content-Security-Policy meta requires one content attribute"
                )
            self.csp_values.append(content_values[0])

    def handle_starttag(
        self, tag: str, attrs: List[Tuple[str, Optional[str]]]
    ) -> None:
        if tag == "head":
            self.in_head = True
        elif tag == "body":
            self.in_head = False
        self._validate_start_tag(tag, attrs)

    def handle_startendtag(
        self, tag: str, attrs: List[Tuple[str, Optional[str]]]
    ) -> None:
        self._validate_start_tag(tag, attrs)

    def handle_endtag(self, tag: str) -> None:
        if tag == "head":
            self.in_head = False


def _require_mapping(value: Any, field: str) -> Dict[str, Any]:
    if not isinstance(value, dict):
        raise ValidationError(f"{field}: expected an object")
    return value


def _require_exact_keys(
    value: Any,
    required: Sequence[str],
    field: str,
    optional: Sequence[str] = (),
) -> Dict[str, Any]:
    mapping = _require_mapping(value, field)
    required_keys = set(required)
    allowed_keys = required_keys | set(optional)
    actual_keys = set(mapping)
    missing = sorted(required_keys - actual_keys)
    unknown = sorted(actual_keys - allowed_keys)
    if missing:
        raise ValidationError(f"{field}: missing required fields: {', '.join(missing)}")
    if unknown:
        raise ValidationError(f"{field}: unknown fields: {', '.join(unknown)}")
    return mapping


def _require_nonempty_string(value: Any, field: str) -> str:
    if not isinstance(value, str) or not value.strip():
        raise ValidationError(f"{field}: expected a non-empty string")
    return value


def _require_integer(value: Any, field: str, minimum: int = MIN_LINE_NUMBER) -> int:
    if isinstance(value, bool) or not isinstance(value, int):
        raise ValidationError(f"{field}: expected an integer")
    if value < minimum:
        raise ValidationError(f"{field}: must be at least {minimum}")
    return value


def _require_string_list(
    value: Any, field: str, *, require_nonempty: bool
) -> List[str]:
    if not isinstance(value, list):
        raise ValidationError(f"{field}: expected an array")
    if require_nonempty and not value:
        raise ValidationError(f"{field}: must contain at least one item")
    for index, item in enumerate(value):
        _require_nonempty_string(item, f"{field}[{index}]")
    return value


def _require_line_list(value: Any, field: str) -> List[int]:
    if not isinstance(value, list):
        raise ValidationError(f"{field}: expected an array")
    seen: Set[int] = set()
    for index, item in enumerate(value):
        line = _require_integer(item, f"{field}[{index}]")
        if line in seen:
            raise ValidationError(f"{field}: duplicate line number {line}")
        seen.add(line)
    return value


def _clone_json_value(value: Any) -> Any:
    if isinstance(value, dict):
        return {key: _clone_json_value(item) for key, item in value.items()}
    if isinstance(value, list):
        return [_clone_json_value(item) for item in value]
    return value


def _resolve_source(root: Path, relative_path: str, field: str) -> Path:
    path = Path(relative_path)
    if path.is_absolute():
        raise ValidationError(f"{field}: absolute paths are not allowed: {relative_path}")
    if ".." in path.parts:
        raise ValidationError(f"{field}: '..' path segments are not allowed: {relative_path}")
    try:
        candidate = (root / relative_path).resolve(strict=True)
    except (OSError, RuntimeError, ValueError) as exc:
        raise ValidationError(f"{field}: source file does not exist: {relative_path}") from exc
    try:
        candidate.relative_to(root)
    except ValueError as exc:
        raise ValidationError(
            f"{field} escapes repository root: {relative_path}"
        ) from exc
    if not candidate.is_file():
        raise ValidationError(f"{field}: source is not a file: {relative_path}")
    return candidate


def _read_source(candidate: Path, relative_path: str, expected_digest: str) -> List[str]:
    try:
        raw_source = candidate.read_bytes()
    except OSError as exc:
        raise ValidationError(f"evidence.path: cannot read source: {relative_path}") from exc
    actual_digest = hashlib.sha256(raw_source).hexdigest()
    if actual_digest != expected_digest:
        raise ValidationError(
            "evidence.source_sha256: source changed for "
            f"{relative_path}; expected {expected_digest}, got {actual_digest}"
        )
    try:
        return raw_source.decode("utf-8").splitlines()
    except UnicodeDecodeError as exc:
        raise ValidationError(
            f"evidence.path: source is not valid UTF-8: {relative_path}"
        ) from exc


def _validate_evidence_item(
    item: Any, index: int, root: Path
) -> Tuple[Dict[str, Any], int]:
    field = f"evidence[{index}]"
    evidence = _require_exact_keys(item, EVIDENCE_FIELDS, field)
    relative_path = _require_nonempty_string(evidence["path"], f"{field}.path")
    digest = _require_nonempty_string(
        evidence["source_sha256"], f"{field}.source_sha256"
    )
    if not SHA256_PATTERN.fullmatch(digest):
        raise ValidationError(f"{field}.source_sha256: expected 64 lowercase hex digits")

    start_line = _require_integer(evidence["start_line"], f"{field}.start_line")
    end_line = _require_integer(evidence["end_line"], f"{field}.end_line")
    if start_line > end_line:
        raise ValidationError(f"{field}: start_line must not exceed end_line")
    excerpt_length = end_line - start_line + 1
    if excerpt_length > MAX_EXCERPT_LINES:
        raise ValidationError(
            f"{field}: excerpt has {excerpt_length} lines; maximum is {MAX_EXCERPT_LINES}"
        )

    focus_lines = _require_line_list(evidence["focus_lines"], f"{field}.focus_lines")
    for line in focus_lines:
        if line < start_line or line > end_line:
            raise ValidationError(
                f"{field}.focus_lines: line {line} is outside "
                f"{start_line}-{end_line}"
            )

    smig_fields = ("situation", "mechanism", "implication", "gotcha")
    smig_word_count = 0
    for name in smig_fields:
        prose = _require_nonempty_string(evidence[name], f"{field}.{name}")
        smig_word_count += len(re.findall(r"\S+", prose))
    if smig_word_count > MAX_SMIG_WORDS:
        raise ValidationError(
            f"{field}: SMIG prose has {smig_word_count} words; maximum is {MAX_SMIG_WORDS}"
        )

    candidate = _resolve_source(root, relative_path, f"{field}.path")
    source_lines = _read_source(candidate, relative_path, digest)
    if end_line > len(source_lines):
        raise ValidationError(
            f"{field}.end_line: {end_line} exceeds file length {len(source_lines)}"
        )

    focused = set(focus_lines)
    collected = _clone_json_value(evidence)
    collected["source_lines"] = [
        (line_number, source_lines[line_number - 1], line_number in focused)
        for line_number in range(start_line, end_line + 1)
    ]
    return collected, len(source_lines)


def _validate_problem(problem: Any) -> None:
    value = _require_exact_keys(
        problem,
        PROBLEM_FIELDS,
        "problem",
        optional=PROBLEM_OPTIONAL_FIELDS,
    )
    for field in PROBLEM_FIELDS:
        _require_nonempty_string(value[field], f"problem.{field}")
    if "bridges" not in value:
        return
    bridges = value["bridges"]
    if not isinstance(bridges, list):
        raise ValidationError("problem.bridges: expected an array")
    if not bridges:
        raise ValidationError("problem.bridges: must contain at least one item")
    if len(bridges) > MAX_PROBLEM_BRIDGES:
        raise ValidationError(
            "problem.bridges: expected at most "
            f"{MAX_PROBLEM_BRIDGES} items"
        )
    for index, bridge in enumerate(bridges):
        field = f"problem.bridges[{index}]"
        bridge_value = _require_exact_keys(
            bridge,
            PROBLEM_BRIDGE_FIELDS,
            field,
        )
        for bridge_field in PROBLEM_BRIDGE_FIELDS:
            _require_nonempty_string(
                bridge_value[bridge_field],
                f"{field}.{bridge_field}",
            )


def _validate_context(context: Any) -> None:
    value = _require_exact_keys(context, CONTEXT_FIELDS, "context")
    _require_nonempty_string(value["file_role"], "context.file_role")
    _require_string_list(value["flow"], "context.flow", require_nonempty=True)


def _validate_verification(verification: Any) -> None:
    if not isinstance(verification, list):
        raise ValidationError("verification: expected an array")
    for index, item in enumerate(verification):
        field = f"verification[{index}]"
        value = _require_exact_keys(item, VERIFICATION_FIELDS, field)
        for name in ("claim", "source", "result"):
            _require_nonempty_string(value[name], f"{field}.{name}")
        status = _require_nonempty_string(value["status"], f"{field}.status")
        if status not in VERIFICATION_STATUSES:
            raise ValidationError(
                f"{field}.status: expected one of {', '.join(VERIFICATION_STATUSES)}"
            )


def _validate_proposed_fix(
    proposed_fix: Any, evidence_line_counts: Dict[str, int]
) -> None:
    if proposed_fix is None:
        return
    value = _require_exact_keys(
        proposed_fix,
        PROPOSED_FIX_BASE_FIELDS,
        "proposed_fix",
        optional=PROPOSED_FIX_CODE_FIELDS,
    )
    code_fields = [field for field in PROPOSED_FIX_CODE_FIELDS if field in value]
    if len(code_fields) != 1:
        raise ValidationError(
            "proposed_fix: provide exactly one of replacement or unified_diff"
        )

    path = _require_nonempty_string(value["path"], "proposed_fix.path")
    if path not in evidence_line_counts:
        raise ValidationError("proposed_fix.path: path must match an evidence path")
    start_line = _require_integer(value["start_line"], "proposed_fix.start_line")
    end_line = _require_integer(value["end_line"], "proposed_fix.end_line")
    if start_line > end_line:
        raise ValidationError(
            "proposed_fix: start_line must not exceed end_line"
        )
    if end_line > evidence_line_counts[path]:
        raise ValidationError(
            f"proposed_fix.end_line: {end_line} exceeds file length "
            f"{evidence_line_counts[path]}"
        )
    _require_nonempty_string(value[code_fields[0]], f"proposed_fix.{code_fields[0]}")
    _require_nonempty_string(value["rationale"], "proposed_fix.rationale")
    _require_nonempty_string(
        value["unchanged_behavior"], "proposed_fix.unchanged_behavior"
    )
    _require_string_list(value["risks"], "proposed_fix.risks", require_nonempty=True)


def validate_and_collect(manifest: dict, repo_root: Path) -> dict:
    """Validate a manifest and attach verified source excerpts to a copy."""

    root_value = _require_exact_keys(manifest, ROOT_REQUIRED_FIELDS, "manifest")
    version = root_value["version"]
    if isinstance(version, bool) or not isinstance(version, int):
        raise ValidationError("version: expected an integer")
    if version != MANIFEST_VERSION:
        raise ValidationError(f"version: expected {MANIFEST_VERSION}")
    _require_nonempty_string(root_value["title"], "title")
    _validate_problem(root_value["problem"])
    _validate_context(root_value["context"])
    _require_nonempty_string(root_value["takeaway"], "takeaway")

    try:
        root = Path(repo_root).resolve(strict=True)
    except (OSError, RuntimeError, ValueError) as exc:
        raise ValidationError(f"repo_root: directory does not exist: {repo_root}") from exc
    if not root.is_dir():
        raise ValidationError(f"repo_root: not a directory: {repo_root}")

    evidence = root_value["evidence"]
    if not isinstance(evidence, list):
        raise ValidationError("evidence: expected an array")
    if not MIN_EVIDENCE_ITEMS <= len(evidence) <= MAX_EVIDENCE_ITEMS:
        raise ValidationError(
            "evidence: expected between "
            f"{MIN_EVIDENCE_ITEMS} and {MAX_EVIDENCE_ITEMS} items"
        )

    collected_evidence = []
    evidence_line_counts: Dict[str, int] = {}
    for index, item in enumerate(evidence):
        collected, line_count = _validate_evidence_item(item, index, root)
        collected_evidence.append(collected)
        evidence_line_counts[collected["path"]] = line_count

    _validate_proposed_fix(root_value["proposed_fix"], evidence_line_counts)
    _validate_verification(root_value["verification"])

    collected_manifest = _clone_json_value(root_value)
    collected_manifest["evidence"] = collected_evidence
    return collected_manifest


def _escape(value: str) -> str:
    return html.escape(value, quote=True)


def _line_range(start_line: int, end_line: int) -> str:
    if start_line == end_line:
        return f"Line {start_line}"
    return f"Lines {start_line}\u2013{end_line}"


def _render_problem(problem: Dict[str, Any]) -> str:
    bridge_facts = "".join(
        "    <div><dt>"
        f'{_escape(bridge["label"])}</dt><dd>'
        f'{_escape(bridge["body"])}</dd></div>\n'
        for bridge in problem.get("bridges", [])
    )
    return (
        '<section class="section-card problem-context" '
        'aria-labelledby="problem-heading">\n'
        '  <p class="eyebrow">Problem context</p>\n'
        '  <h2 id="problem-heading">What is failing</h2>\n'
        '  <dl class="problem-facts">\n'
        '    <div><dt>Observed symptom</dt><dd>'
        f'{_escape(problem["symptom"])}</dd></div>\n'
        '    <div><dt>Expected behavior</dt><dd>'
        f'{_escape(problem["expected"])}</dd></div>\n'
        f'{bridge_facts}'
        '    <div><dt>Diagnosis</dt><dd>'
        f'{_escape(problem["diagnosis"])}</dd></div>\n'
        '  </dl>\n'
        '</section>'
    )


def _render_context(context: Dict[str, Any]) -> str:
    flow_items = "\n".join(
        f"      <li>{_escape(step)}</li>" for step in context["flow"]
    )
    return (
        '<section class="section-card file-context" '
        'aria-labelledby="context-heading">\n'
        '  <p class="eyebrow">File context</p>\n'
        '  <h2 id="context-heading">Where this code sits</h2>\n'
        '  <div class="context-grid">\n'
        '    <div class="context-panel">\n'
        '      <span class="label">File role</span>\n'
        f'      <p>{_escape(context["file_role"])}</p>\n'
        '    </div>\n'
        '    <div class="context-panel">\n'
        '      <span class="label">Flow</span>\n'
        '      <ol class="flow-list">\n'
        f'{flow_items}\n'
        '      </ol>\n'
        '    </div>\n'
        '  </div>\n'
        '</section>'
    )


def _render_source_line(source_line: Tuple[int, str, bool]) -> str:
    line_number, source_text, is_focus = source_line
    class_name = "code-line is-focus" if is_focus else "code-line"
    if is_focus:
        focus_marker = (
            '<span class="focus-marker" aria-label="Focus line">\u25cf</span>'
        )
    else:
        focus_marker = '<span class="focus-marker" aria-hidden="true"></span>'
    return (
        f'        <div class="{class_name}" data-line="{line_number}">'
        f'{focus_marker}<span class="line-number">{line_number}</span>'
        f'<code class="source-text">{_escape(source_text)}</code></div>'
    )


def _render_evidence_item(evidence: Dict[str, Any], index: int) -> str:
    source_lines = "\n".join(
        _render_source_line(source_line) for source_line in evidence["source_lines"]
    )
    path = _escape(evidence["path"])
    return (
        f'<article class="evidence-card" aria-labelledby="evidence-{index}-heading">\n'
        '  <div class="evidence-narrative">\n'
        '    <p class="eyebrow">Observed source</p>\n'
        f'    <h3 class="source-heading" id="evidence-{index}-heading">'
        f'<code class="source-path">{path}</code></h3>\n'
        f'    <p class="line-range">{_line_range(evidence["start_line"], evidence["end_line"])}</p>\n'
        '    <dl class="smig-list">\n'
        f'      <div><dt>Situation</dt><dd>{_escape(evidence["situation"])}</dd></div>\n'
        f'      <div><dt>Mechanism</dt><dd>{_escape(evidence["mechanism"])}</dd></div>\n'
        f'      <div><dt>Implication</dt><dd>{_escape(evidence["implication"])}</dd></div>\n'
        f'      <div><dt>Gotcha</dt><dd>{_escape(evidence["gotcha"])}</dd></div>\n'
        '    </dl>\n'
        '  </div>\n'
        '  <div class="code-column">\n'
        f'    <div class="code-window" data-path="{path}" '
        f'data-start-line="{evidence["start_line"]}" '
        f'data-end-line="{evidence["end_line"]}">\n'
        f'{source_lines}\n'
        '    </div>\n'
        '  </div>\n'
        '</article>'
    )


def _render_evidence(evidence: List[Dict[str, Any]]) -> str:
    items = "\n".join(
        _render_evidence_item(item, index)
        for index, item in enumerate(evidence, start=1)
    )
    return (
        '<section class="evidence-section" aria-labelledby="evidence-heading">\n'
        '  <div class="section-card">\n'
        '    <p class="eyebrow">Evidence</p>\n'
        '    <h2 id="evidence-heading">Read the failure in the source</h2>\n'
        '  </div>\n'
        '  <div class="evidence-list">\n'
        f'{items}\n'
        '  </div>\n'
        '</section>'
    )


def _render_proposed_fix(proposed_fix: Optional[Dict[str, Any]]) -> str:
    if proposed_fix is None:
        return ""

    code_field = (
        "replacement" if "replacement" in proposed_fix else "unified_diff"
    )
    code_label = "Replacement" if code_field == "replacement" else "Unified diff"
    risks = "\n".join(
        f"          <li>{_escape(risk)}</li>" for risk in proposed_fix["risks"]
    )
    return (
        '<section class="proposed-fix" aria-labelledby="proposed-heading">\n'
        '  <header class="proposed-header">\n'
        '    <p class="eyebrow">Proposed \u2014 not applied</p>\n'
        '    <h2 id="proposed-heading">A candidate change for review</h2>\n'
        f'    <p><code>{_escape(proposed_fix["path"])}</code> '
        f'\u00b7 {_line_range(proposed_fix["start_line"], proposed_fix["end_line"])}</p>\n'
        '  </header>\n'
        '  <div class="proposed-body">\n'
        '    <div class="proposed-narrative">\n'
        '      <dl>\n'
        f'        <dt>Rationale</dt><dd>{_escape(proposed_fix["rationale"])}</dd>\n'
        '        <dt>Unchanged behavior</dt><dd>'
        f'{_escape(proposed_fix["unchanged_behavior"])}</dd>\n'
        '        <dt>Risks</dt><dd>\n'
        '        <ul>\n'
        f'{risks}\n'
        '        </ul>\n'
        '        </dd>\n'
        '      </dl>\n'
        '    </div>\n'
        '    <div>\n'
        f'      <span class="code-label">{code_label}</span>\n'
        f'      <pre class="proposed-code"><code>{_escape(proposed_fix[code_field])}</code></pre>\n'
        '    </div>\n'
        '  </div>\n'
        '</section>'
    )


def _render_verification(verification: List[Dict[str, str]]) -> str:
    if verification:
        items = []
        for item in verification:
            status = item["status"]
            items.append(
                '    <li class="verification-item">\n'
                f'      <span class="status status-{status}" '
                f'data-status="{status}">{STATUS_LABELS[status]}</span>\n'
                '      <div>\n'
                f'        <h3>{_escape(item["claim"])}</h3>\n'
                f'        <p><span class="label">Source</span>{_escape(item["source"])}</p>\n'
                f'        <p class="verification-result"><span class="label">Result</span>{_escape(item["result"])}</p>\n'
                '      </div>\n'
                '    </li>'
            )
        verification_body = (
            '  <ul class="verification-list">\n'
            + "\n".join(items)
            + "\n  </ul>"
        )
    else:
        verification_body = "  <p>No verification evidence was supplied.</p>"
    return (
        '<section class="section-card verification" '
        'aria-labelledby="verification-heading">\n'
        '  <p class="eyebrow">Verification evidence</p>\n'
        '  <h2 id="verification-heading">What was checked</h2>\n'
        f'{verification_body}\n'
        '</section>'
    )


def _render_takeaway(takeaway: str) -> str:
    return (
        '<section class="takeaway" aria-labelledby="takeaway-heading">\n'
        '  <p class="eyebrow">Takeaway</p>\n'
        '  <h2 id="takeaway-heading">The short version</h2>\n'
        f'  <p>{_escape(takeaway)}</p>\n'
        '</section>'
    )


def _read_template(template_path: Path) -> str:
    try:
        return template_path.read_text(encoding="utf-8")
    except (OSError, UnicodeDecodeError) as exc:
        raise ValidationError(
            f"template: cannot read valid UTF-8 template: {template_path}"
        ) from exc


def _validate_custom_template(template: str) -> None:
    template_tokens = TEMPLATE_TOKEN_PATTERN.findall(template)
    if template_tokens != list(TEMPLATE_PLACEHOLDERS):
        raise ValidationError(
            "template: placeholders must each occur once in required order: "
            + ", ".join(TEMPLATE_PLACEHOLDERS)
        )
    if re.search(r"@\s*import\b", template, re.IGNORECASE):
        raise ValidationError("template: CSS @import is not allowed")

    parser = _TemplateSafetyParser()
    parser.feed(template)
    parser.close()
    if parser.csp_values != [EXPECTED_CSP]:
        raise ValidationError(
            "template: exactly one required Content-Security-Policy is required"
        )


def _substitute_template(template: str, fragments: Dict[str, str]) -> str:
    template_tokens = TEMPLATE_TOKEN_PATTERN.findall(template)
    expected_tokens = set(TEMPLATE_PLACEHOLDERS)
    actual_tokens = set(template_tokens)
    missing = sorted(expected_tokens - actual_tokens)
    unknown = sorted(actual_tokens - expected_tokens)
    if missing:
        raise ValidationError(
            f"template: missing placeholders: {', '.join(missing)}"
        )
    if unknown:
        raise ValidationError(
            f"template: unknown placeholders: {', '.join(unknown)}"
        )

    return TEMPLATE_TOKEN_PATTERN.sub(
        lambda match: fragments[match.group(1)], template
    )


def _is_within(candidate: Path, root: Path) -> bool:
    try:
        candidate.relative_to(root)
    except ValueError:
        return False
    return True


def _validate_output_path(repo_root: Path, output_path: Path) -> None:
    lexical_root = Path(os.path.abspath(str(repo_root)))
    lexical_output = Path(os.path.abspath(str(output_path)))
    if _is_within(lexical_output, lexical_root):
        raise ValidationError("output: path must be outside repository root")

    try:
        resolved_root = Path(repo_root).resolve(strict=True)
        resolved_output = Path(output_path).resolve(strict=False)
    except (OSError, RuntimeError, ValueError) as exc:
        raise ValidationError("output: cannot resolve path safely") from exc
    if _is_within(resolved_output, resolved_root):
        raise ValidationError("output: resolved path must be outside repository root")


def _atomic_write(output_path: Path, rendered_html: str) -> None:
    parent = output_path.parent
    try:
        parent.mkdir(parents=True, exist_ok=True)
    except OSError as exc:
        raise ValidationError(f"output: cannot create directory: {parent}") from exc

    temporary_path = None
    try:
        with tempfile.NamedTemporaryFile(
            mode="w",
            encoding="utf-8",
            newline="\n",
            dir=str(parent),
            prefix=f".{output_path.name}.",
            suffix=".tmp",
            delete=False,
        ) as temporary_file:
            temporary_path = Path(temporary_file.name)
            temporary_file.write(rendered_html)
            temporary_file.flush()
            os.fsync(temporary_file.fileno())
        os.replace(str(temporary_path), str(output_path))
        temporary_path = None
    except OSError as exc:
        raise ValidationError(f"output: cannot write file: {output_path}") from exc
    finally:
        if temporary_path is not None:
            try:
                temporary_path.unlink()
            except FileNotFoundError:
                pass


def render(
    manifest_path: Path,
    repo_root: Path,
    output_path: Path,
    template_path: Optional[Path] = None,
) -> None:
    """Validate evidence and atomically render a deterministic static page."""

    output_path = Path(output_path)
    _validate_output_path(Path(repo_root), output_path)
    try:
        with Path(manifest_path).open("r", encoding="utf-8") as manifest_file:
            manifest = json.load(manifest_file)
    except (OSError, UnicodeDecodeError, json.JSONDecodeError) as exc:
        raise ValidationError(f"manifest: cannot read valid UTF-8 JSON: {manifest_path}") from exc
    collected = validate_and_collect(manifest, repo_root)

    custom_template = template_path is not None
    if not custom_template:
        template_path = Path(__file__).resolve().parents[1] / "assets" / "explainer-template.html"
    template = _read_template(Path(template_path))
    if custom_template:
        _validate_custom_template(template)
    fragments = {
        "TITLE": _escape(collected["title"]),
        "PROBLEM": _render_problem(collected["problem"]),
        "CONTEXT": _render_context(collected["context"]),
        "EVIDENCE": _render_evidence(collected["evidence"]),
        "PROPOSED_FIX": _render_proposed_fix(collected["proposed_fix"]),
        "VERIFICATION": _render_verification(collected["verification"]),
        "TAKEAWAY": _render_takeaway(collected["takeaway"]),
    }
    rendered_html = _substitute_template(template, fragments)
    _atomic_write(output_path, rendered_html)


def _argument_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="Validate evidence and render a static fix explanation."
    )
    parser.add_argument("--manifest", required=True, type=Path)
    parser.add_argument("--repo-root", required=True, type=Path)
    parser.add_argument("--output", required=True, type=Path)
    parser.add_argument("--template", type=Path)
    return parser


def main() -> None:
    args = _argument_parser().parse_args()
    try:
        render(args.manifest, args.repo_root, args.output, args.template)
    except ValidationError as exc:
        raise SystemExit(f"error: {exc}") from exc


if __name__ == "__main__":
    main()
