#!/usr/bin/env python3
"""Validate fix-explainer evidence before rendering a static page."""

import argparse
import hashlib
import json
import re
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


class ValidationError(ValueError):
    """Raised when a manifest cannot be tied safely to observed source."""


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
    value = _require_exact_keys(problem, PROBLEM_FIELDS, "problem")
    for field in PROBLEM_FIELDS:
        _require_nonempty_string(value[field], f"problem.{field}")


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


def render(
    manifest_path: Path,
    repo_root: Path,
    output_path: Path,
    template_path: Optional[Path] = None,
) -> None:
    """Validate inputs before Task 3 adds deterministic HTML generation."""

    del output_path, template_path
    try:
        with Path(manifest_path).open("r", encoding="utf-8") as manifest_file:
            manifest = json.load(manifest_file)
    except (OSError, UnicodeDecodeError, json.JSONDecodeError) as exc:
        raise ValidationError(f"manifest: cannot read valid UTF-8 JSON: {manifest_path}") from exc
    validate_and_collect(manifest, repo_root)
    raise NotImplementedError("HTML rendering is implemented in Task 3")


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
