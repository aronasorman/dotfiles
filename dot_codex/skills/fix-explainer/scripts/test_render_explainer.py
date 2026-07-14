import copy
import hashlib
import json
import os
import stat
import subprocess
import sys
import tempfile
import unittest
from html.parser import HTMLParser
from pathlib import Path


sys.path.insert(0, str(Path(__file__).resolve().parent))

from render_explainer import (
    MAX_EVIDENCE_ITEMS,
    MIN_EVIDENCE_ITEMS,
    MIN_LINE_NUMBER,
    ROOT_REQUIRED_FIELDS,
    VERIFICATION_STATUSES,
    ValidationError,
    render,
    validate_and_collect,
)
from snapshot_repo import SnapshotError, snapshot_repository, write_snapshot


SKILL_ROOT = Path(__file__).resolve().parents[1]
EXPECTED_CSP = (
    "default-src 'none'; style-src 'unsafe-inline'; img-src data:; "
    "base-uri 'none'; form-action 'none'"
)


class SkillContractTests(unittest.TestCase):
    def test_in_app_delivery_remains_available_while_user_is_viewing(self) -> None:
        skill = (SKILL_ROOT / "SKILL.md").read_text(encoding="utf-8")

        self.assertIn(
            "Keep the server and `$ARTIFACT_DIR` alive while the tab is a "
            "user-facing deliverable.",
            skill,
        )
        self.assertIn(
            "open `$ARTIFACT_DIR/index.html` directly in the system browser",
            skill,
        )
        self.assertIn(
            "Clean up only after the user finishes viewing or the delivery "
            "session ends.",
            skill,
        )
        self.assertNotIn("After a page loads, stop the server.", skill)
        self.assertNotIn(
            "After the comparison succeeds and the page has loaded, remove "
            "`$ARTIFACT_DIR`.",
            skill,
        )


def valid_manifest(path: Path, digest: str) -> dict:
    return {
        "version": 1,
        "title": "Why the build fails",
        "problem": {
            "symptom": "The release build starts without a service name.",
            "expected": "Auto discovery emits a service matrix.",
            "diagnosis": "Provider-specific discovery fails before emitting JSON.",
        },
        "context": {
            "file_role": "This script converts changed paths into build jobs.",
            "flow": ["merge", "service discovery", "matrix", "build job"],
        },
        "evidence": [{
            "path": path.name,
            "source_sha256": digest,
            "start_line": 1,
            "end_line": 3,
            "focus_lines": [2],
            "situation": "This is the discovery boundary.",
            "mechanism": "The second line calls the provider API.",
            "implication": "A failure here prevents every downstream build job.",
            "gotcha": "The wrapper may still exit successfully.",
        }],
        "proposed_fix": None,
        "verification": [{
            "claim": "The original task failed before producing a matrix.",
            "source": "pipeline log 9",
            "result": "TypeError at line 2",
            "status": "verified",
        }],
        "takeaway": "Discovery failed; the application change did not.",
    }


class ValidationTests(unittest.TestCase):
    def setUp(self) -> None:
        self.temporary_directory = tempfile.TemporaryDirectory()
        self.temp_path = Path(self.temporary_directory.name)
        self.repo_root = self.temp_path / "repo"
        self.repo_root.mkdir()
        self.source = self.repo_root / "example.ts"
        self.source.write_text(
            "const first = 1;\nconst caf\u00e9 = callProvider();\nreturn first;\n",
            encoding="utf-8",
        )
        self.digest = hashlib.sha256(self.source.read_bytes()).hexdigest()

    def tearDown(self) -> None:
        self.temporary_directory.cleanup()

    def manifest(self) -> dict:
        return valid_manifest(self.source, self.digest)

    def test_valid_evidence_returns_exact_source_line_tuples(self) -> None:
        manifest = self.manifest()

        collected = validate_and_collect(manifest, self.repo_root)

        self.assertEqual(
            collected["evidence"][0]["source_lines"],
            [
                (1, "const first = 1;", False),
                (2, "const caf\u00e9 = callProvider();", True),
                (3, "return first;", False),
            ],
        )
        self.assertNotIn("source_lines", manifest["evidence"][0])

    def test_missing_file_fails(self) -> None:
        manifest = self.manifest()
        manifest["evidence"][0]["path"] = "missing.ts"

        with self.assertRaises(ValidationError):
            validate_and_collect(manifest, self.repo_root)

    def test_absolute_path_fails(self) -> None:
        manifest = self.manifest()
        manifest["evidence"][0]["path"] = str(self.source)

        with self.assertRaises(ValidationError):
            validate_and_collect(manifest, self.repo_root)

    def test_dot_dot_path_fails(self) -> None:
        outside = self.temp_path / "outside.ts"
        outside.write_text("outside\n", encoding="utf-8")
        manifest = self.manifest()
        manifest["evidence"][0]["path"] = "../outside.ts"
        manifest["evidence"][0]["source_sha256"] = hashlib.sha256(
            outside.read_bytes()
        ).hexdigest()

        with self.assertRaises(ValidationError):
            validate_and_collect(manifest, self.repo_root)

    def test_symlink_outside_repository_fails(self) -> None:
        outside = self.temp_path / "outside.ts"
        outside.write_text("outside\n", encoding="utf-8")
        link = self.repo_root / "linked.ts"
        os.symlink(str(outside), str(link))
        manifest = self.manifest()
        manifest["evidence"][0]["path"] = link.name
        manifest["evidence"][0]["source_sha256"] = hashlib.sha256(
            outside.read_bytes()
        ).hexdigest()

        with self.assertRaises(ValidationError):
            validate_and_collect(manifest, self.repo_root)

    def test_incorrect_source_hash_names_field(self) -> None:
        manifest = self.manifest()
        manifest["evidence"][0]["source_sha256"] = "0" * 64

        with self.assertRaisesRegex(ValidationError, "source_sha256"):
            validate_and_collect(manifest, self.repo_root)

    def test_evidence_cardinality_is_one_to_five(self) -> None:
        for count in (0, 6):
            with self.subTest(count=count):
                manifest = self.manifest()
                manifest["evidence"] = [
                    copy.deepcopy(manifest["evidence"][0]) for _ in range(count)
                ]
                with self.assertRaises(ValidationError):
                    validate_and_collect(manifest, self.repo_root)

    def test_twenty_five_line_excerpt_fails(self) -> None:
        source = self.repo_root / "long.ts"
        source.write_text(
            "".join(f"line {line}\n" for line in range(1, 26)),
            encoding="utf-8",
        )
        manifest = valid_manifest(
            source, hashlib.sha256(source.read_bytes()).hexdigest()
        )
        manifest["evidence"][0]["end_line"] = 25

        with self.assertRaises(ValidationError):
            validate_and_collect(manifest, self.repo_root)

    def test_inverted_and_out_of_bounds_ranges_fail(self) -> None:
        for start_line, end_line in ((3, 2), (0, 2), (1, 4)):
            with self.subTest(start_line=start_line, end_line=end_line):
                manifest = self.manifest()
                manifest["evidence"][0]["start_line"] = start_line
                manifest["evidence"][0]["end_line"] = end_line
                with self.assertRaises(ValidationError):
                    validate_and_collect(manifest, self.repo_root)

    def test_focus_line_outside_excerpt_fails(self) -> None:
        manifest = self.manifest()
        manifest["evidence"][0]["focus_lines"] = [4]

        with self.assertRaises(ValidationError):
            validate_and_collect(manifest, self.repo_root)

    def test_combined_smig_prose_over_ninety_words_fails(self) -> None:
        manifest = self.manifest()
        manifest["evidence"][0]["situation"] = " ".join(["word"] * 91)

        with self.assertRaises(ValidationError):
            validate_and_collect(manifest, self.repo_root)

    def test_unknown_verification_status_fails(self) -> None:
        manifest = self.manifest()
        manifest["verification"][0]["status"] = "probably"

        with self.assertRaises(ValidationError):
            validate_and_collect(manifest, self.repo_root)

    def test_proposed_fix_path_must_be_evidenced(self) -> None:
        manifest = self.manifest()
        manifest["proposed_fix"] = self.proposed_fix(path="other.ts")

        with self.assertRaises(ValidationError):
            validate_and_collect(manifest, self.repo_root)

    def test_proposed_fix_range_must_be_ordered_and_in_file(self) -> None:
        for start_line, end_line in ((2, 1), (0, 1), (1, 4)):
            with self.subTest(start_line=start_line, end_line=end_line):
                manifest = self.manifest()
                manifest["proposed_fix"] = self.proposed_fix(
                    start_line=start_line, end_line=end_line
                )
                with self.assertRaises(ValidationError):
                    validate_and_collect(manifest, self.repo_root)

    def test_proposed_fix_requires_exactly_one_code_representation(self) -> None:
        without_code = self.proposed_fix()
        without_code.pop("replacement")
        with_both = self.proposed_fix()
        with_both["unified_diff"] = "@@ -1 +1 @@\n-old\n+new"

        for proposed_fix in (without_code, with_both):
            with self.subTest(keys=sorted(proposed_fix)):
                manifest = self.manifest()
                manifest["proposed_fix"] = proposed_fix
                with self.assertRaises(ValidationError):
                    validate_and_collect(manifest, self.repo_root)

    def test_valid_proposed_replacement_is_preserved(self) -> None:
        manifest = self.manifest()
        manifest["proposed_fix"] = self.proposed_fix()

        collected = validate_and_collect(manifest, self.repo_root)

        self.assertEqual(
            collected["proposed_fix"]["replacement"],
            "const caf\u00e9 = callLocalRepository();",
        )

    def test_unknown_keys_and_wrong_types_fail(self) -> None:
        cases = []
        unknown_root = self.manifest()
        unknown_root["extra"] = True
        cases.append(unknown_root)
        unknown_evidence = self.manifest()
        unknown_evidence["evidence"][0]["extra"] = True
        cases.append(unknown_evidence)
        boolean_line = self.manifest()
        boolean_line["evidence"][0]["start_line"] = True
        cases.append(boolean_line)
        invalid_flow = self.manifest()
        invalid_flow["context"]["flow"] = ["merge", 2]
        cases.append(invalid_flow)

        for manifest in cases:
            with self.subTest(manifest=manifest):
                with self.assertRaises(ValidationError):
                    validate_and_collect(manifest, self.repo_root)

    def test_validation_failure_leaves_output_absent(self) -> None:
        manifest = self.manifest()
        manifest["evidence"][0]["source_sha256"] = "0" * 64
        manifest_path = self.temp_path / "manifest.json"
        manifest_path.write_text(json.dumps(manifest), encoding="utf-8")
        output_path = self.temp_path / "output" / "index.html"

        with self.assertRaises(ValidationError):
            render(manifest_path, self.repo_root, output_path)

        self.assertFalse(output_path.exists())

    def test_reference_schema_parity_and_shipped_fixture(self) -> None:
        schema_path = SKILL_ROOT / "references" / "explainer-schema.json"
        fixture_root = SKILL_ROOT / "evaluations" / "fixtures"
        source_path = fixture_root / "example.ts"
        manifest_path = fixture_root / "example-manifest.json"

        schema = json.loads(schema_path.read_text(encoding="utf-8"))
        fixture_manifest = json.loads(manifest_path.read_text(encoding="utf-8"))

        self.assertEqual(
            schema["$schema"],
            "https://json-schema.org/draft/2020-12/schema",
        )
        self.assertEqual(set(schema["required"]), set(ROOT_REQUIRED_FIELDS))
        evidence_schema = schema["properties"]["evidence"]
        self.assertEqual(evidence_schema["minItems"], MIN_EVIDENCE_ITEMS)
        self.assertEqual(evidence_schema["maxItems"], MAX_EVIDENCE_ITEMS)
        evidence_properties = evidence_schema["items"]["properties"]
        self.assertEqual(
            evidence_properties["start_line"]["minimum"], MIN_LINE_NUMBER
        )
        self.assertEqual(
            evidence_properties["end_line"]["minimum"], MIN_LINE_NUMBER
        )
        self.assertEqual(
            evidence_properties["focus_lines"]["items"]["minimum"],
            MIN_LINE_NUMBER,
        )
        verification_status = schema["properties"]["verification"]["items"][
            "properties"
        ]["status"]["enum"]
        self.assertEqual(set(verification_status), set(VERIFICATION_STATUSES))
        proposed_schema = schema["properties"]["proposed_fix"]
        proposed_object_schema = proposed_schema["oneOf"][1]
        proposed_properties = proposed_object_schema["properties"]
        self.assertEqual(
            proposed_properties["start_line"]["minimum"], MIN_LINE_NUMBER
        )
        self.assertEqual(
            proposed_properties["end_line"]["minimum"], MIN_LINE_NUMBER
        )
        self.assertEqual(
            len(proposed_object_schema["oneOf"]), 2
        )

        actual_digest = hashlib.sha256(source_path.read_bytes()).hexdigest()
        self.assertEqual(
            fixture_manifest["evidence"][0]["source_sha256"], actual_digest
        )
        collected = validate_and_collect(fixture_manifest, fixture_root)
        self.assertEqual(
            collected["evidence"][0]["source_lines"][0][0],
            fixture_manifest["evidence"][0]["start_line"],
        )

        code_only_source = fixture_root / "code-only-example.ts"
        code_only_evidence = (fixture_root / "code-only-evidence.md").read_text(
            encoding="utf-8"
        )
        code_only_digest = hashlib.sha256(code_only_source.read_bytes()).hexdigest()
        self.assertIn(code_only_digest, code_only_evidence)
        self.assertIn(
            "dot_codex/skills/fix-explainer/evaluations/fixtures/code-only-example.ts",
            code_only_evidence,
        )

    def proposed_fix(
        self,
        path: str = "example.ts",
        start_line: int = 2,
        end_line: int = 2,
    ) -> dict:
        return {
            "path": path,
            "start_line": start_line,
            "end_line": end_line,
            "replacement": "const caf\u00e9 = callLocalRepository();",
            "rationale": "Use repository-local discovery.",
            "unchanged_behavior": "Explicit service selection is unchanged.",
            "risks": ["Merge-base selection still needs verification."],
        }


class StartTagCollector(HTMLParser):
    def __init__(self) -> None:
        super().__init__()
        self.tags = []

    def handle_starttag(self, tag: str, attrs: list) -> None:
        self.tags.append((tag, dict(attrs)))


class RepositorySnapshotTests(unittest.TestCase):
    def setUp(self) -> None:
        self.temporary_directory = tempfile.TemporaryDirectory()
        self.repo_root = Path(self.temporary_directory.name) / "repo"
        self.repo_root.mkdir()
        subprocess.run(["git", "init", "-q"], cwd=str(self.repo_root), check=True)
        subprocess.run(
            ["git", "config", "user.email", "test@example.invalid"],
            cwd=str(self.repo_root),
            check=True,
        )
        subprocess.run(
            ["git", "config", "user.name", "Fix Explainer Test"],
            cwd=str(self.repo_root),
            check=True,
        )
        self.tracked = self.repo_root / "tracked.txt"
        self.tracked.write_text("committed\n", encoding="utf-8")
        (self.repo_root / ".gitignore").write_text(
            "ignored.txt\n", encoding="utf-8"
        )
        subprocess.run(
            ["git", "add", "tracked.txt", ".gitignore"],
            cwd=str(self.repo_root),
            check=True,
        )
        subprocess.run(
            ["git", "commit", "-qm", "fixture"],
            cwd=str(self.repo_root),
            check=True,
        )
        self.untracked = self.repo_root / "untracked.txt"

    def tearDown(self) -> None:
        self.temporary_directory.cleanup()

    def test_snapshot_is_stable_when_dirty_repository_is_unchanged(self) -> None:
        self.tracked.write_text("dirty tracked\n", encoding="utf-8")
        self.untracked.write_text("dirty untracked\n", encoding="utf-8")

        before = snapshot_repository(self.repo_root)
        after = snapshot_repository(self.repo_root)

        self.assertEqual(before, after)

    def test_snapshot_detects_edits_hidden_by_unchanged_porcelain(self) -> None:
        self.tracked.write_text("dirty tracked before\n", encoding="utf-8")
        self.untracked.write_text("dirty untracked before\n", encoding="utf-8")
        before = snapshot_repository(self.repo_root)

        self.tracked.write_text("dirty tracked after\n", encoding="utf-8")
        self.untracked.write_text("dirty untracked after\n", encoding="utf-8")
        after = snapshot_repository(self.repo_root)

        self.assertEqual(before["status_sha256"], after["status_sha256"])
        self.assertNotEqual(
            before["tracked_worktree_diff_sha256"],
            after["tracked_worktree_diff_sha256"],
        )
        self.assertNotEqual(
            before["untracked"][0]["content_sha256"],
            after["untracked"][0]["content_sha256"],
        )

    def test_snapshot_output_must_stay_outside_repository(self) -> None:
        output = self.repo_root / "snapshot.json"

        with self.assertRaises(SnapshotError):
            write_snapshot(self.repo_root, output)

        self.assertFalse(output.exists())

    def test_snapshot_detects_edits_to_ignored_file(self) -> None:
        ignored = self.repo_root / "ignored.txt"
        ignored.write_text("ignored before\n", encoding="utf-8")
        before = snapshot_repository(self.repo_root)

        ignored.write_text("ignored after\n", encoding="utf-8")
        after = snapshot_repository(self.repo_root)

        self.assertEqual(before["status_sha256"], after["status_sha256"])
        self.assertNotEqual(
            before["ignored"][0]["content_sha256"],
            after["ignored"][0]["content_sha256"],
        )

    def test_snapshot_cannot_replace_in_repo_symlink_to_outside(self) -> None:
        outside = self.repo_root.parent / "outside.json"
        outside.write_text("user content\n", encoding="utf-8")
        output = self.repo_root / "snapshot.json"
        output.symlink_to(outside)

        with self.assertRaises(SnapshotError):
            write_snapshot(self.repo_root, output)

        self.assertTrue(output.is_symlink())
        self.assertEqual(outside.read_text(encoding="utf-8"), "user content\n")


class RenderingTests(unittest.TestCase):
    def setUp(self) -> None:
        self.temporary_directory = tempfile.TemporaryDirectory()
        self.temp_path = Path(self.temporary_directory.name)
        self.repo_root = self.temp_path / "repo-root-that-must-not-leak"
        self.repo_root.mkdir()
        self.source = self.repo_root / "example.ts"
        self.source.write_text(
            "const first = 1;\nconst caf\u00e9 = callProvider();\nreturn first;\n",
            encoding="utf-8",
        )
        self.digest = hashlib.sha256(self.source.read_bytes()).hexdigest()
        self.manifest_path = self.temp_path / "manifest.json"
        self.output_path = self.temp_path / "artifact" / "index.html"

    def tearDown(self) -> None:
        self.temporary_directory.cleanup()

    def manifest(self) -> dict:
        return valid_manifest(self.source, self.digest)

    def proposed_fix(self) -> dict:
        return {
            "path": self.source.name,
            "start_line": 2,
            "end_line": 2,
            "replacement": "const caf\u00e9 = callLocalRepository();",
            "rationale": "Use repository-local discovery.",
            "unchanged_behavior": "Explicit service selection is unchanged.",
            "risks": ["Merge-base selection still needs verification."],
        }

    def write_manifest(self, manifest: dict) -> None:
        self.manifest_path.write_text(json.dumps(manifest), encoding="utf-8")

    def render_html(self, manifest: dict) -> str:
        self.write_manifest(manifest)
        render(self.manifest_path, self.repo_root, self.output_path)
        return self.output_path.read_text(encoding="utf-8")

    def test_shipped_template_constrains_narrow_width_overflow(self) -> None:
        template = (
            SKILL_ROOT / "assets" / "explainer-template.html"
        ).read_text(encoding="utf-8")

        for pattern in (
            r"(?s)\.content-stack\s*\{[^}]*grid-template-columns:\s*minmax\(0,\s*1fr\);",
            r"(?s)\.content-stack\s*>\s*\*\s*\{[^}]*min-width:\s*0;",
            r"(?s)\.flow-list li\s*\{[^}]*max-width:\s*100%;[^}]*overflow-wrap:\s*anywhere;",
            r"(?s)\.verification-list\s*\{[^}]*grid-template-columns:\s*minmax\(0,\s*1fr\);",
            r"(?s)\.verification-list\s*>\s*\*,\s*\.verification-item\s*>\s*\*\s*\{[^}]*min-width:\s*0;",
            r"(?s)\.verification-item h3,\s*\.verification-item p\s*\{[^}]*overflow-wrap:\s*anywhere;",
        ):
            with self.subTest(pattern=pattern):
                self.assertRegex(template, pattern)

    def custom_template(
        self,
        placeholders: list = None,
        *,
        csp: str = EXPECTED_CSP,
        extra_head: str = "",
        body_attributes: str = "",
        extra_body: str = "",
    ) -> str:
        if placeholders is None:
            placeholders = [
                "TITLE",
                "PROBLEM",
                "CONTEXT",
                "EVIDENCE",
                "PROPOSED_FIX",
                "VERIFICATION",
                "TAKEAWAY",
            ]
        bindings = "\n".join(
            "{{" + placeholder + "}}" for placeholder in placeholders
        )
        return (
            "<!doctype html>\n"
            "<html lang=\"en\">\n"
            "<head>\n"
            f"  <meta http-equiv=\"Content-Security-Policy\" content=\"{csp}\">\n"
            f"  {extra_head}\n"
            "</head>\n"
            f"<body{body_attributes}>\n"
            f"{extra_body}\n"
            f"{bindings}\n"
            "</body>\n"
            "</html>\n"
        )

    def csp_meta(self) -> str:
        return (
            '<meta http-equiv="Content-Security-Policy" '
            f'content="{EXPECTED_CSP}">'
        )

    def initialize_git_fixture(self) -> None:
        tracked = self.repo_root / "tracked.txt"
        tracked.write_text("committed\n", encoding="utf-8")
        os.chmod(self.source, 0o640)
        subprocess.run(
            ["git", "init", "-q"], cwd=str(self.repo_root), check=True
        )
        subprocess.run(
            ["git", "config", "user.email", "test@example.invalid"],
            cwd=str(self.repo_root),
            check=True,
        )
        subprocess.run(
            ["git", "config", "user.name", "Fix Explainer Test"],
            cwd=str(self.repo_root),
            check=True,
        )
        subprocess.run(
            ["git", "add", "example.ts", "tracked.txt"],
            cwd=str(self.repo_root),
            check=True,
        )
        subprocess.run(
            ["git", "commit", "-qm", "fixture"],
            cwd=str(self.repo_root),
            check=True,
        )
        tracked.write_text("locally modified\n", encoding="utf-8")
        (self.repo_root / "untracked.txt").write_text(
            "user work\n", encoding="utf-8"
        )

    def test_valid_render_contains_complete_annotated_source_narrative(self) -> None:
        html = self.render_html(self.manifest())

        expected_text = (
            "Why the build fails",
            "Problem context",
            "The release build starts without a service name.",
            "Auto discovery emits a service matrix.",
            "Diagnosis",
            "Provider-specific discovery fails before emitting JSON.",
            "File role",
            "This script converts changed paths into build jobs.",
            "merge",
            "service discovery",
            "matrix",
            "build job",
            "Observed source",
            "example.ts",
            "Lines 1\u20133",
            "const caf\u00e9 = callProvider();",
            "Situation",
            "Mechanism",
            "Implication",
            "Gotcha",
            "Verification evidence",
            "verified",
            "Discovery failed; the application change did not.",
        )
        for text in expected_text:
            with self.subTest(text=text):
                self.assertIn(text, html)
        self.assertIn('class="code-line is-focus" data-line="2"', html)
        self.assertIn('aria-label="Focus line"', html)
        self.assertNotIn(str(self.repo_root), html)

    def test_not_run_status_has_human_label_and_machine_value(self) -> None:
        manifest = self.manifest()
        manifest["verification"][0]["status"] = "not_run"

        html = self.render_html(manifest)

        self.assertIn('class="status status-not_run"', html)
        self.assertIn('data-status="not_run"', html)
        self.assertIn(">not run</span>", html)

    def test_every_rendered_field_is_html_escaped(self) -> None:
        unsafe = '<script data-x="quoted">alert(\'unsafe\') & stop</script>'
        unsafe_filename = '<script> & "source".ts'
        self.source.rename(self.repo_root / unsafe_filename)
        self.source = self.repo_root / unsafe_filename
        self.source.write_text(
            "const value = '<script src=\"https://evil.invalid/x.js\">&';\n"
            "const second = \"quoted\";\n"
            "return value;\n",
            encoding="utf-8",
        )
        self.digest = hashlib.sha256(self.source.read_bytes()).hexdigest()
        manifest = valid_manifest(self.source, self.digest)
        manifest["title"] = unsafe
        manifest["problem"] = {
            "symptom": unsafe,
            "expected": unsafe,
            "diagnosis": unsafe,
        }
        manifest["context"] = {"file_role": unsafe, "flow": [unsafe]}
        for field in ("situation", "mechanism", "implication", "gotcha"):
            manifest["evidence"][0][field] = unsafe
        manifest["verification"] = [{
            "claim": unsafe,
            "source": unsafe,
            "result": unsafe,
            "status": "not_run",
        }]
        manifest["takeaway"] = unsafe
        manifest["proposed_fix"] = {
            "path": unsafe_filename,
            "start_line": 1,
            "end_line": 1,
            "unified_diff": unsafe,
            "rationale": unsafe,
            "unchanged_behavior": unsafe,
            "risks": [unsafe],
        }

        html = self.render_html(manifest)

        self.assertNotIn(unsafe, html)
        self.assertNotIn(unsafe_filename, html)
        self.assertNotIn('<script src="https://evil.invalid/x.js">', html)
        self.assertIn("&lt;script", html)
        self.assertIn("&amp;", html)
        self.assertIn("&quot;quoted&quot;", html)
        self.assertIn("&#x27;unsafe&#x27;", html)

    def test_output_contains_no_active_or_remote_content(self) -> None:
        manifest = self.manifest()
        manifest["evidence"][0]["situation"] = (
            "The source mentions https://example.invalid without creating a link."
        )
        html = self.render_html(manifest)
        parser = StartTagCollector()
        parser.feed(html)

        active_tags = {"script", "iframe", "object", "embed"}
        self.assertFalse(active_tags.intersection(tag for tag, _ in parser.tags))
        for tag, attrs in parser.tags:
            with self.subTest(tag=tag, attrs=attrs):
                if tag == "link":
                    self.assertFalse(
                        (attrs.get("href") or "").startswith(("http:", "https:", "//"))
                    )
                if "src" in attrs:
                    self.assertTrue((attrs["src"] or "").startswith("data:"))
                if "href" in attrs:
                    self.assertTrue((attrs["href"] or "").startswith("#"))
        self.assertNotIn("@import", html.lower())

    def test_proposed_section_is_omitted_when_fix_is_null(self) -> None:
        html = self.render_html(self.manifest())

        self.assertNotIn("Proposed \u2014 not applied", html)
        self.assertNotIn('class="proposed-fix"', html)

    def test_proposed_section_labels_unapplied_fix_and_risks(self) -> None:
        manifest = self.manifest()
        proposed = self.proposed_fix()
        proposed["replacement"] = '<newCall arg="a&b">\'value\'</newCall>'
        manifest["proposed_fix"] = proposed

        html = self.render_html(manifest)

        for text in (
            "Proposed \u2014 not applied",
            "example.ts",
            "Line 2",
            "Rationale",
            "Use repository-local discovery.",
            "Unchanged behavior",
            "Explicit service selection is unchanged.",
            "Risks",
            "Merge-base selection still needs verification.",
            "Replacement",
        ):
            with self.subTest(text=text):
                self.assertIn(text, html)
        self.assertIn(
            "&lt;newCall arg=&quot;a&amp;b&quot;&gt;&#x27;value&#x27;&lt;/newCall&gt;",
            html,
        )

    def test_identical_inputs_produce_byte_identical_outputs(self) -> None:
        manifest = self.manifest()
        self.write_manifest(manifest)
        first_output = self.temp_path / "first" / "index.html"
        second_output = self.temp_path / "second" / "index.html"

        render(self.manifest_path, self.repo_root, first_output)
        render(self.manifest_path, self.repo_root, second_output)

        self.assertEqual(first_output.read_bytes(), second_output.read_bytes())

    def test_template_substitution_failure_leaves_output_absent(self) -> None:
        self.write_manifest(self.manifest())
        template_path = self.temp_path / "broken-template.html"
        template_path.write_text(
            "{{TITLE}}{{PROBLEM}}{{CONTEXT}}{{EVIDENCE}}"
            "{{PROPOSED_FIX}}{{VERIFICATION}}",
            encoding="utf-8",
        )

        with self.assertRaisesRegex(ValidationError, "template"):
            render(
                self.manifest_path,
                self.repo_root,
                self.output_path,
                template_path,
            )

        self.assertFalse(self.output_path.exists())

    def test_custom_template_requires_each_binding_once_in_order(self) -> None:
        cases = {
            "missing": [
                "TITLE",
                "PROBLEM",
                "CONTEXT",
                "EVIDENCE",
                "PROPOSED_FIX",
                "VERIFICATION",
            ],
            "reordered": [
                "TITLE",
                "CONTEXT",
                "PROBLEM",
                "EVIDENCE",
                "PROPOSED_FIX",
                "VERIFICATION",
                "TAKEAWAY",
            ],
            "duplicated": [
                "TITLE",
                "TITLE",
                "PROBLEM",
                "CONTEXT",
                "EVIDENCE",
                "PROPOSED_FIX",
                "VERIFICATION",
                "TAKEAWAY",
            ],
        }
        self.write_manifest(self.manifest())

        for name, placeholders in cases.items():
            with self.subTest(name=name):
                template_path = self.temp_path / f"{name}-template.html"
                output_path = self.temp_path / f"{name}-output.html"
                template_path.write_text(
                    self.custom_template(placeholders), encoding="utf-8"
                )

                with self.assertRaisesRegex(ValidationError, "template"):
                    render(
                        self.manifest_path,
                        self.repo_root,
                        output_path,
                        template_path,
                    )

                self.assertFalse(output_path.exists())

    def test_custom_template_requires_exact_csp(self) -> None:
        self.write_manifest(self.manifest())
        template_path = self.temp_path / "wrong-csp-template.html"
        template_path.write_text(
            self.custom_template(csp="default-src 'self'"), encoding="utf-8"
        )

        with self.assertRaisesRegex(ValidationError, "template"):
            render(
                self.manifest_path,
                self.repo_root,
                self.output_path,
                template_path,
            )

        self.assertFalse(self.output_path.exists())

    def test_custom_template_rejects_csp_attributes_on_non_meta_element(self) -> None:
        self.write_manifest(self.manifest())
        template_path = self.temp_path / "non-meta-csp-template.html"
        template_path.write_text(
            self.custom_template().replace(
                self.csp_meta(),
                '<div http-equiv="Content-Security-Policy" '
                f'content="{EXPECTED_CSP}"></div>',
                1,
            ),
            encoding="utf-8",
        )

        with self.assertRaisesRegex(ValidationError, "template"):
            render(
                self.manifest_path,
                self.repo_root,
                self.output_path,
                template_path,
            )

        self.assertFalse(self.output_path.exists())

    def test_custom_template_rejects_csp_meta_outside_head(self) -> None:
        self.write_manifest(self.manifest())
        template_path = self.temp_path / "body-csp-template.html"
        template = self.custom_template().replace(self.csp_meta(), "", 1)
        template_path.write_text(
            template.replace(
                "<body>\n", f"<body>\n{self.csp_meta()}\n", 1
            ),
            encoding="utf-8",
        )

        with self.assertRaisesRegex(ValidationError, "template"):
            render(
                self.manifest_path,
                self.repo_root,
                self.output_path,
                template_path,
            )

        self.assertFalse(self.output_path.exists())

    def test_custom_template_rejects_other_http_equiv(self) -> None:
        self.write_manifest(self.manifest())
        template_path = self.temp_path / "refresh-template.html"
        template_path.write_text(
            self.custom_template(
                extra_head=(
                    '<meta http-equiv="refresh" '
                    'content="0; URL=https://evil.invalid/">'
                )
            ),
            encoding="utf-8",
        )

        with self.assertRaisesRegex(ValidationError, "template"):
            render(
                self.manifest_path,
                self.repo_root,
                self.output_path,
                template_path,
            )

        self.assertFalse(self.output_path.exists())

    def test_custom_template_rejects_active_or_remote_content(self) -> None:
        cases = {
            "script": {"extra_body": "<script>run()</script>"},
            "iframe": {"extra_body": "<iframe></iframe>"},
            "object": {"extra_body": "<object></object>"},
            "embed": {"extra_body": "<embed>"},
            "external-link": {
                "extra_head": '<link rel="stylesheet" href="https://evil.invalid/x.css">'
            },
            "event-handler": {"body_attributes": ' onclick="run()"'},
            "remote-src": {
                "extra_body": '<img src="https://evil.invalid/x.png">'
            },
            "external-href": {
                "extra_body": '<a href="https://evil.invalid/">leave</a>'
            },
            "css-import": {
                "extra_head": '<style>@import "https://evil.invalid/x.css";</style>'
            },
        }
        self.write_manifest(self.manifest())

        for name, additions in cases.items():
            with self.subTest(name=name):
                template_path = self.temp_path / f"unsafe-{name}.html"
                output_path = self.temp_path / f"unsafe-{name}-output.html"
                template_path.write_text(
                    self.custom_template(**additions), encoding="utf-8"
                )

                with self.assertRaisesRegex(ValidationError, "template"):
                    render(
                        self.manifest_path,
                        self.repo_root,
                        output_path,
                        template_path,
                    )

                self.assertFalse(output_path.exists())

    def test_safe_custom_template_remains_supported(self) -> None:
        self.write_manifest(self.manifest())
        template_path = self.temp_path / "safe-custom-template.html"
        template_path.write_text(
            self.custom_template(
                extra_body=(
                    '<img alt="pixel" src="data:image/gif;base64,R0lGODlhAQABAAAAACw=">'
                    '<a href="#evidence">Evidence</a>'
                )
            ),
            encoding="utf-8",
        )

        render(
            self.manifest_path,
            self.repo_root,
            self.output_path,
            template_path,
        )

        self.assertTrue(self.output_path.exists())
        self.assertIn("Why the build fails", self.output_path.read_text("utf-8"))

    def test_existing_repository_file_cannot_be_used_as_output(self) -> None:
        self.initialize_git_fixture()
        self.write_manifest(self.manifest())
        before_files = self.repository_snapshot()
        before_status = self.git_status()

        with self.assertRaisesRegex(ValidationError, "output"):
            render(self.manifest_path, self.repo_root, self.source)

        self.assertEqual(self.repository_snapshot(), before_files)
        self.assertEqual(self.git_status(), before_status)

    def test_new_repository_path_cannot_be_used_as_output(self) -> None:
        self.initialize_git_fixture()
        self.write_manifest(self.manifest())
        output_path = self.repo_root / "generated" / "index.html"
        before_files = self.repository_snapshot()
        before_status = self.git_status()

        with self.assertRaisesRegex(ValidationError, "output"):
            render(self.manifest_path, self.repo_root, output_path)

        self.assertFalse(output_path.exists())
        self.assertFalse(output_path.parent.exists())
        self.assertEqual(self.repository_snapshot(), before_files)
        self.assertEqual(self.git_status(), before_status)

    def test_symlinked_output_parent_cannot_resolve_into_repository(self) -> None:
        self.initialize_git_fixture()
        self.write_manifest(self.manifest())
        symlinked_parent = self.temp_path / "outside-link"
        os.symlink(str(self.repo_root), str(symlinked_parent))
        output_path = symlinked_parent / "generated.html"
        repository_target = self.repo_root / "generated.html"
        before_files = self.repository_snapshot()
        before_status = self.git_status()

        with self.assertRaisesRegex(ValidationError, "output"):
            render(self.manifest_path, self.repo_root, output_path)

        self.assertFalse(repository_target.exists())
        self.assertEqual(self.repository_snapshot(), before_files)
        self.assertEqual(self.git_status(), before_status)

    def test_render_preserves_repository_bytes_modes_names_and_git_status(self) -> None:
        self.initialize_git_fixture()
        before_files = self.repository_snapshot()
        before_status = self.git_status()

        self.render_html(self.manifest())

        self.assertEqual(self.repository_snapshot(), before_files)
        self.assertEqual(self.git_status(), before_status)

    def repository_snapshot(self) -> dict:
        return {
            str(path.relative_to(self.repo_root)): (
                path.read_bytes(),
                stat.S_IMODE(path.stat().st_mode),
            )
            for path in sorted(self.repo_root.rglob("*"))
            if path.is_file() and ".git" not in path.relative_to(self.repo_root).parts
        }

    def git_status(self) -> str:
        result = subprocess.run(
            ["git", "status", "--porcelain=v1"],
            cwd=str(self.repo_root),
            check=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
        )
        return result.stdout


if __name__ == "__main__":
    unittest.main()
