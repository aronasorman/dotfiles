import copy
import hashlib
import json
import os
import sys
import tempfile
import unittest
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


SKILL_ROOT = Path(__file__).resolve().parents[1]


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


if __name__ == "__main__":
    unittest.main()
