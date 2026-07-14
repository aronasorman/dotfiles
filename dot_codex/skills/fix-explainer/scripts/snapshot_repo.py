#!/usr/bin/env python3
"""Create a deterministic snapshot of a Git worktree's persistent state."""

import argparse
import base64
import hashlib
import json
import os
import stat
import subprocess
import sys
import tempfile
from pathlib import Path
from typing import Any, Dict, List, Optional, Sequence, Tuple


class SnapshotError(Exception):
    """Raised when a repository cannot be snapshotted safely."""


def _git(
    repo_root: Path,
    arguments: Sequence[str],
    allowed_codes: Tuple[int, ...] = (0,),
) -> subprocess.CompletedProcess:
    process = subprocess.run(
        ["git", "-C", str(repo_root), *arguments],
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        check=False,
    )
    if process.returncode not in allowed_codes:
        detail = process.stderr.decode("utf-8", errors="replace").strip()
        raise SnapshotError(
            f"git {' '.join(arguments)} failed with exit {process.returncode}: {detail}"
        )
    return process


def _sha256_bytes(value: bytes) -> str:
    return hashlib.sha256(value).hexdigest()


def _sha256_file(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as source:
        for chunk in iter(lambda: source.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def _file_kind(mode: int) -> str:
    if stat.S_ISREG(mode):
        return "file"
    if stat.S_ISLNK(mode):
        return "symlink"
    if stat.S_ISDIR(mode):
        return "directory"
    return "other"


def _untracked_entries(repo_root: Path) -> List[Dict[str, Any]]:
    raw_paths = _git(
        repo_root,
        ["ls-files", "--others", "--exclude-standard", "-z"],
    ).stdout
    entries = []
    for path_bytes in sorted(path for path in raw_paths.split(b"\0") if path):
        relative_path = os.fsdecode(path_bytes)
        candidate = repo_root / relative_path
        metadata = os.lstat(str(candidate))
        kind = _file_kind(metadata.st_mode)
        if kind == "file":
            content_digest: Optional[str] = _sha256_file(candidate)
        elif kind == "symlink":
            content_digest = _sha256_bytes(os.fsencode(os.readlink(str(candidate))))
        else:
            content_digest = None
        entries.append({
            "path": relative_path,
            "path_base64": base64.b64encode(path_bytes).decode("ascii"),
            "kind": kind,
            "mode": format(stat.S_IMODE(metadata.st_mode), "04o"),
            "content_sha256": content_digest,
        })
    return entries


def _repository_root(repo_root: Path) -> Path:
    try:
        root = Path(repo_root).resolve(strict=True)
    except (OSError, RuntimeError, ValueError) as exc:
        raise SnapshotError(f"repository does not exist: {repo_root}") from exc
    if not root.is_dir():
        raise SnapshotError(f"repository is not a directory: {repo_root}")
    top_level = Path(
        os.fsdecode(_git(root, ["rev-parse", "--show-toplevel"]).stdout.strip())
    ).resolve(strict=True)
    if top_level != root:
        raise SnapshotError(
            f"repo-root must be the Git top level: expected {top_level}, got {root}"
        )
    return root


def snapshot_repository(repo_root: Path) -> Dict[str, Any]:
    """Return a stable snapshot that catches changes hidden by porcelain status."""

    root = _repository_root(repo_root)
    status = _git(
        root,
        ["status", "--porcelain=v2", "-z", "--untracked-files=all"],
    ).stdout
    tracked_worktree_diff = _git(
        root,
        ["diff", "--no-ext-diff", "--no-textconv", "--binary", "HEAD", "--"],
    ).stdout
    index_diff = _git(
        root,
        ["diff", "--cached", "--no-ext-diff", "--no-textconv", "--binary", "HEAD", "--"],
    ).stdout
    symbolic_ref_process = _git(
        root,
        ["symbolic-ref", "--quiet", "HEAD"],
        allowed_codes=(0, 1),
    )
    symbolic_ref = (
        os.fsdecode(symbolic_ref_process.stdout.strip())
        if symbolic_ref_process.returncode == 0
        else None
    )
    return {
        "version": 1,
        "head": os.fsdecode(_git(root, ["rev-parse", "HEAD"]).stdout.strip()),
        "symbolic_ref": symbolic_ref,
        "status_sha256": _sha256_bytes(status),
        "tracked_worktree_diff_sha256": _sha256_bytes(tracked_worktree_diff),
        "index_diff_sha256": _sha256_bytes(index_diff),
        "untracked": _untracked_entries(root),
    }


def _is_within(path: Path, directory: Path) -> bool:
    return path == directory or directory in path.parents


def write_snapshot(repo_root: Path, output: Path) -> None:
    root = _repository_root(repo_root)
    output_path = Path(output)
    try:
        resolved_output = output_path.resolve(strict=False)
    except (OSError, RuntimeError, ValueError) as exc:
        raise SnapshotError(f"invalid output path: {output}") from exc
    if _is_within(resolved_output, root):
        raise SnapshotError("output must be outside the target repository")

    snapshot = snapshot_repository(root)
    output_path.parent.mkdir(parents=True, exist_ok=True)
    descriptor, temporary_name = tempfile.mkstemp(
        prefix=f".{output_path.name}.", dir=str(output_path.parent)
    )
    try:
        with os.fdopen(descriptor, "w", encoding="utf-8") as destination:
            json.dump(snapshot, destination, ensure_ascii=True, indent=2, sort_keys=True)
            destination.write("\n")
        os.replace(temporary_name, output_path)
    finally:
        try:
            os.unlink(temporary_name)
        except FileNotFoundError:
            pass


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--repo-root", required=True, type=Path)
    parser.add_argument("--output", required=True, type=Path)
    arguments = parser.parse_args()
    try:
        write_snapshot(arguments.repo_root, arguments.output)
    except (OSError, SnapshotError) as exc:
        print(f"snapshot error: {exc}", file=sys.stderr)
        return 2
    return 0


if __name__ == "__main__":
    sys.exit(main())
