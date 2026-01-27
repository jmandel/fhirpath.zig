#!/usr/bin/env python3
"""Prepare FHIR definitions under models/.

This script is repo-local and avoids depending on generators outside this root.
"""

from __future__ import annotations

import argparse
import shutil
import urllib.request
import zipfile
from pathlib import Path

DEFAULT_URLS: dict[str, str] = {
    "r4": "https://hl7.org/fhir/R4/definitions.json.zip",
    "r5": "https://hl7.org/fhir/R5/definitions.json.zip",
}

REQUIRED_FILES: tuple[str, ...] = (
    "profiles-types.json",
    "profiles-resources.json",
)


def download_file(url: str, dest: Path) -> None:
    dest.parent.mkdir(parents=True, exist_ok=True)
    with urllib.request.urlopen(url) as response, dest.open("wb") as out:
        shutil.copyfileobj(response, out)


def extract_required(zip_path: Path, out_dir: Path) -> None:
    out_dir.mkdir(parents=True, exist_ok=True)
    with zipfile.ZipFile(zip_path) as zf:
        names = set(zf.namelist())
        for filename in REQUIRED_FILES:
            if filename not in names:
                raise RuntimeError(f"{filename} not found in {zip_path}")
            out_path = out_dir / filename
            with zf.open(filename) as src, out_path.open("wb") as dst:
                shutil.copyfileobj(src, dst)


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--fhir-version", default="r5", help="FHIR version (r4, r5, ...)")
    parser.add_argument("--url", default=None, help="Override definitions.zip URL")
    parser.add_argument(
        "--out-dir",
        default=None,
        help="Output directory (default: v3/models/<version>)",
    )
    parser.add_argument(
        "--force-download",
        action="store_true",
        help="Re-download even if a local zip exists",
    )
    args = parser.parse_args()

    version = args.fhir_version.strip().lower()
    root = Path(__file__).resolve().parents[1]
    out_dir = Path(args.out_dir) if args.out_dir else (root / "models" / version)
    zip_path = out_dir / "definitions.zip"

    url = args.url or DEFAULT_URLS.get(version)
    if not url:
        raise SystemExit(f"No default URL for version: {version}")

    if zip_path.exists() and not args.force_download:
        print(f"Using existing {zip_path}")
    else:
        print(f"Downloading {url} -> {zip_path}")
        download_file(url, zip_path)

    extract_required(zip_path, out_dir)
    print(f"Definitions ready in {out_dir}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
