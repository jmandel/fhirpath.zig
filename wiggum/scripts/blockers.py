#!/usr/bin/env python3
import argparse
import datetime as dt
import re
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
DEFAULT_FILE = ROOT / "wiggum.config" / "blockers.yaml"

STATUS_VALUES = {"open", "in_progress", "blocked", "resolved", "wontfix"}
SEVERITY_VALUES = {"low", "medium", "high", "critical"}


def _slugify(text: str) -> str:
    text = text.strip().lower()
    text = re.sub(r"[^a-z0-9]+", "-", text)
    text = re.sub(r"^-+|-+$", "", text)
    return text or "blocker"


def _parse_value(raw: str):
    raw = raw.strip()
    if not raw:
        return ""
    if raw.startswith("[") and raw.endswith("]"):
        inner = raw[1:-1].strip()
        if not inner:
            return []
        parts = [p.strip() for p in inner.split(",")]
        return [p.strip('"\'') for p in parts if p]
    if (raw.startswith("\"") and raw.endswith("\"")) or (raw.startswith("'") and raw.endswith("'")):
        return raw[1:-1]
    return raw


def _yaml_escape(value: str) -> str:
    if value is None:
        return ""
    if not value:
        return "\"\""
    if re.search(r"[:#\n]|^\s|\s$", value):
        return '"' + value.replace('"', "\\\"") + '"'
    return value


def load_blockers(path: Path) -> dict:
    if not path.exists():
        return {"version": 1, "blockers": []}

    version = 1
    blockers = []
    current = None
    in_blockers = False

    for raw_line in path.read_text(encoding="utf-8").splitlines():
        line = raw_line.rstrip()
        if not line or line.lstrip().startswith("#"):
            continue
        if line.startswith("version:"):
            version = int(_parse_value(line.split(":", 1)[1]) or 1)
            continue
        if line.startswith("blockers:"):
            in_blockers = True
            continue
        if not in_blockers:
            continue
        if line.startswith("  - "):
            current = {}
            blockers.append(current)
            rest = line[4:]
            if rest and ":" in rest:
                key, val = rest.split(":", 1)
                current[key.strip()] = _parse_value(val)
            continue
        if line.startswith("    ") and current is not None:
            rest = line[4:]
            if ":" in rest:
                key, val = rest.split(":", 1)
                current[key.strip()] = _parse_value(val)

    return {"version": version, "blockers": blockers}


def save_blockers(path: Path, data: dict) -> None:
    lines = [
        "# Blockers (YAML)",
        "#",
        "# Format is simple YAML for deterministic parsing by scripts.",
        "# Only single-line values are supported.",
        "#",
        "# Allowed status values (convention): open, in_progress, blocked, resolved, wontfix",
        "# Allowed severity values (convention): low, medium, high, critical",
        "",
        f"version: {int(data.get('version', 1))}",
        "blockers:",
    ]

    for blk in data.get("blockers", []):
        lines.append(f"  - slug: {_yaml_escape(str(blk.get('slug', '')))}")
        for key in ("status", "severity", "title", "description", "created", "updated"):
            if key in blk:
                lines.append(f"    {key}: {_yaml_escape(str(blk.get(key, '')))}")
        tags = blk.get("tags")
        if tags is not None:
            if isinstance(tags, list):
                rendered = ", ".join(tags)
                lines.append(f"    tags: [{rendered}]")
            else:
                lines.append(f"    tags: [{tags}]")

    path.write_text("\n".join(lines) + "\n", encoding="utf-8")


def list_blockers(data: dict, show_all: bool):
    for blk in data.get("blockers", []):
        status = blk.get("status", "open")
        if not show_all and status in ("resolved", "wontfix"):
            continue
        slug = blk.get("slug", "")
        title = blk.get("title", "")
        severity = blk.get("severity", "")
        print(f"{slug}\t{status}\t{severity}\t{title}")


def show_blocker(data: dict, slug: str):
    for blk in data.get("blockers", []):
        if blk.get("slug") == slug:
            for key in ("slug", "status", "severity", "title", "description", "created", "updated", "tags"):
                if key in blk:
                    print(f"{key}: {blk[key]}")
            return True
    return False


def find_blocker(data: dict, slug: str):
    for blk in data.get("blockers", []):
        if blk.get("slug") == slug:
            return blk
    return None


def main() -> int:
    parser = argparse.ArgumentParser(description="Manage blockers.yaml")
    parser.add_argument("--file", default=str(DEFAULT_FILE), help="Path to blockers.yaml")

    sub = parser.add_subparsers(dest="cmd", required=True)

    p_list = sub.add_parser("list", help="List blockers")
    p_list.add_argument("--all", action="store_true", help="Include resolved/wontfix")

    p_show = sub.add_parser("show", help="Show blocker by slug")
    p_show.add_argument("slug")

    p_add = sub.add_parser("add", help="Add a blocker")
    p_add.add_argument("--slug", default="", help="Slug (auto from title if omitted)")
    p_add.add_argument("--title", required=True)
    p_add.add_argument("--severity", default="medium")
    p_add.add_argument("--status", default="open")
    p_add.add_argument("--description", default="")
    p_add.add_argument("--tags", default="")

    p_update = sub.add_parser("update", help="Update a blocker by slug")
    p_update.add_argument("slug")
    p_update.add_argument("--title")
    p_update.add_argument("--severity")
    p_update.add_argument("--status")
    p_update.add_argument("--description")
    p_update.add_argument("--tags")

    p_resolve = sub.add_parser("resolve", help="Mark blocker resolved")
    p_resolve.add_argument("slug")

    args = parser.parse_args()
    path = Path(args.file)
    data = load_blockers(path)

    if args.cmd == "list":
        list_blockers(data, args.all)
        return 0

    if args.cmd == "show":
        ok = show_blocker(data, args.slug)
        return 0 if ok else 1

    today = dt.date.today().isoformat()

    if args.cmd == "add":
        slug = args.slug.strip() or _slugify(args.title)
        if find_blocker(data, slug):
            print(f"Blocker already exists: {slug}")
            return 1
        status = args.status
        if status not in STATUS_VALUES:
            print(f"Unknown status: {status}")
            return 1
        severity = args.severity
        if severity not in SEVERITY_VALUES:
            print(f"Unknown severity: {severity}")
            return 1
        tags = []
        if args.tags:
            tags = [t.strip() for t in args.tags.split(",") if t.strip()]
        data["blockers"].append({
            "slug": slug,
            "status": status,
            "severity": severity,
            "title": args.title,
            "description": args.description,
            "created": today,
            "updated": today,
            "tags": tags,
        })
        save_blockers(path, data)
        print(slug)
        return 0

    if args.cmd in ("update", "resolve"):
        slug = args.slug
        blk = find_blocker(data, slug)
        if not blk:
            print(f"Blocker not found: {slug}")
            return 1
        if args.cmd == "resolve":
            blk["status"] = "resolved"
        else:
            if args.title is not None:
                blk["title"] = args.title
            if args.severity is not None:
                if args.severity not in SEVERITY_VALUES:
                    print(f"Unknown severity: {args.severity}")
                    return 1
                blk["severity"] = args.severity
            if args.status is not None:
                if args.status not in STATUS_VALUES:
                    print(f"Unknown status: {args.status}")
                    return 1
                blk["status"] = args.status
            if args.description is not None:
                blk["description"] = args.description
            if args.tags is not None:
                blk["tags"] = [t.strip() for t in args.tags.split(",") if t.strip()]
        blk["updated"] = today
        save_blockers(path, data)
        return 0

    return 1


if __name__ == "__main__":
    raise SystemExit(main())
