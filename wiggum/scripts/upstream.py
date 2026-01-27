#!/usr/bin/env python3
"""
Manage upstream issues for FHIRPath spec/implementation maintainers.

Usage:
    upstream.py list [--all]           List issues (excludes reported/fixed by default)
    upstream.py show <slug>            Show issue details
    upstream.py add --title "..." ...  Add a new issue
    upstream.py update <slug> ...      Update an issue
    upstream.py ready <slug>           Mark issue ready to report
    upstream.py report <slug>          Generate issue report for filing upstream
    upstream.py reported <slug> --url  Mark as reported with tracking URL
"""
import argparse
import datetime as dt
import re
import sys
import textwrap
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
DEFAULT_FILE = ROOT / "wiggum" / "upstream-issues.yaml"

STATUS_VALUES = {"draft", "ready", "reported", "acknowledged", "fixed", "wontfix", "duplicate"}
SEVERITY_VALUES = {"low", "medium", "high", "critical"}
CATEGORY_VALUES = {"spec-ambiguity", "spec-error", "test-error", "test-missing", "clarification"}
TARGET_VALUES = {"fhirpath-editors", "fhir-test-cases", "fhir-jira"}

UPSTREAM_REPOS = {
    "fhirpath-editors": "https://github.com/HL7/FHIRPath",
    "fhir-test-cases": "https://github.com/FHIR/fhir-test-cases",
    "fhir-jira": "https://jira.hl7.org/projects/FHIR",
}


def _slugify(text: str) -> str:
    text = text.strip().lower()
    text = re.sub(r"[^a-z0-9]+", "-", text)
    text = re.sub(r"^-+|-+$", "", text)
    return text or "issue"


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
    if (raw.startswith('"') and raw.endswith('"')) or (raw.startswith("'") and raw.endswith("'")):
        return raw[1:-1]
    return raw


def _yaml_escape(value: str) -> str:
    if value is None:
        return ""
    if not value:
        return '""'
    if re.search(r'[:#\n]|^\s|\s$', value):
        return '"' + value.replace('"', '\\"') + '"'
    return value


def load_issues(path: Path) -> dict:
    if not path.exists():
        return {"version": 1, "issues": []}

    version = 1
    issues = []
    current = None
    in_issues = False

    for raw_line in path.read_text(encoding="utf-8").splitlines():
        line = raw_line.rstrip()
        if not line or line.lstrip().startswith("#"):
            continue
        if line.startswith("version:"):
            version = int(_parse_value(line.split(":", 1)[1]) or 1)
            continue
        if line.startswith("issues:"):
            in_issues = True
            continue
        if not in_issues:
            continue
        if line.startswith("  - "):
            current = {}
            issues.append(current)
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

    return {"version": version, "issues": issues}


def save_issues(path: Path, data: dict) -> None:
    lines = [
        "# Upstream Issues (YAML)",
        "#",
        "# Track issues to report to FHIRPath specification/reference implementation maintainers.",
        "# Format is simple YAML for deterministic parsing by scripts.",
        "# Only single-line values are supported.",
        "#",
        "# Allowed status values: draft, ready, reported, acknowledged, fixed, wontfix, duplicate",
        "# Allowed severity values: low, medium, high, critical",
        "# Allowed category values: spec-ambiguity, spec-error, test-error, test-missing, clarification",
        "# Allowed target values: fhirpath-editors, fhir-test-cases, fhir-jira",
        "#",
        "# Workflow:",
        "#   1. Add issue with status=draft while investigating",
        "#   2. Change to status=ready when ready to file upstream",
        "#   3. Run `upstream.py report <slug>` to generate report, then update to status=reported",
        "#   4. Update to acknowledged/fixed/wontfix/duplicate based on upstream response",
        "",
        f"version: {int(data.get('version', 1))}",
        "issues:",
    ]

    fields = (
        "slug", "status", "severity", "category", "target", "title", "description",
        "expected", "actual", "spec_section", "test_file", "test_name",
        "upstream_url", "created", "updated",
    )

    for iss in data.get("issues", []):
        lines.append(f"  - slug: {_yaml_escape(str(iss.get('slug', '')))}")
        for key in fields[1:]:
            if key in iss and iss[key]:
                lines.append(f"    {key}: {_yaml_escape(str(iss.get(key, '')))}")
        tags = iss.get("tags")
        if tags is not None:
            if isinstance(tags, list):
                rendered = ", ".join(tags)
                lines.append(f"    tags: [{rendered}]")
            else:
                lines.append(f"    tags: [{tags}]")

    path.write_text("\n".join(lines) + "\n", encoding="utf-8")


def list_issues(data: dict, show_all: bool):
    for iss in data.get("issues", []):
        status = iss.get("status", "draft")
        if not show_all and status in ("reported", "acknowledged", "fixed", "wontfix", "duplicate"):
            continue
        slug = iss.get("slug", "")
        title = iss.get("title", "")
        severity = iss.get("severity", "")
        category = iss.get("category", "")
        print(f"{slug}\t{status}\t{severity}\t{category}\t{title}")


def show_issue(data: dict, slug: str):
    for iss in data.get("issues", []):
        if iss.get("slug") == slug:
            for key in (
                "slug", "status", "severity", "category", "target", "title", "description",
                "expected", "actual", "spec_section", "test_file", "test_name",
                "upstream_url", "created", "updated", "tags"
            ):
                if key in iss:
                    print(f"{key}: {iss[key]}")
            return True
    return False


def find_issue(data: dict, slug: str):
    for iss in data.get("issues", []):
        if iss.get("slug") == slug:
            return iss
    return None


def generate_report(iss: dict) -> str:
    """Generate a formatted issue report for filing upstream."""
    lines = []

    # Title
    title = iss.get("title", "Untitled Issue")
    category = iss.get("category", "")
    if category:
        category_label = category.replace("-", " ").title()
        lines.append(f"# [{category_label}] {title}")
    else:
        lines.append(f"# {title}")
    lines.append("")

    # Summary
    desc = iss.get("description", "")
    if desc:
        lines.append("## Summary")
        lines.append("")
        lines.append(desc)
        lines.append("")

    # Expected vs Actual
    expected = iss.get("expected", "")
    actual = iss.get("actual", "")
    if expected or actual:
        lines.append("## Observed Behavior")
        lines.append("")
        if expected:
            lines.append(f"**Expected:** {expected}")
        if actual:
            lines.append(f"**Actual:** {actual}")
        lines.append("")

    # Spec reference
    spec_section = iss.get("spec_section", "")
    if spec_section:
        lines.append("## Specification Reference")
        lines.append("")
        lines.append(f"Section: {spec_section}")
        lines.append("")

    # Test reference
    test_file = iss.get("test_file", "")
    test_name = iss.get("test_name", "")
    if test_file or test_name:
        lines.append("## Test Reference")
        lines.append("")
        if test_file:
            lines.append(f"File: `{test_file}`")
        if test_name:
            lines.append(f"Test: `{test_name}`")
        lines.append("")

    # Metadata footer
    lines.append("---")
    lines.append("")
    lines.append(f"Severity: {iss.get('severity', 'medium')}")
    target = iss.get("target", "fhirpath-editors")
    target_url = UPSTREAM_REPOS.get(target, "")
    lines.append(f"Target: {target} ({target_url})")
    lines.append(f"Reported from: fhirpath.zig")
    lines.append(f"Slug: {iss.get('slug', '')}")

    return "\n".join(lines)


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Manage upstream issues for FHIRPath maintainers",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=textwrap.dedent("""
            Examples:
              # Add a new spec ambiguity issue
              upstream.py add --title "union() ordering unclear" \\
                  --category spec-ambiguity \\
                  --description "Spec does not define ordering of union() results" \\
                  --spec_section "5.4.1"

              # Add a test error issue
              upstream.py add --title "testQuantityEquality expects wrong result" \\
                  --category test-error \\
                  --test_file "tests-fhir-r4.xml" \\
                  --test_name "testQuantityEquality" \\
                  --expected "true" --actual "false"

              # Generate report for filing
              upstream.py report union-ordering-unclear

              # Mark as reported with tracking URL
              upstream.py reported union-ordering-unclear \\
                  --url "https://github.com/HL7/FHIRPath/issues/123"

            Upstream repositories:
              fhirpath:        https://github.com/HL7/FHIRPath
              fhir-test-cases: https://github.com/FHIR/fhir-test-cases
              fhir (JIRA):     https://jira.hl7.org/projects/FHIR
        """),
    )
    parser.add_argument("--file", default=str(DEFAULT_FILE), help="Path to upstream-issues.yaml")

    sub = parser.add_subparsers(dest="cmd", required=True)

    # list
    p_list = sub.add_parser("list", help="List upstream issues")
    p_list.add_argument("--all", action="store_true", help="Include reported/fixed/wontfix")

    # show
    p_show = sub.add_parser("show", help="Show issue by slug")
    p_show.add_argument("slug")

    # add
    p_add = sub.add_parser("add", help="Add an upstream issue")
    p_add.add_argument("--slug", default="", help="Slug (auto from title if omitted)")
    p_add.add_argument("--title", required=True)
    p_add.add_argument("--severity", default="medium", choices=list(SEVERITY_VALUES))
    p_add.add_argument("--category", default="", choices=[""] + list(CATEGORY_VALUES))
    p_add.add_argument("--target", default="fhirpath-editors", choices=list(TARGET_VALUES),
                       help="Where to file: fhirpath-editors (spec), fhir-test-cases, fhir-jira")
    p_add.add_argument("--status", default="draft", choices=list(STATUS_VALUES))
    p_add.add_argument("--description", default="")
    p_add.add_argument("--expected", default="", help="Expected behavior/result")
    p_add.add_argument("--actual", default="", help="Actual behavior/result")
    p_add.add_argument("--spec_section", default="", help="Relevant spec section")
    p_add.add_argument("--test_file", default="", help="Test file if test-related")
    p_add.add_argument("--test_name", default="", help="Test name if test-related")
    p_add.add_argument("--tags", default="")

    # update
    p_update = sub.add_parser("update", help="Update an issue by slug")
    p_update.add_argument("slug")
    p_update.add_argument("--title")
    p_update.add_argument("--severity", choices=list(SEVERITY_VALUES))
    p_update.add_argument("--category", choices=list(CATEGORY_VALUES))
    p_update.add_argument("--target", choices=list(TARGET_VALUES))
    p_update.add_argument("--status", choices=list(STATUS_VALUES))
    p_update.add_argument("--description")
    p_update.add_argument("--expected")
    p_update.add_argument("--actual")
    p_update.add_argument("--spec_section")
    p_update.add_argument("--test_file")
    p_update.add_argument("--test_name")
    p_update.add_argument("--tags")

    # ready
    p_ready = sub.add_parser("ready", help="Mark issue ready to report upstream")
    p_ready.add_argument("slug")

    # report
    p_report = sub.add_parser("report", help="Generate issue report for filing upstream")
    p_report.add_argument("slug")
    p_report.add_argument("--format", default="markdown", choices=["markdown", "plain"])

    # reported
    p_reported = sub.add_parser("reported", help="Mark issue as reported with tracking URL")
    p_reported.add_argument("slug")
    p_reported.add_argument("--url", required=True, help="Upstream issue URL")

    args = parser.parse_args()
    path = Path(args.file)
    data = load_issues(path)

    if args.cmd == "list":
        list_issues(data, args.all)
        return 0

    if args.cmd == "show":
        ok = show_issue(data, args.slug)
        return 0 if ok else 1

    today = dt.date.today().isoformat()

    if args.cmd == "add":
        slug = args.slug.strip() or _slugify(args.title)
        if find_issue(data, slug):
            print(f"Issue already exists: {slug}", file=sys.stderr)
            return 1
        issue = {
            "slug": slug,
            "status": args.status,
            "severity": args.severity,
            "target": args.target,
            "title": args.title,
            "created": today,
            "updated": today,
        }
        if args.category:
            issue["category"] = args.category
        if args.description:
            issue["description"] = args.description
        if args.expected:
            issue["expected"] = args.expected
        if args.actual:
            issue["actual"] = args.actual
        if args.spec_section:
            issue["spec_section"] = args.spec_section
        if args.test_file:
            issue["test_file"] = args.test_file
        if args.test_name:
            issue["test_name"] = args.test_name
        if args.tags:
            issue["tags"] = [t.strip() for t in args.tags.split(",") if t.strip()]
        data["issues"].append(issue)
        save_issues(path, data)
        print(slug)
        return 0

    if args.cmd == "update":
        iss = find_issue(data, args.slug)
        if not iss:
            print(f"Issue not found: {args.slug}", file=sys.stderr)
            return 1
        for field in ("title", "severity", "category", "target", "status", "description",
                      "expected", "actual", "spec_section", "test_file", "test_name"):
            val = getattr(args, field, None)
            if val is not None:
                iss[field] = val
        if args.tags is not None:
            iss["tags"] = [t.strip() for t in args.tags.split(",") if t.strip()]
        iss["updated"] = today
        save_issues(path, data)
        return 0

    if args.cmd == "ready":
        iss = find_issue(data, args.slug)
        if not iss:
            print(f"Issue not found: {args.slug}", file=sys.stderr)
            return 1
        iss["status"] = "ready"
        iss["updated"] = today
        save_issues(path, data)
        print(f"Marked {args.slug} as ready to report")
        return 0

    if args.cmd == "report":
        iss = find_issue(data, args.slug)
        if not iss:
            print(f"Issue not found: {args.slug}", file=sys.stderr)
            return 1
        report = generate_report(iss)
        print(report)
        return 0

    if args.cmd == "reported":
        iss = find_issue(data, args.slug)
        if not iss:
            print(f"Issue not found: {args.slug}", file=sys.stderr)
            return 1
        iss["status"] = "reported"
        iss["upstream_url"] = args.url
        iss["updated"] = today
        save_issues(path, data)
        print(f"Marked {args.slug} as reported: {args.url}")
        return 0

    return 1


if __name__ == "__main__":
    raise SystemExit(main())
