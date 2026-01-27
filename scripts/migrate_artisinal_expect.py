#!/usr/bin/env python3
"""Migrate artisinal test files to use explicit type+value format in expect arrays.

Converts:
  "expect": ["hello", 42, "@2024-01-15", "@T10:30:00", "10 'mg'"]
To:
  "expect": [
    {"type": "string", "value": "hello"},
    {"type": "integer", "value": "42"},
    {"type": "date", "value": "2024-01-15"},
    {"type": "time", "value": "10:30:00"},
    {"type": "Quantity", "value": {"value": "10", "unit": "mg"}}
  ]
"""

import json
import re
import sys
from pathlib import Path


def infer_type_and_normalize(val):
    """Infer type from value and normalize to {type, value} format."""
    
    # Already in correct format
    if isinstance(val, dict) and "type" in val and "value" in val:
        # But check if it needs normalization (e.g., date with @ prefix)
        return normalize_typed_value(val)
    
    # Boolean
    if isinstance(val, bool):
        return {"type": "boolean", "value": str(val).lower()}
    
    # Integer
    if isinstance(val, int):
        return {"type": "integer", "value": str(val)}
    
    # Float/decimal
    if isinstance(val, float):
        return {"type": "decimal", "value": str(val)}
    
    # String - need to infer type from content
    if isinstance(val, str):
        return infer_string_type(val)
    
    # Null/other - keep as-is with unknown type
    return {"type": "unknown", "value": val}


def infer_string_type(val: str) -> dict:
    """Infer type from string value based on FHIRPath literal patterns."""
    
    # Time: @Thh:mm:ss...
    if val.startswith("@T"):
        return {"type": "time", "value": val[2:]}  # Strip @T
    
    # DateTime: @yyyy-mm-ddThh:mm:ss... or @yyyyT...
    if val.startswith("@") and "T" in val:
        return {"type": "dateTime", "value": val[1:]}  # Strip @
    
    # Date: @yyyy, @yyyy-mm, @yyyy-mm-dd
    if val.startswith("@"):
        return {"type": "date", "value": val[1:]}  # Strip @
    
    # Quantity: number 'unit'
    qty_match = re.match(r"^([\d.+-eE]+)\s*'([^']*)'$", val)
    if qty_match:
        num_str = qty_match.group(1)
        # Convert to number (int or float)
        if '.' in num_str or 'e' in num_str.lower():
            num_val = float(num_str)
        else:
            num_val = int(num_str)
        return {
            "type": "Quantity",
            "value": {
                "value": num_val,
                "unit": qty_match.group(2),
            }
        }
    
    # Boolean strings
    if val in ("true", "false"):
        return {"type": "boolean", "value": val}
    
    # Integer string
    if re.match(r"^-?\d+$", val):
        return {"type": "integer", "value": val}
    
    # Decimal string
    if re.match(r"^-?\d+\.\d+$", val):
        return {"type": "decimal", "value": val}
    
    # Default to string
    return {"type": "string", "value": val}


def normalize_typed_value(val: dict) -> dict:
    """Normalize already-typed values (strip prefixes, restructure Quantity)."""
    typ = val.get("type", "")
    v = val.get("value", "")
    
    # Date/DateTime: strip @ prefix
    if typ in ("date", "dateTime") and isinstance(v, str) and v.startswith("@"):
        return {"type": typ, "value": v[1:]}
    
    # Time: strip @T or @ prefix
    if typ == "time" and isinstance(v, str):
        if v.startswith("@T"):
            return {"type": typ, "value": v[2:]}
        if v.startswith("@"):
            return {"type": typ, "value": v[1:]}
    
    # Quantity: convert "number 'unit'" to structured form, or fix existing struct with string value
    if typ == "Quantity":
        if isinstance(v, str):
            qty_match = re.match(r"^([\d.+-eE]+)\s*'([^']*)'$", v)
            if qty_match:
                num_str = qty_match.group(1)
                if '.' in num_str or 'e' in num_str.lower():
                    num_val = float(num_str)
                else:
                    num_val = int(num_str)
                return {
                    "type": "Quantity",
                    "value": {
                        "value": num_val,
                        "unit": qty_match.group(2),
                    }
                }
        elif isinstance(v, dict) and "value" in v and isinstance(v["value"], str):
            # Already structured but value is string - convert to number
            num_str = v["value"]
            if '.' in num_str or 'e' in num_str.lower():
                num_val = float(num_str)
            else:
                num_val = int(num_str)
            return {
                "type": "Quantity",
                "value": {
                    "value": num_val,
                    "unit": v.get("unit", ""),
                }
            }
    
    return val


def migrate_file(path: Path, dry_run: bool = False) -> tuple[int, int]:
    """Migrate a single file. Returns (cases_updated, values_updated)."""
    
    with open(path) as f:
        data = json.load(f)
    
    cases_updated = 0
    values_updated = 0
    
    if "cases" not in data:
        return 0, 0
    
    for case in data["cases"]:
        if "expect" not in case:
            continue
        
        new_expect = []
        case_changed = False
        
        for val in case["expect"]:
            new_val = infer_type_and_normalize(val)
            if new_val != val:
                case_changed = True
                values_updated += 1
            new_expect.append(new_val)
        
        if case_changed:
            case["expect"] = new_expect
            cases_updated += 1
    
    if not dry_run and cases_updated > 0:
        with open(path, "w") as f:
            json.dump(data, f, indent=2, ensure_ascii=False)
            f.write("\n")
    
    return cases_updated, values_updated


def main():
    import argparse
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("files", nargs="*", help="Files to migrate (default: all artisinal/*.json)")
    parser.add_argument("--dry-run", "-n", action="store_true", help="Show what would be changed")
    args = parser.parse_args()
    
    if args.files:
        files = [Path(f) for f in args.files]
    else:
        root = Path(__file__).resolve().parent.parent
        files = sorted(root.glob("tests/artisinal/*.json"))
    
    total_cases = 0
    total_values = 0
    
    for path in files:
        cases, values = migrate_file(path, dry_run=args.dry_run)
        if cases > 0:
            action = "would update" if args.dry_run else "updated"
            print(f"{path.name}: {action} {cases} cases ({values} values)")
            total_cases += cases
            total_values += values
    
    if total_cases == 0:
        print("No changes needed.")
    else:
        action = "Would update" if args.dry_run else "Updated"
        print(f"\n{action} {total_cases} cases ({total_values} values) total.")
    
    return 0


if __name__ == "__main__":
    sys.exit(main())
