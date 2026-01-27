#!/usr/bin/env python3
"""Build a compact schema model blob from FHIR StructureDefinitions."""

from __future__ import annotations

import argparse
import json
import struct
from dataclasses import dataclass, field
from pathlib import Path
from typing import Dict, Iterable, List, Optional, Tuple


SYSTEM_TYPES = [
    "System.Any",
    "System.Boolean",
    "System.Integer",
    "System.Decimal",
    "System.String",
    "System.Date",
    "System.DateTime",
    "System.Time",
    "System.Quantity",
]

SYSTEM_TYPE_IDS = {name: i + 1 for i, name in enumerate(SYSTEM_TYPES)}

KIND_PRIMITIVE = 1
KIND_COMPLEX = 2
KIND_RESOURCE = 3
KIND_BACKBONE = 4

FIELD_MULTIPLE = 1 << 0
FIELD_CHOICE_BASE = 1 << 1
FIELD_CHOICE_VARIANT = 1 << 2


@dataclass
class FieldInfo:
    name: str
    child_type: Optional[str]
    child_system_id: Optional[int]
    flags: int
    choice_group: Optional[int]


@dataclass
class TypeInfo:
    name: str
    kind: int
    base_name: Optional[str]
    prim_base_name: Optional[str]
    fields: List[FieldInfo] = field(default_factory=list)
    field_map: Dict[str, FieldInfo] = field(default_factory=dict)


@dataclass
class ChoiceGroup:
    base_name: str
    variants: List[Tuple[str, Optional[str], Optional[int]]] = field(default_factory=list)


def load_structure_defs(paths: Iterable[Path]) -> List[dict]:
    defs: List[dict] = []
    for path in paths:
        with path.open() as fh:
            bundle = json.load(fh)
        for entry in bundle.get("entry", []):
            res = entry.get("resource")
            if not res or res.get("resourceType") != "StructureDefinition":
                continue
            defs.append(res)
    return defs


def last_path_segment(url: Optional[str]) -> Optional[str]:
    if not url:
        return None
    if "/" in url:
        return url.rsplit("/", 1)[-1]
    return url


def normalize_type_code(code: str) -> Tuple[Optional[str], Optional[int]]:
    if code.startswith("http://hl7.org/fhirpath/System."):
        sys_name = "System." + code.split("System.", 1)[1]
        return None, SYSTEM_TYPE_IDS.get(sys_name)
    if "/" in code:
        code = code.rsplit("/", 1)[-1]
    return code, None


def capitalize_choice_suffix(name: str) -> str:
    if not name:
        return name
    return name[0].upper() + name[1:]


def align4(value: int) -> int:
    return (value + 3) & ~3


def add_field(
    typ: TypeInfo,
    name: str,
    child_type: Optional[str],
    child_system_id: Optional[int],
    flags: int,
    choice_group: Optional[int],
) -> None:
    if name in typ.field_map:
        field = typ.field_map[name]
        field.flags |= flags
        if child_type and not field.child_type:
            field.child_type = child_type
        if child_system_id and not field.child_system_id:
            field.child_system_id = child_system_id
        if choice_group is not None:
            field.choice_group = choice_group
        return
    field = FieldInfo(
        name=name,
        child_type=child_type,
        child_system_id=child_system_id,
        flags=flags,
        choice_group=choice_group,
    )
    typ.fields.append(field)
    typ.field_map[name] = field


def build_model(version: str, in_dir: Path, out_path: Path) -> None:
    defs = load_structure_defs(
        [in_dir / "profiles-types.json", in_dir / "profiles-resources.json"]
    )

    types: Dict[str, TypeInfo] = {}

    for sd in defs:
        name = sd.get("type")
        if not name or name in types:
            continue
        kind = sd.get("kind")
        if kind == "primitive-type":
            kind_id = KIND_PRIMITIVE
        elif kind == "resource":
            kind_id = KIND_RESOURCE
        else:
            kind_id = KIND_COMPLEX
        base_name = last_path_segment(sd.get("baseDefinition"))
        prim_base = base_name if kind == "primitive-type" else None
        types[name] = TypeInfo(
            name=name,
            kind=kind_id,
            base_name=base_name,
            prim_base_name=prim_base,
        )

    backbone_base = "BackboneElement" if "BackboneElement" in types else "Element"
    for sd in defs:
        root = sd.get("type")
        snap = sd.get("snapshot", {}).get("element", [])
        for el in snap:
            path = el.get("path")
            if not path or "." not in path:
                continue
            parent_path = path.rsplit(".", 1)[0]
            if parent_path == root:
                continue
            if parent_path not in types:
                types[parent_path] = TypeInfo(
                    name=parent_path,
                    kind=KIND_BACKBONE,
                    base_name=backbone_base,
                    prim_base_name=None,
                )

    choice_groups: Dict[Tuple[str, str], int] = {}
    groups: List[ChoiceGroup] = []

    for sd in defs:
        root = sd.get("type")
        snap = sd.get("snapshot", {}).get("element", [])
        for el in snap:
            path = el.get("path")
            if not path or path == root or "." not in path:
                continue
            parent_path, field_name = path.rsplit(".", 1)
            parent = types.get(parent_path)
            if parent is None:
                continue

            max_val = el.get("max", "1")
            multiple = max_val not in ("0", "1")
            flags_base = FIELD_MULTIPLE if multiple else 0

            if field_name.endswith("[x]"):
                base_name = field_name.replace("[x]", "")
                key = (parent_path, base_name)
                group_idx = choice_groups.get(key)
                if group_idx is None:
                    group_idx = len(groups)
                    choice_groups[key] = group_idx
                    groups.append(ChoiceGroup(base_name=base_name))
                add_field(parent, base_name, None, None, flags_base | FIELD_CHOICE_BASE, group_idx + 1)
                for t in el.get("type", []):
                    code = t.get("code")
                    if not code:
                        continue
                    local, sys_id = normalize_type_code(code)
                    if sys_id is not None:
                        variant_type = None
                    else:
                        variant_type = local
                    if local is None and sys_id is None:
                        continue
                    suffix = capitalize_choice_suffix(local or SYSTEM_TYPES[sys_id - 1].split(".")[-1])
                    variant_name = base_name + suffix
                    add_field(parent, variant_name, variant_type, sys_id, flags_base | FIELD_CHOICE_VARIANT, group_idx + 1)
                    groups[group_idx].variants.append((variant_name, variant_type, sys_id))
                continue

            types_list = el.get("type", [])
            content_ref = el.get("contentReference")
            child_type: Optional[str] = None
            child_sys_id: Optional[int] = None
            if types_list:
                code = types_list[0].get("code")
                if code:
                    local, sys_id = normalize_type_code(code)
                    child_sys_id = sys_id
                    child_type = local
            elif content_ref:
                child_type = content_ref.lstrip("#")

            if child_type in ("BackboneElement", "Element"):
                child_type = path

            add_field(parent, field_name, child_type, child_sys_id, flags_base, None)

    type_list = sorted(types.values(), key=lambda t: t.name)
    type_index = {t.name: i for i, t in enumerate(type_list)}

    string_pool: bytearray = bytearray()
    string_offsets: Dict[str, Tuple[int, int]] = {}

    def intern(text: str) -> Tuple[int, int]:
        if text in string_offsets:
            return string_offsets[text]
        off = len(string_pool)
        data = text.encode("utf-8")
        string_pool.extend(data)
        string_offsets[text] = (off, len(data))
        return off, len(data)

    field_entries: List[Tuple[int, int, int, int, int]] = []  # (name_off, name_len, child_id, flags, choice_group)
    type_entries: List[Tuple[int, int, int, int, int, int, int, int]] = []

    for t in type_list:
        t.fields.sort(key=lambda f: f.name)

    for t in type_list:
        field_start = len(field_entries)
        for f in t.fields:
            child_id = 0
            if f.child_system_id is not None:
                child_id = f.child_system_id
            elif f.child_type is not None:
                if f.child_type not in type_index:
                    continue
                child_id = 0x8000_0000 | (type_index[f.child_type] + 1)
            name_off, name_len = intern(f.name)
            field_entries.append((name_off, name_len, child_id, f.flags, f.choice_group or 0))
        field_count = len(field_entries) - field_start
        name_off, name_len = intern(t.name)
        base_id = 0
        prim_base_id = 0
        if t.base_name and t.base_name in type_index:
            base_id = 0x8000_0000 | (type_index[t.base_name] + 1)
        if t.prim_base_name and t.prim_base_name in type_index:
            prim_base_id = 0x8000_0000 | (type_index[t.prim_base_name] + 1)
        type_entries.append(
            (
                name_off,
                name_len,
                t.kind,
                0,
                base_id,
                prim_base_id,
                field_start,
                field_count,
            )
        )

    name_index = []
    for idx, t in enumerate(type_list):
        off, length = intern(t.name)
        name_index.append((t.name, off, length, idx))
    name_index.sort(key=lambda x: x[0])

    choice_groups_entries = []
    choice_variants_entries = []
    for group in groups:
        base_off, base_len = intern(group.base_name)
        variant_start = len(choice_variants_entries)
        for variant_name, variant_type, variant_sys in group.variants:
            name_off, name_len = intern(variant_name)
            if variant_sys is not None:
                type_id = variant_sys
            elif variant_type and variant_type in type_index:
                type_id = 0x8000_0000 | (type_index[variant_type] + 1)
            else:
                type_id = 0
            choice_variants_entries.append((name_off, name_len, type_id))
        variant_count = len(choice_variants_entries) - variant_start
        choice_groups_entries.append((base_off, base_len, variant_start, variant_count))

    strings_off = 64
    strings_len = len(string_pool)
    types_off = align4(strings_off + strings_len)
    name_index_off = align4(types_off + len(type_entries) * 24)
    fields_off = align4(name_index_off + len(name_index) * 12)
    choice_groups_off = align4(fields_off + len(field_entries) * 20)
    choice_variants_off = align4(choice_groups_off + len(choice_groups_entries) * 16)
    total_size = align4(choice_variants_off + len(choice_variants_entries) * 12)

    out = bytearray(total_size)
    out[0:4] = b"FPM4"
    struct.pack_into("<HH", out, 4, 1, 64)
    struct.pack_into(
        "<IIIIIIIIIIIIII",
        out,
        8,
        total_size,
        strings_off,
        strings_len,
        types_off,
        len(type_entries),
        name_index_off,
        len(name_index),
        fields_off,
        len(field_entries),
        choice_groups_off,
        len(choice_groups_entries),
        choice_variants_off,
        len(choice_variants_entries),
        0,
    )

    out[strings_off : strings_off + strings_len] = string_pool

    for i, entry in enumerate(type_entries):
        off = types_off + i * 24
        name_off, name_len, kind, flags, base_id, prim_base_id, field_start, field_count = entry
        struct.pack_into("<IHB", out, off, name_off, name_len, kind)
        struct.pack_into("B", out, off + 7, flags)
        struct.pack_into("<IIII", out, off + 8, base_id, prim_base_id, field_start, field_count)

    for i, entry in enumerate(name_index):
        _, name_off, name_len, type_idx = entry
        off = name_index_off + i * 12
        struct.pack_into("<III", out, off, name_off, name_len, type_idx)

    for i, entry in enumerate(field_entries):
        name_off, name_len, child_id, flags, choice_group = entry
        off = fields_off + i * 20
        struct.pack_into("<IIIII", out, off, name_off, name_len, child_id, flags, choice_group)

    for i, entry in enumerate(choice_groups_entries):
        base_off, base_len, variant_start, variant_count = entry
        off = choice_groups_off + i * 16
        struct.pack_into("<IIII", out, off, base_off, base_len, variant_start, variant_count)

    for i, entry in enumerate(choice_variants_entries):
        name_off, name_len, type_id = entry
        off = choice_variants_off + i * 12
        struct.pack_into("<III", out, off, name_off, name_len, type_id)

    out_path.parent.mkdir(parents=True, exist_ok=True)
    out_path.write_bytes(out)
    print(f"Wrote {out_path} ({len(out)} bytes)")


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--fhir-version", default="r5", help="FHIR version (r4, r5)")
    parser.add_argument("--in-dir", default=None, help="Input directory (default: models/<version>)")
    parser.add_argument("--out", default=None, help="Output model path (default: models/<version>/model.bin)")
    args = parser.parse_args()

    version = args.fhir_version.strip().lower()
    root = Path(__file__).resolve().parents[1]
    in_dir = Path(args.in_dir) if args.in_dir else root / "models" / version
    out_path = Path(args.out) if args.out else in_dir / "model.bin"

    build_model(version, in_dir, out_path)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
