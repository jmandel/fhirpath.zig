#!/usr/bin/env python3
import argparse
import json
import random
import time
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
DEFAULT_BLOCKERS = ROOT / "blockers.yaml"


def load_state_file(path: Path) -> dict:
    with path.open("r", encoding="utf-8") as f:
        data = json.load(f)
    return data if isinstance(data, dict) else {}


def _parse_value(raw: str):
    raw = raw.strip()
    if not raw:
        return ""
    if raw.startswith("[") and raw.endswith("]"):
        inner = raw[1:-1].strip()
        if not inner:
            return []
        parts = [p.strip() for p in inner.split(",")]
        return [p.strip("\"'") for p in parts if p]
    if (raw.startswith("\"") and raw.endswith("\"")) or (raw.startswith("'") and raw.endswith("'")):
        return raw[1:-1]
    return raw


def load_blockers(path: Path) -> list[dict]:
    if not path.exists():
        return []
    blockers = []
    current = None
    in_blockers = False
    for raw_line in path.read_text(encoding="utf-8").splitlines():
        line = raw_line.rstrip()
        if not line or line.lstrip().startswith("#"):
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
    return blockers


def open_blockers(blockers: list[dict]) -> list[dict]:
    open_list = []
    for blk in blockers:
        status = str(blk.get("status", "open")).strip().lower()
        if status not in ("resolved", "wontfix"):
            open_list.append(blk)
    return open_list


def derive_state_from_artisinals(root: Path) -> dict:
    total = 0
    reviewed = 0
    implemented = 0

    for p in sorted(root.glob("*.json")):
        if p.name == "state.json":
            continue
        total += 1
        status = None
        try:
            with p.open("r", encoding="utf-8") as f:
                data = json.load(f)
            if isinstance(data, dict):
                meta = data.get("meta", {})
                if isinstance(meta, dict):
                    status = meta.get("status")
                if not status:
                    status = data.get("status")
        except Exception:
            status = None

        if status in ("reviewed", "implemented"):
            reviewed += 1
        if status == "implemented":
            implemented += 1

    return {
        "artisinal_total": total,
        "reviewed_total": reviewed,
        "implemented_total": implemented,
    }


def clamp(v: float, lo: float, hi: float) -> float:
    return max(lo, min(hi, v))


def choose_mode(state: dict, params: dict, seed: int | None, blockers: list[dict]):
    total = int(state.get("artisinal_total", 0) or 0)
    reviewed = int(state.get("reviewed_total", 0) or 0)
    implemented = int(state.get("implemented_total", 0) or 0)

    floor = float(params.get("floor", 0.15))
    max_floor = 1.0 / (4.0 if blockers else 3.0)
    floor = clamp(floor, 0.0, max_floor)
    explore_scale = float(params.get("explore_scale", 10.0))
    if explore_scale <= 0:
        explore_scale = 10.0
    blocker_scale = float(params.get("blocker_scale", 6.0))
    if blocker_scale <= 0:
        blocker_scale = 6.0

    if total <= 0:
        raw_explore = 1.0
        raw_develop = 1.0
        raw_confirm = 1.0
    else:
        reviewed_ratio = reviewed / max(total, 1)
        implemented_ratio = implemented / max(total, 1)
        raw_explore = 1.0 / (1.0 + (total / explore_scale))
        raw_develop = max(0.0, 1.0 - implemented_ratio)
        raw_confirm = max(0.0, 1.0 - reviewed_ratio)

    if blockers:
        severity_weights = {"critical": 4.0, "high": 3.0, "medium": 2.0, "low": 1.0}
        total_weight = 0.0
        for blk in blockers:
            sev = str(blk.get("severity", "medium")).strip().lower()
            total_weight += severity_weights.get(sev, 1.0)
        raw_blocker = min(1.0, total_weight / blocker_scale)
        raw_sum = raw_explore + raw_develop + raw_confirm + raw_blocker
        if raw_sum <= 0:
            raw_explore = raw_develop = raw_confirm = raw_blocker = 1.0
            raw_sum = 4.0
        scale = 1.0 - 4.0 * floor
        p_explore = floor + scale * (raw_explore / raw_sum)
        p_develop = floor + scale * (raw_develop / raw_sum)
        p_confirm = floor + scale * (raw_confirm / raw_sum)
        p_blocker = floor + scale * (raw_blocker / raw_sum)
    else:
        raw_sum = raw_explore + raw_develop + raw_confirm
        if raw_sum <= 0:
            raw_explore = raw_develop = raw_confirm = 1.0
            raw_sum = 3.0
        scale = 1.0 - 3.0 * floor
        p_explore = floor + scale * (raw_explore / raw_sum)
        p_develop = floor + scale * (raw_develop / raw_sum)
        p_confirm = floor + scale * (raw_confirm / raw_sum)
        p_blocker = 0.0

    rng = random.Random(seed if seed is not None else time.time_ns())
    roll = rng.random()
    if not blockers:
        if roll < p_explore:
            mode = "EXPLORE"
        elif roll < p_explore + p_develop:
            mode = "DEVELOP"
        else:
            mode = "CONFIRM"
    else:
        if roll < p_explore:
            mode = "EXPLORE"
        elif roll < p_explore + p_develop:
            mode = "DEVELOP"
        elif roll < p_explore + p_develop + p_confirm:
            mode = "CONFIRM"
        else:
            mode = "TACKLE_BLOCKER"

    return mode, (p_explore, p_develop, p_confirm, p_blocker)


def main() -> int:
    parser = argparse.ArgumentParser(description="Choose a work-loop mode based on project state.")
    parser.add_argument("--state", type=str, default="", help="Path to a JSON state summary.")
    parser.add_argument("--root", type=str, default="tests/artisinal", help="Artisinal tests root.")
    parser.add_argument("--seed", type=int, default=None, help="Optional RNG seed.")
    parser.add_argument("--blockers", type=str, default=str(DEFAULT_BLOCKERS), help="Path to blockers.yaml")
    args = parser.parse_args()

    state = {}
    params = {}

    if args.state:
        state_path = Path(args.state)
        if state_path.exists():
            state = load_state_file(state_path)
            params = state.get("params", {}) if isinstance(state.get("params", {}), dict) else {}

    if not state:
        state = derive_state_from_artisinals(Path(args.root))

    seed = args.seed
    if seed is None:
        seed = params.get("seed")
        if seed is not None:
            try:
                seed = int(seed)
            except Exception:
                seed = None

    rng = random.Random(seed if seed is not None else time.time_ns())

    blocker_list = open_blockers(load_blockers(Path(args.blockers)))

    mode, probs = choose_mode(state, params, seed, blocker_list)
    p_explore, p_develop, p_confirm, p_blocker = probs

    print("Mode:", mode)
    if blocker_list:
        print(
            "Probabilities:",
            f"EXPLORE={p_explore:.2f}",
            f"DEVELOP={p_develop:.2f}",
            f"CONFIRM={p_confirm:.2f}",
            f"TACKLE_BLOCKER={p_blocker:.2f}",
        )
    else:
        print(
            "Probabilities:",
            f"EXPLORE={p_explore:.2f}",
            f"DEVELOP={p_develop:.2f}",
            f"CONFIRM={p_confirm:.2f}",
        )
    print(
        "State:",
        f"written={state.get('artisinal_total', 0)}",
        f"reviewed={state.get('reviewed_total', 0)}",
        f"implemented={state.get('implemented_total', 0)}",
        f"blockers_open={len(blocker_list)}",
    )
    if mode == "TACKLE_BLOCKER" and blocker_list:
        severity_weights = {"critical": 4.0, "high": 3.0, "medium": 2.0, "low": 1.0}
        weights = [severity_weights.get(str(b.get("severity", "medium")).lower(), 1.0) for b in blocker_list]
        total = sum(weights)
        pick = rng.random() * total if total > 0 else rng.random() * len(blocker_list)
        idx = 0
        acc = 0.0
        for i, w in enumerate(weights):
            acc += w
            if pick <= acc:
                idx = i
                break
        chosen = blocker_list[idx]
        print("Blocker:", chosen.get("slug", ""))
        if chosen.get("title"):
            print("Blocker Title:", chosen.get("title", ""))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
