#!/usr/bin/env python3
import argparse
import json
import os
import random
import time
from pathlib import Path


def load_state_file(path: Path) -> dict:
    with path.open("r", encoding="utf-8") as f:
        data = json.load(f)
    return data if isinstance(data, dict) else {}


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


def choose_mode(state: dict, params: dict, seed: int | None):
    total = int(state.get("artisinal_total", 0) or 0)
    reviewed = int(state.get("reviewed_total", 0) or 0)
    implemented = int(state.get("implemented_total", 0) or 0)

    floor = float(params.get("floor", 0.15))
    floor = clamp(floor, 0.0, 1.0 / 3.0)
    explore_scale = float(params.get("explore_scale", 10.0))
    if explore_scale <= 0:
        explore_scale = 10.0

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

    raw_sum = raw_explore + raw_develop + raw_confirm
    if raw_sum <= 0:
        raw_explore = raw_develop = raw_confirm = 1.0
        raw_sum = 3.0

    # Apply floor while preserving relative weights.
    scale = 1.0 - 3.0 * floor
    p_explore = floor + scale * (raw_explore / raw_sum)
    p_develop = floor + scale * (raw_develop / raw_sum)
    p_confirm = floor + scale * (raw_confirm / raw_sum)

    rng = random.Random(seed if seed is not None else time.time_ns())
    roll = rng.random()
    if roll < p_explore:
        mode = "EXPLORE"
    elif roll < p_explore + p_develop:
        mode = "DEVELOP"
    else:
        mode = "CONFIRM"

    return mode, (p_explore, p_develop, p_confirm)


def main() -> int:
    parser = argparse.ArgumentParser(description="Choose a work-loop mode based on project state.")
    parser.add_argument("--state", type=str, default="", help="Path to a JSON state summary.")
    parser.add_argument("--root", type=str, default="tests/artisinal", help="Artisinal tests root.")
    parser.add_argument("--seed", type=int, default=None, help="Optional RNG seed.")
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

    mode, probs = choose_mode(state, params, seed)
    p_explore, p_develop, p_confirm = probs

    print("Mode:", mode)
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
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
