#!/usr/bin/env bash
#
# Compare versions of Warp with bench:server.
#
#   warp/bench/compare.sh [options] REF [REF...]
#
# Each REF is a git ref, or "." for the current working tree. Example:
#
#   warp/bench/compare.sh origin/master origin/perf/time-manager-tickle .
#
# The benchmark measures whichever Warp it was linked against and cannot
# switch at runtime, so comparing versions means building each one. This
# script does that, and encodes the things that are easy to get wrong when
# doing it by hand:
#
#   * the benchmark lives in the tree being measured, so an older ref does not
#     have it -- its source and cabal stanza are copied in from here, so every
#     version runs byte-identical benchmark code;
#   * runs are interleaved rather than one block per version, because the same
#     binary has been seen to vary 35% between consecutive runs and running in
#     blocks attributes that drift to the code;
#   * result files are truncated, because appending silently mixes a previous
#     session into the medians;
#   * a ref that bumps time-manager's version needs allow-newer or dependency
#     resolution fails before anything is built.
#
# Absolute numbers here are worth little: they depend on core count, kernel
# and background load. The ratios between versions, measured in one sitting on
# an idle machine, are the point.
set -euo pipefail

rounds=3
scenarios="keepalive,stream,churn,idle"
extra=""
clean=0
workdir="${TMPDIR:-/tmp}/warp-compare"

usage() {
    sed -n '2,/^set -euo/p' "$0" | sed 's/^# \{0,1\}//; $d'
    cat <<'EOF'
Options:
  --rounds N        interleaved rounds per version (default 3)
  --scenarios LIST  comma-separated: keepalive,stream,churn,idle
  --extra "ARGS"    extra arguments passed to every benchmark invocation
  --workdir DIR     where worktrees are built (default $TMPDIR/warp-compare)
  --clean           remove the worktrees afterwards (default: keep them, so a
                    re-run does not rebuild from scratch)
EOF
}

while [ $# -gt 0 ]; do
    case "$1" in
        --rounds) rounds="$2"; shift 2 ;;
        --scenarios) scenarios="$2"; shift 2 ;;
        --extra) extra="$2"; shift 2 ;;
        --workdir) workdir="$2"; shift 2 ;;
        --clean) clean=1; shift ;;
        -h|--help) usage; exit 0 ;;
        --*) echo "unknown option: $1" >&2; usage >&2; exit 2 ;;
        *) break ;;
    esac
done

[ $# -ge 1 ] || { echo "need at least one REF" >&2; usage >&2; exit 2; }
command -v python3 >/dev/null || {
    echo "python3 is needed to summarise the results" >&2; exit 2; }

root=$(git rev-parse --show-toplevel)
cd "$root"
[ -f warp/bench/ServerBench.hs ] || {
    echo "run this from a checkout that has warp/bench/ServerBench.hs" >&2; exit 2; }

mkdir -p "$workdir"
results="$workdir/results"
rm -rf "$results"; mkdir -p "$results"

# Per-scenario arguments. churn is count-bounded and the rest are time-bounded;
# all are sized to give a measured window of several seconds, since CPU is
# sampled at clock-tick granularity.
args_for() {
    case "$1" in
        keepalive) echo "--conns 100 --duration 8 --warmup 2" ;;
        stream)    echo "--conns 16 --duration 8 --warmup 2" ;;
        churn)     echo "--conns 50" ;;
        idle)      echo "--conns 2000 --duration 8" ;;
        *) echo "unknown scenario: $1" >&2; exit 2 ;;
    esac
}

# Give a tree the benchmark and its stanza, then build it. Cabal does not care
# where a stanza appears in the file, so appending avoids having to splice.
prepare() {
    local dir="$1" name="$2"
    cp warp/bench/ServerBench.hs "$dir/warp/bench/"
    if ! grep -q '^benchmark server$' "$dir/warp/warp.cabal"; then
        sed -n '/^benchmark server$/,/^benchmark response$/p' warp/warp.cabal \
            | sed '$d' >> "$dir/warp/warp.cabal"
    fi
    # Harmless when the ref does not move time-manager; required when it does.
    printf 'allow-newer: http2:time-manager, http3:time-manager, http-semantics:time-manager\n' \
        > "$dir/cabal.project.local"
    echo "building $name ..." >&2
    ( cd "$dir" && cabal build warp:bench:server >/dev/null 2>&1 ) || {
        echo "build failed for $name; rerun by hand in $dir to see why" >&2; exit 1; }
}

names=(); bins=()
for ref in "$@"; do
    if [ "$ref" = "." ]; then
        name="working-tree"
        echo "building $name ..." >&2
        cabal build warp:bench:server >/dev/null 2>&1 || {
            echo "build failed for $name" >&2; exit 1; }
        bin=$(cabal list-bin warp:bench:server)
    else
        name=$(echo "$ref" | tr '/' '-')
        dir="$workdir/$name"
        if [ -d "$dir" ]; then
            git -C "$dir" checkout --detach "$ref" >/dev/null 2>&1
        else
            git worktree add --detach "$dir" "$ref" >/dev/null 2>&1 || {
                echo "could not create a worktree for $ref" >&2; exit 1; }
        fi
        prepare "$dir" "$name"
        bin=$( cd "$dir" && cabal list-bin warp:bench:server )
    fi
    names+=("$name"); bins+=("$bin")
done

IFS=',' read -r -a scen <<< "$scenarios"
for s in "${scen[@]}"; do args_for "$s" >/dev/null; done

echo >&2
for r in $(seq 1 "$rounds"); do
    for i in "${!names[@]}"; do
        for s in "${scen[@]}"; do
            # shellcheck disable=SC2086
            "${bins[$i]}" --scenario "$s" $(args_for "$s") $extra --json \
                >> "$results/${names[$i]}-$s.json" 2>/dev/null
        done
    done
    echo "round $r of $rounds complete" >&2
done
echo >&2

python3 - "$results" "$rounds" "${names[@]}" <<'PY'
import json, os, statistics, sys

results, rounds, *names = sys.argv[1], sys.argv[2], *sys.argv[3:]

SPECS = {
    "keepalive": [("req/s", "req/s", 1),
                  ("server cpu ns per op", "server cpu ns/req", -1),
                  ("latency p50 ns", "p50 ns", -1),
                  ("latency p99 ns", "p99 ns", -1)],
    "stream":    [("fragment/s", "fragment/s", 1),
                  ("server cpu ns per op", "cpu ns/fragment", -1),
                  ("latency p50 ns", "p50 ns", -1)],
    "churn":     [("conn/s", "conn/s", 1),
                  ("server cpu ns per op", "server cpu ns/conn", -1),
                  ("latency p50 ns", "p50 ns", -1),
                  ("latency p99 ns", "p99 ns", -1)],
    "idle":      [("conn/s", "establish conn/s", 1),
                  ("establish cpu ns per conn", "establish cpu ns/conn", -1),
                  ("first-window cpu ns per conn", "first-win cpu ns/conn", -1),
                  ("total cpu ns per conn", "total cpu ns/conn", -1),
                  ("server rss kb per conn", "rss kb/conn", -1),
                  ("idle cpu ns per conn per s (steady)", "steady cpu ns/conn/s", -1)],
}

def rows(name, scenario):
    path = os.path.join(results, f"{name}-{scenario}.json")
    if not os.path.exists(path):
        return []
    with open(path) as fh:
        return [json.loads(l) for l in fh if l.strip().startswith("{")]

def median(rs, key):
    vals = []
    for r in rs:
        try:
            vals.append(float(r[key]))
        except (KeyError, ValueError):
            pass
    return statistics.median(vals) if vals else None

def fmt(v):
    if v is None:
        return "-"
    return f"{v:,.0f}" if abs(v) >= 100 else f"{v:,.2f}"

w = max(14, max(len(n) for n in names) + 2)
print(f"median of {rounds} interleaved rounds\n")
for scenario, metrics in SPECS.items():
    data = {n: rows(n, scenario) for n in names}
    if not any(data.values()):
        continue
    print(f"--- {scenario} ---")
    print("metric".ljust(24) + "".join(n.rjust(w) for n in names) + "   best")
    for key, label, sign in metrics:
        vals = {n: median(data[n], key) for n in names}
        have = {n: v for n, v in vals.items() if v is not None}
        # No winner when everything ties: reporting one would invent a result.
        best = ""
        if have and len(set(have.values())) > 1:
            best = (max if sign > 0 else min)(have, key=have.get)
        elif have:
            best = "(tie)"
        print(label.ljust(24) + "".join(fmt(vals[n]).rjust(w) for n in names)
              + "   " + best)
    print()
print(f"raw json: {results}")
PY

if [ "$clean" = 1 ]; then
    for ref in "$@"; do
        [ "$ref" = "." ] && continue
        git worktree remove "$workdir/$(echo "$ref" | tr '/' '-')" --force 2>/dev/null || true
    done
else
    echo "worktrees kept in $workdir (--clean to remove)" >&2
fi
