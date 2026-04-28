"""
Match Understat-style player display names to Transfermarkt `active_players` rows.

Handles nicknames vs formal names (e.g. Matty Cash vs Matthew Cash) by:
1. Normalizing unicode / accents / spacing
2. Canonicalizing nation labels between sources
3. Scoping fuzzy match (WRatio) to players with the same citizenship when possible
4. Optional manual overrides via CSV

Usage:
    from pathlib import Path
    import pandas as pd
    from transfermarkt_match import merge_understat_with_transfermarkt

    u = pd.read_csv("wc2026_outputs/understat_big5_player_season_2122_2526_full_merged_intl.csv")
    tm = pd.read_csv("transfermarkt_data/active_players_2025_nation_filtered.csv")
    merged, report = merge_understat_with_transfermarkt(u, tm)
    merged.to_csv("wc2026_outputs/understat_tm_merged.csv", index=False)
"""

from __future__ import annotations

import re
import unicodedata
from pathlib import Path
from typing import Any, Dict, List, Optional, Tuple

import pandas as pd

try:
    from rapidfuzz import fuzz, process
except ImportError as e:  # pragma: no cover
    raise ImportError(
        "transfermarkt_match requires rapidfuzz. Install with: pip install rapidfuzz"
    ) from e

# --- Nation labels: align Understat `nation` with TM `country_of_citizenship` ---
NATION_CANON: Dict[str, str] = {
    "united kingdom": "england",
    "northern ireland": "northern ireland",
    "scotland": "scotland",
    "wales": "wales",
    "bosnia and herzegovina": "bosnia-herzegovina",
    "bosnia-herzegovina": "bosnia-herzegovina",
    "czech republic": "czech republic",
    "czechia": "czech republic",
    "cote divoire": "cote d'ivoire",
    "côte d'ivoire": "cote d'ivoire",
    "cote d'ivoire": "cote d'ivoire",
    "democratic republic of the congo": "dr congo",
    "congo": "dr congo",
    "south korea": "korea, south",
    "korea republic": "korea, south",
    "korea, south": "korea, south",
    "usa": "united states",
    "united states of america": "united states",
    "turkey": "türkiye",
    "türkiye": "türkiye",
    "curaçao": "curacao",
    "curacao": "curacao",
}


def normalize_player_name(name: Any) -> str:
    if name is None or (isinstance(name, float) and pd.isna(name)):
        return ""
    s = str(name).strip()
    s = unicodedata.normalize("NFKD", s)
    s = "".join(c for c in s if not unicodedata.combining(c))
    s = s.lower().strip()
    s = re.sub(r"\s+", " ", s)
    return s


def canonicalize_nation(nation: Any) -> str:
    if nation is None or (isinstance(nation, float) and pd.isna(nation)):
        return ""
    s = normalize_player_name(nation)
    s = s.replace(".", "")
    return NATION_CANON.get(s, s)


def _tm_display_name(row: pd.Series) -> str:
    n = row.get("name")
    if pd.notna(n) and str(n).strip():
        return str(n).strip()
    parts = []
    for c in ("first_name", "last_name"):
        v = row.get(c)
        if pd.notna(v) and str(v).strip():
            parts.append(str(v).strip())
    return " ".join(parts)


def load_manual_overrides(path: Path) -> Dict[Tuple[str, str], int]:
    """CSV columns: understat_player, nation (optional), tm_player_id"""
    if not path.exists():
        return {}
    o = pd.read_csv(path)
    o.columns = [c.strip().lower() for c in o.columns]
    out: Dict[Tuple[str, str], int] = {}
    for _, r in o.iterrows():
        u = normalize_player_name(r.get("understat_player", ""))
        nat = canonicalize_nation(r.get("nation", ""))
        pid = r.get("tm_player_id")
        if pd.isna(pid):
            continue
        out[(u, nat)] = int(pid)
        out[(u, "")] = int(pid)
    return out


def merge_understat_with_transfermarkt(
    understat_df: pd.DataFrame,
    tm_df: pd.DataFrame,
    *,
    understat_player_col: str = "player",
    understat_nation_col: str = "nation",
    tm_name_col: str = "name",
    tm_nation_col: str = "country_of_citizenship",
    tm_id_col: str = "player_id",
    score_cutoff_nation_scoped: float = 78.0,
    score_cutoff_global: float = 78.0,
    overrides_path: Optional[Path] = None,
    replace_nation_with_tm: bool = True,
    understat_nation_backup_col: Optional[str] = "nation_understat",
) -> Tuple[pd.DataFrame, Dict[str, Any]]:
    """
    Returns merged DataFrame (all understat columns + tm_* columns) and a small report dict.

    When ``replace_nation_with_tm`` is True (default), the Understat ``nation`` column is
    overwritten with Transfermarkt ``country_of_citizenship`` for every row that matched a
    TM player. The previous Understat value is kept in ``nation_understat`` (unless
    ``understat_nation_backup_col`` is None). Rows with no TM match keep the original
    ``nation``.
    """
    overrides_path = overrides_path or Path("transfermarkt_data/player_tm_overrides.csv")
    manual = load_manual_overrides(overrides_path)

    tm = tm_df.copy()
    tm["_tm_display"] = tm.apply(_tm_display_name, axis=1)
    tm["_tm_norm"] = tm["_tm_display"].map(normalize_player_name)
    tm["_tm_nation"] = tm[tm_nation_col].map(canonicalize_nation)

    # Index TM rows by nation for faster scoped matching
    by_nation: Dict[str, List[int]] = {}
    for i, row in tm.iterrows():
        nat = row["_tm_nation"]
        by_nation.setdefault(nat, []).append(i)

    u = understat_df.copy()
    u["_u_norm"] = u[understat_player_col].map(normalize_player_name)
    u["_u_nation"] = u[understat_nation_col].map(canonicalize_nation)

    def pick_tm_row(norm_name: str, nation_key: str) -> Tuple[Optional[int], float, Optional[str]]:
        key = (norm_name, nation_key)
        if key in manual:
            pid = manual[key]
            hit = tm.loc[tm[tm_id_col] == pid]
            if len(hit):
                lab = hit.iloc[0]["_tm_display"]
                return int(pid), 100.0, str(lab)
        key_any = (norm_name, "")
        if key_any in manual:
            pid = manual[key_any]
            hit = tm.loc[tm[tm_id_col] == pid]
            if len(hit):
                lab = hit.iloc[0]["_tm_display"]
                return int(pid), 100.0, str(lab)

        # Exact normalized full name (whole TM table)
        exact = tm.loc[tm["_tm_norm"] == norm_name]
        if len(exact) == 1:
            r = exact.iloc[0]
            if not nation_key or r["_tm_nation"] == nation_key or not r["_tm_nation"]:
                return int(r[tm_id_col]), 100.0, r["_tm_display"]
        if len(exact) > 1 and nation_key:
            sub = exact[exact["_tm_nation"] == nation_key]
            if len(sub) == 1:
                r = sub.iloc[0]
                return int(r[tm_id_col]), 100.0, r["_tm_display"]

        # Prefer same citizenship (Understat `nation` vs TM citizenship), but allow
        # dual-national / labelling mismatches via global fallback below.
        choices_idx = by_nation.get(nation_key, []) if nation_key else []
        if choices_idx:
            labels = tm.loc[choices_idx, "_tm_norm"].tolist()
            m = process.extractOne(
                norm_name,
                labels,
                scorer=fuzz.WRatio,
                score_cutoff=score_cutoff_nation_scoped,
            )
            if m:
                _match_name, score, pos = m
                orig_i = choices_idx[pos]
                r = tm.loc[orig_i]
                return int(r[tm_id_col]), float(score), r["_tm_display"]

        # Global fallback (e.g. nickname vs formal name when nations disagree, or
        # player missing from nation-filtered TM — use full active_players for coverage)
        all_norm = tm["_tm_norm"].tolist()
        all_idx = tm.index.tolist()
        m = process.extractOne(
            norm_name,
            all_norm,
            scorer=fuzz.WRatio,
            score_cutoff=score_cutoff_global,
        )
        if m:
            _match_name, score, pos = m
            orig_i = all_idx[pos]
            r = tm.loc[orig_i]
            return int(r[tm_id_col]), float(score), r["_tm_display"]

        return None, 0.0, None

    # Unique logical players (name + nation)
    keys = u.groupby([understat_player_col, understat_nation_col], dropna=False).size().reset_index()[
        [understat_player_col, understat_nation_col]
    ]

    def _row_key(pname: Any, nn: Any) -> Tuple[str, str]:
        return (str(pname), "" if pd.isna(nn) else nn)

    lookup: Dict[Tuple[str, str], Tuple[Optional[int], float, Optional[str]]] = {}
    for _, row in keys.iterrows():
        pname = row[understat_player_col]
        nn = row[understat_nation_col]
        norm = normalize_player_name(pname)
        nat = canonicalize_nation(nn)
        lookup[_row_key(pname, nn)] = pick_tm_row(norm, nat)

    tm_cols = [c for c in tm_df.columns if c not in ("_tm_display", "_tm_norm", "_tm_nation")]
    prefix = "tm_"
    rename_tm = {c: f"{prefix}{c}" for c in tm_cols}

    rows_out = []
    for _, row in u.iterrows():
        k = _row_key(row[understat_player_col], row[understat_nation_col])
        pid, score, tm_label = lookup[k]
        base = row.to_dict()
        if pid is None:
            for c in tm_cols:
                base[rename_tm[c]] = pd.NA
            base[f"{prefix}match_score"] = pd.NA
            base[f"{prefix}matched_name"] = pd.NA
        else:
            hit = tm.loc[tm[tm_id_col] == pid].iloc[0]
            for c in tm_cols:
                base[rename_tm[c]] = hit[c]
            base[f"{prefix}match_score"] = score
            base[f"{prefix}matched_name"] = tm_label
        rows_out.append(base)

    merged = pd.DataFrame(rows_out)
    merged = merged.drop(columns=["_u_norm", "_u_nation"], errors="ignore")

    tm_nat_col = f"{prefix}{tm_nation_col}"
    tm_pid_col = f"{prefix}{tm_id_col}"
    if replace_nation_with_tm and understat_nation_col in merged.columns and tm_nat_col in merged.columns:
        if understat_nation_backup_col:
            merged[understat_nation_backup_col] = merged[understat_nation_col]
        matched_mask = merged[tm_pid_col].notna()
        merged.loc[matched_mask, understat_nation_col] = merged.loc[matched_mask, tm_nat_col].values

    matched = int(merged[tm_pid_col].notna().sum())
    total = len(merged)
    uniq = len(keys)
    matched_u = sum(1 for v in lookup.values() if v[0] is not None)

    report = {
        "rows_total": total,
        "rows_matched": matched,
        "unique_player_nation_pairs": uniq,
        "unique_pairs_matched": matched_u,
        "overrides_file": str(overrides_path) if overrides_path else None,
    }
    return merged, report


def unmatched_pairs(
    merged: pd.DataFrame,
    *,
    tm_id_col: str = "tm_player_id",
    player_col: str = "player",
    nation_col: str = "nation",
) -> pd.DataFrame:
    """Rows where TM match failed (one row per understat row)."""
    return merged.loc[merged[tm_id_col].isna(), [player_col, nation_col]].drop_duplicates()
