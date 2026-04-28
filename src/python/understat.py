import re
import traceback
import warnings
from pathlib import Path
from typing import Optional, Iterable

import numpy as np
import pandas as pd
import soccerdata as sd

# ============================================================
# CONFIG
# ============================================================

OUT_DIR = Path("wc2026_outputs")
OUT_DIR.mkdir(parents=True, exist_ok=True)

CACHE_DIR = Path.home() / "soccerdata"

# Your requested seasons
SEASONS = ["2122", "2223", "2324", "2425", "2526"]

# Season ID -> folder name under wc2026_outputs
SEASON_FOLDERS = {
    "2122": "21-22",
    "2223": "22-23",
    "2324": "23-24",
    "2425": "24-25",
    "2526": "25-26",
}

# Top 5 leagues as individual league IDs
# soccerdata uses IDs like ENG-Premier League, ESP-La Liga, etc.
LEAGUES = [
    "ENG-Premier League",
    "ESP-La Liga",
    "GER-Bundesliga",
    "ITA-Serie A",
    "FRA-Ligue 1",
]

MIN_MINUTES_DEFAULT = 900

# Optional metadata file if Understat output doesn't include nationality/position.
# Understat's league API does NOT provide country/nationality; use this file to add it.
# Expected columns (case-insensitive, flexible):
#   player, team (optional), season (optional), nation, position
PLAYER_METADATA_CSV = Path("player_metadata.csv")

WC_TEAMS_RAW = [
    "Canada", "Mexico", "United States",
    "Argentina", "Brazil", "Colombia", "Equador", "Paraguay", "Uruguay",
    "Austria", "Belgium", "Croatia", "England", "France", "Germany",
    "Netherlands", "Norway", "Portugal", "Scotland", "Spain", "Switzerland",
    "Algeria", "Cape Verde", "Ivory Coast", "Egypt", "Ghana", "Morocco",
    "South Africa", "Senegal", "Tunisia",
    "Australia", "Iran", "Japan", "Korea Republic", "Jordan",
    "Qatar", "Uzbekistan",
    "Curacao", "Haiti", "Panama", "New Zealand",
    "Wales", "Italy", "Sweden", "Poland", "Turkey", "Republic of Ireland",
    "Denmark", "DR Congo", "Bolivia", "Iraq",
]

COUNTRY_ALIASES = {
    "Equador": "Ecuador",
    "Ivory Coast": "Côte d'Ivoire",
    "Korea Republic": "South Korea",
    "Curacao": "Curaçao",
    "USA": "United States",
    "United States of America": "United States",
    "IR Iran": "Iran",
    "Korea, South": "South Korea",
    "Cape Verde Islands": "Cape Verde",
    "Congo DR": "DR Congo",
}

# Example LCQ shortlist nations
DEFAULT_LCQ_NATIONS = ["Italy", "Sweden"]

POSITION_MAP = {
    "GK": "GK",
    "G": "GK",
    "Goalkeeper": "GK",

    "D": "DEF",
    "DF": "DEF",
    "CB": "DEF",
    "LB": "DEF",
    "RB": "DEF",
    "LWB": "DEF",
    "RWB": "DEF",
    "Defender": "DEF",

    "M": "MID",
    "MF": "MID",
    "CM": "MID",
    "DM": "MID",
    "CDM": "MID",
    "CAM": "MID",
    "AM": "MID",
    "LM": "MID",
    "RM": "MID",
    "Midfielder": "MID",

    "F": "FWD",
    "FW": "FWD",
    "ST": "FWD",
    "CF": "FWD",
    "LF": "FWD",
    "RF": "FWD",
    "LW": "ATT",
    "RW": "ATT",
    "W": "ATT",
    "Forward": "FWD",
    "Attacker": "ATT",
}


# ============================================================
# HELPERS
# ============================================================

def canon_country(name: object) -> Optional[str]:
    if pd.isna(name):
        return None
    s = str(name).strip()
    return COUNTRY_ALIASES.get(s, s)


def build_wc_team_set() -> set[str]:
    return {canon_country(x) for x in WC_TEAMS_RAW if canon_country(x)}


def flatten_columns(df: pd.DataFrame) -> pd.DataFrame:
    if isinstance(df.columns, pd.MultiIndex):
        df = df.copy()
        df.columns = [
            "_".join(str(x) for x in col if str(x) != "nan").strip("_")
            for col in df.columns
        ]
    return df


def clean_strings(df: pd.DataFrame) -> pd.DataFrame:
    df = df.copy()
    for col in df.columns:
        if df[col].dtype == "object":
            df[col] = df[col].astype(str).str.strip()
            df.loc[df[col].isin(["nan", "None", ""]), col] = np.nan
    return df


def safe_numeric(df: pd.DataFrame, cols: Iterable[str]) -> pd.DataFrame:
    df = df.copy()
    for col in cols:
        if col in df.columns:
            df[col] = pd.to_numeric(df[col], errors="coerce")
    return df


def find_col(df: pd.DataFrame, candidates: list[str]) -> Optional[str]:
    cols = list(df.columns)
    lower_map = {str(c).lower(): c for c in cols}

    for c in candidates:
        if c in cols:
            return c
    for c in candidates:
        if c.lower() in lower_map:
            return lower_map[c.lower()]
    return None


def first_existing(df: pd.DataFrame, candidates: list[str], default=np.nan) -> pd.Series:
    col = find_col(df, candidates)
    if col is None:
        return pd.Series([default] * len(df), index=df.index)
    return df[col]


def safe_series(df: pd.DataFrame, *candidate_names: str, default: float = 0.0) -> pd.Series:
    """Return the first existing column as a numeric series, or a constant series if none exist."""
    col = find_col(df, list(candidate_names))
    if col is not None:
        return pd.to_numeric(df[col], errors="coerce").fillna(default)
    return pd.Series(default, index=df.index)


def normalize_position(value: object) -> Optional[str]:
    if pd.isna(value):
        return None
    s = str(value).strip()

    # Handle combos like "FW,MF" or "AM/ST"
    tokens = re.split(r"[,/]| and ", s)
    tokens = [t.strip() for t in tokens if t.strip()]

    mapped = [POSITION_MAP.get(tok, tok) for tok in tokens]
    if not mapped:
        return None

    # Priority: GK > ATT/FWD > MID > DEF
    priority = ["GK", "ATT", "FWD", "MID", "DEF"]
    for p in priority:
        if p in mapped:
            return p

    return mapped[0]


def add_per90(df: pd.DataFrame, numerators: list[str], minutes_col: str = "minutes") -> pd.DataFrame:
    df = df.copy()
    if minutes_col not in df.columns:
        return df

    mins = pd.to_numeric(df[minutes_col], errors="coerce")
    for col in numerators:
        if col in df.columns:
            df[f"{col}_per90"] = np.where(mins > 0, pd.to_numeric(df[col], errors="coerce") * 90 / mins, np.nan)

    return df


# ============================================================
# UNDERSTAT LOAD
# ============================================================

def init_understat() -> sd.Understat:
    warnings.filterwarnings("ignore", category=UserWarning)
    us = sd.Understat(
        leagues=LEAGUES,
        seasons=SEASONS,
        data_dir=CACHE_DIR,
        no_cache=False,
        no_store=False,
    )
    return us


def load_understat_player_season_stats(us: sd.Understat) -> pd.DataFrame:
    print("Loading Understat player season stats...")
    df = us.read_player_season_stats()
    df = flatten_columns(df).reset_index()
    df = clean_strings(df)
    return df


# ============================================================
# OPTIONAL METADATA MERGE
# ============================================================

def load_optional_player_metadata(path: Path) -> Optional[pd.DataFrame]:
    if not path.exists():
        return None

    meta = pd.read_csv(path)
    meta = flatten_columns(meta)
    meta = clean_strings(meta)

    rename_map = {}
    for src, dst in [
        ("Player", "player"),
        ("player_name", "player"),
        ("name", "player"),
        ("Team", "team"),
        ("club", "team"),
        ("Season", "season"),
        ("Nation", "nation"),
        ("country", "nation"),
        ("Position", "position_raw"),
        ("pos", "position_raw"),
    ]:
        if src in meta.columns:
            rename_map[src] = dst

    meta = meta.rename(columns=rename_map)

    needed = ["player", "nation", "position_raw"]
    present = [c for c in needed if c in meta.columns]
    if "player" not in present:
        raise ValueError("player_metadata.csv must include a player column.")

    if "nation" in meta.columns:
        meta["nation"] = meta["nation"].map(canon_country)

    if "position_raw" in meta.columns:
        meta["position_group"] = meta["position_raw"].map(normalize_position)

    return meta


# ============================================================
# STANDARDIZE UNDERSTAT TABLE
# ============================================================

def standardize_understat_player_stats(df: pd.DataFrame) -> pd.DataFrame:
    df = df.copy()

    # Common structural columns
    rename_map = {}

    for src, dst in [
        ("league", "league"),
        ("season", "season"),
        ("team", "team"),
        ("player", "player"),
        ("player_name", "player"),
        ("name", "player"),
        ("games", "games"),
        ("time", "minutes"),
        ("minutes", "minutes"),
        ("position", "position_raw"),
        ("pos", "position_raw"),
        ("nation", "nation"),
        ("country", "nation"),
        ("goals", "goals"),
        ("shots", "shots"),
        ("xG", "xg"),
        ("xA", "xa"),
        ("xGChain", "xgchain"),
        ("xGBuildup", "xgbuildup"),
        ("npg", "npg"),
        ("key_passes", "key_passes"),
        ("assists", "assists"),
        ("yellow_cards", "yellow_cards"),
        ("red_cards", "red_cards"),
    ]:
        if src in df.columns:
            rename_map[src] = dst

    df = df.rename(columns=rename_map)

    # If some columns are missing with alternate names, pick them heuristically
    if "player" not in df.columns:
        col = find_col(df, ["player", "player_name", "name"])
        if col:
            df = df.rename(columns={col: "player"})

    if "team" not in df.columns:
        col = find_col(df, ["team", "club", "squad"])
        if col:
            df = df.rename(columns={col: "team"})

    if "league" not in df.columns:
        col = find_col(df, ["league"])
        if col:
            df = df.rename(columns={col: "league"})

    if "season" not in df.columns:
        col = find_col(df, ["season"])
        if col:
            df = df.rename(columns={col: "season"})

    if "minutes" not in df.columns:
        col = find_col(df, ["time", "minutes", "min"])
        if col:
            df = df.rename(columns={col: "minutes"})

    if "xg" not in df.columns:
        col = find_col(df, ["xG", "xg"])
        if col:
            df = df.rename(columns={col: "xg"})

    if "xa" not in df.columns:
        col = find_col(df, ["xA", "xa"])
        if col:
            df = df.rename(columns={col: "xa"})

    if "xgchain" not in df.columns:
        col = find_col(df, ["xGChain", "xgchain", "xg_chain"])
        if col:
            df = df.rename(columns={col: "xgchain"})

    if "xgbuildup" not in df.columns:
        col = find_col(df, ["xGBuildup", "xgbuildup", "xg_buildup"])
        if col:
            df = df.rename(columns={col: "xgbuildup"})

    if "npg" not in df.columns:
        col = find_col(df, ["npg", "non_penalty_goals"])
        if col:
            df = df.rename(columns={col: "npg"})

    if "position_raw" not in df.columns:
        col = find_col(df, ["position", "pos"])
        if col:
            df = df.rename(columns={col: "position_raw"})

    if "nation" not in df.columns:
        col = find_col(df, ["nation", "country"])
        if col:
            df = df.rename(columns={col: "nation"})

    # Understat league API does not provide nationality; ensure column exists for metadata merge / manual fill
    if "nation" not in df.columns:
        df["nation"] = np.nan

    numeric_cols = [
        "games", "minutes", "goals", "shots", "xg", "xa", "xgchain",
        "xgbuildup", "npg", "key_passes", "assists", "yellow_cards", "red_cards"
    ]
    df = safe_numeric(df, numeric_cols)

    if "nation" in df.columns:
        df["nation"] = df["nation"].map(canon_country)

    if "position_raw" in df.columns:
        df["position_group"] = df["position_raw"].map(normalize_position)
    else:
        df["position_group"] = np.nan

    # Derived columns
    if "xg" not in df.columns:
        df["xg"] = np.nan
    if "xa" not in df.columns:
        df["xa"] = np.nan
    if "npg" not in df.columns:
        df["npg"] = np.nan

    df["xg_plus_xa"] = df[["xg", "xa"]].sum(axis=1, min_count=1)
    df["npg_plus_xa"] = df[["npg", "xa"]].sum(axis=1, min_count=1)

    df = add_per90(
        df,
        numerators=["xg", "xa", "xgchain", "xgbuildup", "npg", "goals", "shots", "key_passes", "assists"],
        minutes_col="minutes",
    )

    return df


def merge_optional_metadata(df: pd.DataFrame, meta: Optional[pd.DataFrame]) -> pd.DataFrame:
    if meta is None:
        return df

    out = df.copy()

    # Preferred merge keys
    merge_keys = ["player"]
    if "team" in out.columns and "team" in meta.columns:
        merge_keys.append("team")
    if "season" in out.columns and "season" in meta.columns:
        merge_keys.append("season")

    keep_cols = merge_keys + [c for c in ["nation", "position_raw", "position_group"] if c in meta.columns]
    meta2 = meta[keep_cols].drop_duplicates()

    out = out.merge(meta2, on=merge_keys, how="left", suffixes=("", "_meta"))

    if "nation_meta" in out.columns:
        out["nation"] = out["nation"].combine_first(out["nation_meta"]) if "nation" in out.columns else out["nation_meta"]
        out = out.drop(columns=["nation_meta"])

    if "position_raw_meta" in out.columns:
        if "position_raw" in out.columns:
            out["position_raw"] = out["position_raw"].combine_first(out["position_raw_meta"])
        else:
            out["position_raw"] = out["position_raw_meta"]
        out = out.drop(columns=["position_raw_meta"])

    if "position_group_meta" in out.columns:
        out["position_group"] = out["position_group"].combine_first(out["position_group_meta"])
        out = out.drop(columns=["position_group_meta"])

    return out


# ============================================================
# FILTERING + RANKING
# ============================================================

def filter_wc_players(df: pd.DataFrame, min_minutes: int = MIN_MINUTES_DEFAULT) -> pd.DataFrame:
    out = df.copy()
    wc_set = build_wc_team_set()

    if "minutes" in out.columns:
        out = out[out["minutes"].fillna(0) >= min_minutes]

    if "nation" in out.columns and out["nation"].notna().any():
        out = out[out["nation"].isin(wc_set)]

    return out.reset_index(drop=True)


def compute_position_score(df: pd.DataFrame) -> pd.DataFrame:
    out = df.copy()
    out["position_group"] = out["position_group"].fillna("UNK")
    out["rank_score"] = np.nan

    # Support both naming styles: xg_chain_per90 / xgchain_per90, xg_buildup_per90 / xgbuildup_per90
    xg_bu = safe_series(out, "xg_buildup_per90", "xgbuildup_per90")
    xg_ch = safe_series(out, "xg_chain_per90", "xgchain_per90")
    xa_90 = safe_series(out, "xa_per90")
    xg_90 = safe_series(out, "xg_per90")
    mins = out["minutes"].fillna(0) if "minutes" in out.columns else pd.Series(0.0, index=out.index)

    mask_gk = out["position_group"].eq("GK")
    out.loc[mask_gk, "rank_score"] = 0.0

    mask_def = out["position_group"].eq("DEF")
    out.loc[mask_def, "rank_score"] = (
        0.35 * xg_bu + 0.25 * xg_ch + 0.20 * xa_90 + 0.20 * xg_90
    )

    mask_mid = out["position_group"].eq("MID")
    out.loc[mask_mid, "rank_score"] = (
        0.35 * xa_90 + 0.25 * xg_ch + 0.20 * xg_bu + 0.20 * xg_90
    )

    mask_att = out["position_group"].isin(["ATT", "FWD"])
    out.loc[mask_att, "rank_score"] = (
        0.55 * xg_90 + 0.25 * xa_90 + 0.20 * xg_ch
    )

    mask_unk = out["position_group"].eq("UNK")
    out.loc[mask_unk, "rank_score"] = (
        0.50 * xg_90 + 0.25 * xa_90 + 0.15 * xg_ch + 0.10 * xg_bu
    )

    out["reliability_weight"] = np.log1p(mins)
    out["rank_score_weighted"] = out["rank_score"] * out["reliability_weight"]

    return out


def top_n_per_position(df: pd.DataFrame, nations: list[str], n: int = 2) -> pd.DataFrame:
    out = df.copy()
    nations = [canon_country(x) for x in nations]

    if "nation" in out.columns and out["nation"].notna().any():
        out = out[out["nation"].isin(nations)]

    out = compute_position_score(out)

    sort_cols = [c for c in ["nation", "position_group", "rank_score_weighted", "minutes"] if c in out.columns]
    asc = [True, True, False, False][:len(sort_cols)]
    out = out.sort_values(by=sort_cols, ascending=asc)

    group_cols = [c for c in ["nation", "position_group"] if c in out.columns]
    if not group_cols:
        group_cols = ["position_group"]

    out["rank_within_group"] = out.groupby(group_cols).cumcount() + 1
    out = out[out["rank_within_group"] <= n].reset_index(drop=True)
    return out


# ============================================================
# MAIN
# ============================================================

def main():
    print("Initializing Understat reader...")
    us = init_understat()

    print("Reading Understat player season stats...")
    raw = load_understat_player_season_stats(us)
    print(f"Raw rows pulled: {len(raw):,}")

    print("Standardizing columns...")
    player_df = standardize_understat_player_stats(raw)

    # Save a column inspection file so you can see exactly what Understat returned
    cols_path = OUT_DIR / "understat_player_season_columns.txt"
    with open(cols_path, "w", encoding="utf-8") as f:
        for c in player_df.columns:
            f.write(f"{c}\n")
    print(f"Saved column list to: {cols_path}")

    # Optional metadata merge if nationality / position are missing or incomplete
    meta = load_optional_player_metadata(PLAYER_METADATA_CSV)
    if meta is not None:
        print("Merging optional player metadata...")
        player_df = merge_optional_metadata(player_df, meta)

    # Save full standardized dataset (combined, all seasons)
    full_path = OUT_DIR / "understat_big5_player_season_2122_2526_full.csv"
    player_df.to_csv(full_path, index=False)
    print(f"Saved full dataset to: {full_path}")

    # WC filter (combined)
    wc_df = filter_wc_players(player_df, min_minutes=MIN_MINUTES_DEFAULT)
    wc_path = OUT_DIR / "understat_big5_player_season_2122_2526_wc_filtered.csv"
    wc_df.to_csv(wc_path, index=False)
    print(f"Saved WC-filtered dataset to: {wc_path}")

    # Per-season folders: 21-22, 22-23, 23-24, 24-25, 25-26 (all leagues for that season)
    if "season" in player_df.columns:
        season_col = player_df["season"].astype(str).str.strip()
        for sid, folder_name in SEASON_FOLDERS.items():
            season_dir = OUT_DIR / folder_name
            season_dir.mkdir(parents=True, exist_ok=True)
            mask = season_col == sid
            if mask.any():
                season_full = player_df.loc[mask]
                season_full.to_csv(season_dir / "understat_full.csv", index=False)
                season_wc = filter_wc_players(season_full, min_minutes=MIN_MINUTES_DEFAULT)
                season_wc.to_csv(season_dir / "understat_wc_filtered.csv", index=False)
                print(f"Saved {folder_name}: full={len(season_full):,} rows, WC-filtered={len(season_wc):,} rows")
            else:
                print(f"[WARN] No data for season {folder_name} ({sid})")

    # Example LCQ shortlist (skip on error so main CSVs are still saved)
    try:
        lcq_df = top_n_per_position(wc_df, nations=DEFAULT_LCQ_NATIONS, n=2)
        lcq_path = OUT_DIR / "understat_lcq_top2_per_position_example.csv"
        lcq_df.to_csv(lcq_path, index=False)
        print(f"Saved LCQ shortlist to: {lcq_path}")
    except Exception as e:
        print(f"[WARN] Could not build LCQ shortlist: {e}")

    # Helpful debug previews
    try:
        print("\n=== Preview: first 10 rows ===")
        preview_cols = [c for c in [
            "league", "season", "team", "player", "nation", "position_raw", "position_group",
            "games", "minutes", "xg", "xa", "xgchain", "xgbuildup", "xg_chain", "xg_buildup",
            "xg_per90", "xa_per90", "rank_score_weighted"
        ] if c in wc_df.columns]
        if preview_cols:
            print(wc_df[preview_cols].head(10).to_string(index=False))
        else:
            print("No preview columns found.")
    except Exception as e:
        print(f"[WARN] Could not show preview: {e}")

    if "nation" not in player_df.columns or player_df["nation"].isna().all():
        print("\n[WARN] Understat does not provide nationality/country data.")
        print("       The 'nation' column is in the CSV but will be empty unless you add it via player_metadata.csv")
        print("       with columns: player,nation,position  (optional: team,season)")

    if "position_group" not in player_df.columns or player_df["position_group"].isna().all():
        print("\n[WARN] Understat output did not provide usable position data.")
        print("       Add position info in player_metadata.csv to improve top-2-per-position results.")


if __name__ == "__main__":
    try:
        main()
    except Exception as e:
        print(f"\n[ERROR] {e}")
        traceback.print_exc()
        raise SystemExit(1)