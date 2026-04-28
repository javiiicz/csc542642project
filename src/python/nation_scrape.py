import time
import requests
import pandas as pd
from pathlib import Path

WIKIDATA_API = "https://www.wikidata.org/w/api.php"
WIKIDATA_SPARQL = "https://query.wikidata.org/sparql"

HEADERS = {
    "User-Agent": "wc2026-research/1.0 (contact: somakshivlani@gmail.com)"
}

CACHE_PATH = Path("player_country_map.csv")

def wikidata_search_player(name: str, limit: int = 5):
    params = {
        "action": "wbsearchentities",
        "format": "json",
        "language": "en",
        "uselang": "en",
        "type": "item",
        "limit": limit,
        "search": name,
    }
    r = requests.get(WIKIDATA_API, params=params, headers=HEADERS, timeout=30)
    r.raise_for_status()
    data = r.json()
    return data.get("search", [])

def get_country_from_qid(qid: str):
    query = f"""
    SELECT ?countryLabel WHERE {{
      wd:{qid} wdt:P27 ?country .
      SERVICE wikibase:label {{ bd:serviceParam wikibase:language "en". }}
    }}
    LIMIT 5
    """
    r = requests.get(
        WIKIDATA_SPARQL,
        params={"query": query, "format": "json"},
        headers=HEADERS,
        timeout=30,
    )
    r.raise_for_status()
    data = r.json()
    bindings = data["results"]["bindings"]
    if not bindings:
        return None
    # take first citizenship result
    return bindings[0]["countryLabel"]["value"]

def choose_best_candidate(name: str, candidates: list[dict]):
    """
    Heuristic:
    prefer descriptions that mention football / soccer.
    """
    if not candidates:
        return None

    football_words = [
        "footballer", "soccer player", "association football", "football player"
    ]

    scored = []
    for c in candidates:
        desc = (c.get("description") or "").lower()
        score = 0
        if any(word in desc for word in football_words):
            score += 10
        if name.lower() == (c.get("label") or "").lower():
            score += 3
        scored.append((score, c))

    scored.sort(key=lambda x: x[0], reverse=True)
    return scored[0][1]

def resolve_player_country(name: str, sleep_s: float = 0.2):
    candidates = wikidata_search_player(name)
    best = choose_best_candidate(name, candidates)
    if not best:
        return {
            "player": name,
            "wikidata_id": None,
            "wikidata_label": None,
            "wikidata_description": None,
            "nation": None,
        }

    qid = best["id"]
    country = get_country_from_qid(qid)

    time.sleep(sleep_s)

    return {
        "player": name,
        "wikidata_id": qid,
        "wikidata_label": best.get("label"),
        "wikidata_description": best.get("description"),
        "nation": country,
    }

def build_or_update_country_map(df: pd.DataFrame, player_col: str = "player") -> pd.DataFrame:
    if player_col not in df.columns:
        raise ValueError(f"Column '{player_col}' not found in dataframe.")

    players = sorted(df[player_col].dropna().astype(str).str.strip().unique())

    if CACHE_PATH.exists():
        cache = pd.read_csv(CACHE_PATH)
    else:
        cache = pd.DataFrame(columns=[
            "player", "wikidata_id", "wikidata_label", "wikidata_description", "nation"
        ])

    done_players = set(cache["player"].astype(str)) if not cache.empty else set()
    new_players = [p for p in players if p not in done_players]

    rows = []
    for i, player in enumerate(new_players, start=1):
        print(f"[{i}/{len(new_players)}] Resolving {player}")
        try:
            rows.append(resolve_player_country(player))
        except Exception as e:
            rows.append({
                "player": player,
                "wikidata_id": None,
                "wikidata_label": None,
                "wikidata_description": f"ERROR: {e}",
                "nation": None,
            })

    if rows:
        new_df = pd.DataFrame(rows)
        cache = pd.concat([cache, new_df], ignore_index=True)
        cache.to_csv(CACHE_PATH, index=False)

    return cache

def merge_country_map(df: pd.DataFrame, country_map: pd.DataFrame, player_col: str = "player") -> pd.DataFrame:
    merged = df.merge(
        country_map[["player", "nation", "wikidata_id"]],
        left_on=player_col,
        right_on="player",
        how="left",
        suffixes=("", "_map"),
    )
    if "player_map" in merged.columns:
        merged = merged.drop(columns=["player_map"])
    return merged

# Example usage:
df = pd.read_csv("wc2026_outputs/understat_big5_player_season_2122_2526_wc_filtered.csv")
country_map = build_or_update_country_map(df, player_col="player")
df = merge_country_map(df, country_map, player_col="player")
print(df[["player", "nation"]].head())