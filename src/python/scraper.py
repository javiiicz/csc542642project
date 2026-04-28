import time
import re
import hashlib
import random
import atexit
import os
from io import StringIO
from pathlib import Path
from typing import Dict, List, Tuple, Optional

import pandas as pd
import requests
from requests.adapters import HTTPAdapter
from urllib3.util.retry import Retry
from bs4 import BeautifulSoup

# Optional: use Playwright when requests get 403 (FBref often blocks plain HTTP)
try:
    from playwright.sync_api import sync_playwright
    _PLAYWRIGHT_AVAILABLE = True
except ImportError:
    sync_playwright = None  # type: ignore
    _PLAYWRIGHT_AVAILABLE = False

_playwright_state: Optional[dict] = None

# -----------------------------
# Config
# -----------------------------

CACHE_DIR = Path("cache_fbref")
CACHE_DIR.mkdir(exist_ok=True)

# Browser-like headers so FBref doesn't return 403 (they block script User-Agents).
# For research / polite scraping; identify in a comment or minimal header if desired.
HEADERS = {
    "User-Agent": (
        "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) "
        "AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"
    ),
    "Accept": "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8",
    "Accept-Language": "en-US,en;q=0.9",
    "Accept-Encoding": "gzip, deflate, br",
    "Referer": "https://fbref.com/",
    "DNT": "1",
    "Connection": "keep-alive",
    "Upgrade-Insecure-Requests": "1",
    "Sec-Fetch-Dest": "document",
    "Sec-Fetch-Mode": "navigate",
    "Sec-Fetch-Site": "none",
    "Sec-Fetch-User": "?1",
}

# Your requested 5 seasons: 21-22 .. 25-26
YEARS = [2021, 2022, 2023, 2024, 2025]

TOP_5_LEAGUES = ["Premier League", "La Liga", "Bundesliga", "Serie A", "Ligue 1"]

# FBref competition IDs (used in /en/comps/<id>/)
FBREF_LEAGUE_IDS = {
    "Premier League": 9,
    "La Liga": 12,
    "Bundesliga": 20,
    "Serie A": 11,
    "Ligue 1": 13,
}

FBREF_BASE_URL = "https://fbref.com"

# WC 2026 qualified teams by confederation
WC_QUALIFIED_TEAMS_RAW = [
    # Hosts
    "Canada", "Mexico", "United States",
    # UEFA (Europe)
    "England", "France", "Germany", "Spain", "Portugal", "Netherlands",
    "Croatia", "Austria", "Norway", "Scotland", "Switzerland", "Türkiye",
    "Sweden", "Bosnia and Herzegovina", "Czechia",
    # CONMEBOL (South America)
    "Argentina", "Brazil", "Colombia", "Ecuador", "Paraguay", "Uruguay",
    # AFC (Asia)
    "Japan", "Iran", "South Korea", "Qatar", "Saudi Arabia", "Australia",
    "Uzbekistan", "Jordan", "Iraq",
    # CAF (Africa)
    "Morocco", "Senegal", "Algeria", "Egypt", "Côte d'Ivoire", "Nigeria",
    "Tunisia", "Cameroon", "Mali", "DR Congo",
    # CONCACAF (North/Central America & Caribbean)
    "Panama", "Haiti", "Curaçao",
    # OFC (Oceania)
    "New Zealand",
]

COUNTRY_ALIASES = {
    # Common alternate spellings that may appear in data sources
    "United States": "United States",
    "USA": "United States",
    "Ivory Coast": "Côte d'Ivoire",
    "Korea Republic": "South Korea",
    "Republic of Korea": "South Korea",
    "Turkey": "Türkiye",
    "Curacao": "Curaçao",
    "Bosnia-Herzegovina": "Bosnia and Herzegovina",
    "Czech Republic": "Czechia",
    "Congo DR": "DR Congo",
    "Equador": "Ecuador",
}

def canonical_country(name: str) -> str:
    name = name.strip()
    return COUNTRY_ALIASES.get(name, name)

# De-duplicate and canonicalize your WC list
WC_TEAMS = sorted({canonical_country(x) for x in WC_QUALIFIED_TEAMS_RAW})

# -----------------------------
# HTTP session with retries
# -----------------------------

def make_session() -> requests.Session:
    s = requests.Session()
    # By default, ignore HTTP(S)_PROXY env vars for FBref calls.
    # Some dev setups point these at a local proxy that blocks FBref (403 tunnel errors).
    # If you truly need a corporate proxy, set FBREF_USE_SYSTEM_PROXY=1.
    s.trust_env = os.getenv("FBREF_USE_SYSTEM_PROXY", "").strip() in {"1", "true", "TRUE", "yes", "YES"}
    retry = Retry(
        total=6,
        connect=6,
        read=6,
        backoff_factor=1.3,
        status_forcelist=[429, 500, 502, 503, 504],
        allowed_methods=["GET"],
        raise_on_status=False,
    )
    s.mount("https://", HTTPAdapter(max_retries=retry))
    s.headers.update(HEADERS)
    return s

SESSION = make_session()

def url_to_cache_name(url: str) -> str:
    h = hashlib.sha1(url.encode("utf-8")).hexdigest()[:16]
    return f"{h}.html"


def _fetch_with_playwright(url: str, timeout_ms: int = 30000) -> str:
    """Fetch page HTML using a real browser. Used when requests returns 403."""
    if not _PLAYWRIGHT_AVAILABLE:
        raise RuntimeError(
            "FBref returned 403. Install Playwright and install browser: "
            "pip install playwright && playwright install chromium"
        )
    global _playwright_state
    if _playwright_state is None:
        pw = sync_playwright().start()
        browser = pw.chromium.launch(headless=True)
        context = browser.new_context(
            user_agent=HEADERS["User-Agent"],
            locale="en-US",
        )
        context.set_extra_http_headers({
            "Accept": HEADERS["Accept"],
            "Accept-Language": HEADERS["Accept-Language"],
        })
        page = context.new_page()
        _playwright_state = {"playwright": pw, "browser": browser, "context": context, "page": page}

        def _close():
            if _playwright_state:
                try:
                    _playwright_state["browser"].close()
                    _playwright_state["playwright"].stop()
                except Exception:
                    pass
        atexit.register(_close)

    page = _playwright_state["page"]
    page.goto(url, wait_until="domcontentloaded", timeout=timeout_ms)
    try:
        page.wait_for_load_state("networkidle", timeout=15000)
    except Exception:
        pass  # use whatever we have after domcontentloaded
    html = page.content()
    return html


def fetch_cached(url: str, sleep_s: float = 4.5) -> str:
    cache_name = url_to_cache_name(url)
    path = CACHE_DIR / cache_name
    if path.exists():
        html = path.read_text(encoding="utf-8")
        # Reject cache if it's a Cloudflare challenge page (so we re-fetch, e.g. via Playwright)
        if "Just a moment" not in html and "Performing security verification" not in html:
            return html
        path.unlink(missing_ok=True)

    resp = SESSION.get(url, timeout=30)
    if resp.status_code == 403:
        if os.getenv("FBREF_USE_TURNSTILE_BRIDGE", "").strip().lower() in ("1", "true", "yes"):
            try:
                from fbref_turnstile import fetch_fbref_html_with_turnstile

                html = fetch_fbref_html_with_turnstile(url)
                path.write_text(html, encoding="utf-8")
                time.sleep(sleep_s + random.uniform(0.0, 1.5))
                return html
            except Exception as e:
                print(f"[FBREF_USE_TURNSTILE_BRIDGE] 403 path failed: {e}")
        if _PLAYWRIGHT_AVAILABLE:
            html = _fetch_with_playwright(url)
            path.write_text(html, encoding="utf-8")
            time.sleep(sleep_s + random.uniform(0.0, 1.5))
            return html
    if resp.status_code != 200:
        raise RuntimeError(f"Fetch failed {resp.status_code} for {url}")

    html = resp.text

    # Optional: Turnstile bridge (Turnstilesolver-style widget + cookie handoff). See fbref_turnstile.py.
    if os.getenv("FBREF_USE_TURNSTILE_BRIDGE", "").strip().lower() in ("1", "true", "yes"):
        if "Just a moment" in html or "cf-turnstile" in html.lower() or "Performing security verification" in html:
            try:
                from fbref_turnstile import fetch_fbref_html_with_turnstile

                html = fetch_fbref_html_with_turnstile(url)
            except Exception as e:
                print(f"[FBREF_USE_TURNSTILE_BRIDGE] failed: {e}")

    path.write_text(html, encoding="utf-8")

    # polite + jitter
    time.sleep(sleep_s + random.uniform(0.0, 1.5))
    return html

# -----------------------------
# FBref URL helpers
# -----------------------------

def league_to_slug(league_name: str) -> str:
    return league_name.replace(" ", "-")

def league_url_season(league_name: str, year: int) -> str:
    """Season year-(year+1). FBref player stats use /stats/ in the path."""
    comp_id = FBREF_LEAGUE_IDS[league_name]
    slug = league_to_slug(league_name)
    season_str = f"{year}-{year + 1}"
    return f"{FBREF_BASE_URL}/en/comps/{comp_id}/{season_str}/stats/{season_str}-{slug}-Stats"

def all_league_season_urls() -> List[Tuple[str, str, int]]:
    out = []
    for league_name in TOP_5_LEAGUES:
        for year in YEARS:
            out.append((league_url_season(league_name, year), league_name, year))
    return out

# -----------------------------
# Table parsing + cleanup
# -----------------------------

def uncomment_fbref_tables(html: str) -> str:
    # FBref frequently wraps tables in <!-- -->
    return re.sub(r"<!--|-->", "", html)

def extract_player_ids_from_table(html: str, table_id: str = "stats_standard") -> List[Optional[str]]:
    """Parse HTML and return list of FBref player IDs in table row order (tbody only)."""
    html2 = uncomment_fbref_tables(html)
    soup = BeautifulSoup(html2, "html.parser")
    table = soup.find("table", id=table_id)
    if not table:
        return []
    tbody = table.find("tbody")
    if not tbody:
        return []
    player_link_re = re.compile(r"^/en/players/([a-f0-9]+)/")
    ids: List[Optional[str]] = []
    for tr in tbody.find_all("tr"):
        a = tr.find("a", href=player_link_re)
        if a and a.get("href"):
            m = player_link_re.match(a["href"])
            ids.append(m.group(1) if m else None)
        else:
            ids.append(None)
    return ids

def flatten_columns(df: pd.DataFrame) -> pd.DataFrame:
    if isinstance(df.columns, pd.MultiIndex):
        df.columns = [
            "_".join([str(x) for x in col if str(x) != "nan"]).strip("_")
            for col in df.columns
        ]
    return df

def normalize_basic(df: pd.DataFrame) -> pd.DataFrame:
    df = flatten_columns(df).copy()

    # Remove repeated header rows if present
    if "Rk" in df.columns:
        df = df[df["Rk"].astype(str) != "Rk"]

    # Drop fully empty rows
    df = df.dropna(how="all")

    # Strip whitespace in object columns
    for c in df.columns:
        if df[c].dtype == "object":
            df[c] = df[c].astype(str).str.strip()

    return df

def extract_nation_country(nation_field: str) -> str:
    """
    FBref Nation fields often look like:
      'eng ENG' or 'fr FRA' or sometimes just 'England'
    We'll take the last token if it looks like a 3-letter code; otherwise keep the string.
    """
    if not isinstance(nation_field, str):
        return nation_field
    s = nation_field.strip()
    parts = s.split()
    if len(parts) >= 2 and len(parts[-1]) == 3 and parts[-1].isalpha():
        # Unfortunately this is a code, not a full country name.
        # Many tables also include full country elsewhere; if not, you may keep code.
        # We'll keep the whole string for now, and also store a parsed code.
        return s
    return s

def read_table_by_id(html: str, table_id: str) -> pd.DataFrame:
    html2 = uncomment_fbref_tables(html)
    tables = pd.read_html(StringIO(html2), attrs={"id": table_id})
    if not tables:
        raise ValueError(f"Table id '{table_id}' not found.")
    return normalize_basic(tables[0])

def scrape_league_season(url: str) -> Dict[str, pd.DataFrame]:
    """
    Pull the main player tables you’ll want first:
      - stats_standard (minutes, pos, nation, goals/assists, etc.)
      - stats_shooting (shots, xG, npxG etc.)
    """
    html = fetch_cached(url)
    out: Dict[str, pd.DataFrame] = {}

    for tid in ["stats_standard", "stats_shooting"]:
        try:
            out[tid] = read_table_by_id(html, tid)
        except Exception:
            pass

    if "stats_standard" in out:
        ids = extract_player_ids_from_table(html, "stats_standard")
        df_std = out["stats_standard"]
        if len(ids) == len(df_std):
            out["stats_standard"] = df_std.assign(player_id=ids)
        else:
            out["stats_standard"] = df_std.assign(player_id=None)

    return out

# -----------------------------
# Merge + filter (first pass)
# -----------------------------

def merge_standard_shooting(std: pd.DataFrame, sht: pd.DataFrame) -> pd.DataFrame:
    """
    Merge on (Player, Squad) is a first pass.
    Better long-term: extract FBref player ids from links (needs HTML parsing)
    but this gets you running quickly.
    """
    # Common columns often include: Player, Squad, Nation, Pos, Age, 90s, Min
    # Shooting has xG columns.
    key_cols = [c for c in ["Player", "Squad"] if c in std.columns and c in sht.columns]
    if not key_cols:
        raise ValueError("No shared merge keys found between standard and shooting tables.")

    merged = std.merge(
        sht,
        on=key_cols,
        how="left",
        suffixes=("", "_shooting"),
        validate="one_to_one"
    )
    return merged

def filter_to_wc_nations(df: pd.DataFrame) -> pd.DataFrame:
    """
    This is tricky because FBref Nation can be coded.
    If your Nation field is full names, great.
    If it’s codes, you’ll need a code->country map later.
    For now, we attempt to canonicalize string and match your WC_TEAMS list.
    """
    if "Nation" not in df.columns:
        return df

    nation_clean = df["Nation"].astype(str).map(extract_nation_country).map(canonical_country)
    df = df.assign(Nation_Canon=nation_clean)

    # Only keep rows where Nation_Canon matches your set
    return df[df["Nation_Canon"].isin(WC_TEAMS)].copy()

def add_metadata(df: pd.DataFrame, league: str, year: int) -> pd.DataFrame:
    return df.assign(
        League=league,
        SeasonStartYear=year,
        Season=f"{year}-{year+1}"
    )

def one_row_per_player_season(df: pd.DataFrame) -> pd.DataFrame:
    """One row per (player, season): keep row with max league minutes (primary club)."""
    if "Min" not in df.columns or df.empty:
        return df
    df = df.copy()
    df["Min"] = pd.to_numeric(df["Min"], errors="coerce").fillna(0)
    if "player_id" in df.columns and df["player_id"].notna().any():
        group_cols = ["player_id", "SeasonStartYear"]
    else:
        group_cols = ["Player", "Nation_Canon", "SeasonStartYear"]
    idx = df.groupby(group_cols, dropna=False)["Min"].idxmax()
    return df.loc[idx].reset_index(drop=True)

# FBref squad IDs for men's national teams (for country stats by season).
# Add more as needed; missing nations skip country stats.
FBREF_NATION_SQUAD_IDS: Dict[str, str] = {
    "France": "b1b36dcd",
    "Brazil": "304635c3",
    "Argentina": "0e4c6f9c",
    "England": "e2d8892c",
    "Spain": "c6f84d3c",
    "Germany": "4b2a10a0",
    "Portugal": "2b390eca",
    "Netherlands": "e52ad728",
    "Belgium": "7a8f3e20",
    "Croatia": "7b08e376",
    "Italy": "e0652b02",
    "United States": "9a807b42",
    "Mexico": "a2a962e4",
    "Uruguay": "9f306f6a",
    "Japan": "3430a3d2",
    "Morocco": "1c3765d4",
    "Senegal": "2b6d3696",
    "Switzerland": "59fbd94a",
    "Denmark": "1f44c576",
}

def nation_season_url(nation: str, year: int) -> Optional[str]:
    """URL for a national team's stats in a given season."""
    squad_id = FBREF_NATION_SQUAD_IDS.get(nation)
    if not squad_id:
        return None
    slug = nation.replace(" ", "-").replace("'", "")
    return f"{FBREF_BASE_URL}/en/squads/{squad_id}/{year}-{year+1}/{slug}-Men-Stats"

def scrape_nation_season(nation: str, year: int) -> Optional[pd.DataFrame]:
    """Scrape one nation's player stats for one season."""
    url = nation_season_url(nation, year)
    if not url:
        return None
    try:
        html = fetch_cached(url)
        html2 = uncomment_fbref_tables(html)
        tables = pd.read_html(StringIO(html2), attrs={"id": "stats_standard"})
        if not tables:
            return None
        df = normalize_basic(tables[0])
        df = df.assign(Nation_Canon=nation, SeasonStartYear=year, Season=f"{year}-{year+1}")
        rename = {c: f"country_{c}" for c in ["Min", "Gls", "Ast", "90s"] if c in df.columns}
        df = df.rename(columns=rename)
        return df
    except Exception:
        return None

# -----------------------------
# Example runner (for 1 league-season)
# -----------------------------

def run_one(league_name: str, year: int) -> pd.DataFrame:
    url = league_url_season(league_name, year)
    tables = scrape_league_season(url)

    if "stats_standard" not in tables or "stats_shooting" not in tables:
        raise RuntimeError(f"Missing required tables on {url}. Found: {list(tables.keys())}")

    merged = merge_standard_shooting(tables["stats_standard"], tables["stats_shooting"])
    merged = add_metadata(merged, league_name, year)
    merged = filter_to_wc_nations(merged)

    return merged

# -----------------------------
# Bulk runner (all leagues/seasons)
# -----------------------------

def run_all(include_country_stats: bool = True) -> pd.DataFrame:
    # 1) Scrape all top-5 league seasons and keep WC-nation players
    frames = []
    for url, league_name, year in all_league_season_urls():
        try:
            tables = scrape_league_season(url)
            if "stats_standard" not in tables or "stats_shooting" not in tables:
                print(f"[WARN] Missing tables for {league_name} {year}-{year+1}: {url}")
                continue
            merged = merge_standard_shooting(tables["stats_standard"], tables["stats_shooting"])
            merged = add_metadata(merged, league_name, year)
            merged = filter_to_wc_nations(merged)
            frames.append(merged)
            print(f"[OK] {league_name} {year}-{year+1}: {len(merged)} rows (WC filtered)")
        except Exception as e:
            print(f"[ERR] {league_name} {year}-{year+1}: {e}")

    if not frames:
        return pd.DataFrame()

    # 2) One row per (player, season): primary club by minutes
    club_all = pd.concat(frames, ignore_index=True)
    club_one_per_season = one_row_per_player_season(club_all)
    print(f"[OK] Collapsed to {len(club_one_per_season)} rows (one per player per season)")

    # 3) Optionally add country stats (merge on Player, Nation_Canon, Season)
    if not include_country_stats:
        club_one_per_season = club_one_per_season.rename(columns={"Nation_Canon": "Nation"})
        return club_one_per_season

    country_frames = []
    nations_with_id = [n for n in WC_TEAMS if n in FBREF_NATION_SQUAD_IDS]
    for nation in nations_with_id:
        for year in YEARS:
            try:
                cdf = scrape_nation_season(nation, year)
                if cdf is not None and not cdf.empty:
                    country_frames.append(cdf)
            except Exception:
                pass
    if country_frames:
        country_df = pd.concat(country_frames, ignore_index=True)
        # Keep only cols we need for merge + country stats
        merge_cols = ["Player", "Nation_Canon", "SeasonStartYear"]
        country_cols = [c for c in country_df.columns if c.startswith("country_") or c in merge_cols]
        country_df = country_df[country_cols].drop_duplicates(subset=["Player", "Nation_Canon", "SeasonStartYear"], keep="first")
        out = club_one_per_season.merge(
            country_df,
            on=["Player", "Nation_Canon", "SeasonStartYear"],
            how="left",
        )
        print(f"[OK] Merged country stats for {len(nations_with_id)} nations")
    else:
        out = club_one_per_season

    out = out.rename(columns={"Nation_Canon": "Nation"})
    return out

if __name__ == "__main__":
    # Past five seasons (21-22 through 25-26); 25-26 is current and partial up to today
    df = run_all(include_country_stats=True)
    if df.empty:
        print("No data collected.")
    else:
        # Prefer column order: identity, season, club, then club stats, then country stats
        first = [c for c in ["Player", "Nation", "Season", "Squad", "League", "Pos", "Age"] if c in df.columns]
        rest = [c for c in df.columns if c not in first]
        df = df[first + rest]
        print(df.head(10))
        df.to_csv("wc_players.csv", index=False)
        print(f"Wrote wc_players.csv ({len(df)} rows)")