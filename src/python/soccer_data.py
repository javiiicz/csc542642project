import io
import json
import os
import random
import re
import time
import warnings
from datetime import timedelta
from pathlib import Path
from typing import IO, Iterable, Optional, Union

import numpy as np
import pandas as pd
import soccerdata as sd

try:
    from playwright.sync_api import sync_playwright

    _PLAYWRIGHT_AVAILABLE = True
except ImportError:
    sync_playwright = None  # type: ignore[misc, assignment]
    _PLAYWRIGHT_AVAILABLE = False

# ============================================================
# CONFIG
# ============================================================

DATA_DIR = Path("wc2026_outputs")
DATA_DIR.mkdir(parents=True, exist_ok=True)

CACHE_DIR = Path.home() / "soccerdata"

SEASONS = ["2122", "2223", "2324", "2425", "2526"]
LEAGUES = "Big 5 European Leagues Combined"

MIN_MINUTES_DEFAULT = 900

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
}

# Optional: normalize some alternate strings you may encounter from FBref
COUNTRY_ALIASES_EXTRA = {
    "Korea, South": "South Korea",
    "Congo DR": "DR Congo",
    "United States of America": "United States",
    "Cape Verde Islands": "Cape Verde",
    "IR Iran": "Iran",
}

POSITION_MAP = {
    "GK": "GK",
    "DF": "DEF",
    "MF": "MID",
    "FW": "FWD",
    "DF,MF": "DEF",
    "MF,DF": "MID",
    "MF,FW": "ATT",
    "FW,MF": "ATT",
    "DF,FW": "DEF",
    "FW,DF": "ATT",
}

# Nations you want LCQ shortlists for
# Example only — edit to whatever bracket you want.
DEFAULT_LCQ_NATIONS = ["Italy", "Sweden"]

# Cloudflare / FBref: soccerdata uses tls_requests, which is often blocked with HTTP 403 before
# any HTML is returned. We auto-fallback to Playwright on 403/429/503 when Playwright is installed
# unless FBREF_NO_PLAYWRIGHT=1.
# - FBREF_NO_PLAYWRIGHT=1     disable browser fallback (tls only)
# - FBREF_PLAYWRIGHT_FIRST=1  skip tls entirely for HTML fetches (useful if every request is 403)
# - FBREF_PLAYWRIGHT_HEADFUL=1  visible browser (often required for Turnstile / manual checkbox)
# - FBREF_PLAYWRIGHT_CHROME=1  use Google Chrome channel instead of bundled Chromium (better for CF)
# - FBREF_CHALLENGE_TIMEOUT_S=600  headful: keep polling until FBref HTML appears (default 600)
# - FBREF_MANUAL_CONTINUE=1  headful: after opening page, wait for Enter before continuing (you verify first)
# - FBREF_PLAYWRIGHT_WARM=1  optional first hit to https://fbref.com/ (off by default; can break Turnstile)
# - FBREF_PURGE_BAD_CACHE=1   at startup of main(), delete poisoned *.html caches


def is_cloudflare_challenge_page(content: bytes) -> bool:
    """True if HTML looks like a Cloudflare interstitial (not real FBref tables)."""
    if not content:
        return False
    head = content[:80_000].lower()
    markers = (
        b"just a moment",
        b"cf-chl",
        b"challenge-platform",
        b"cf-turnstile",
        b"performing security verification",
        b"/cdn-cgi/challenge",
        b"challenges.cloudflare.com",
    )
    return any(m in head for m in markers)


def fbref_html_looks_valid(blob: bytes) -> bool:
    """Heuristic: response is real FBref HTML, not CF interstitial or tiny error body."""
    if not blob or is_cloudflare_challenge_page(blob):
        return False
    low = blob[:200_000].lower()
    if b"stats_standard" in blob or b"stats_table" in blob or b"div_stats_" in blob:
        return True
    if b"fbref" in low and (b"<table" in low or b"<!--" in low or b"<h1" in low):
        return True
    if b"sports reference" in low or b"data-stat=" in low:
        return True
    if b"schedule" in low and b"fbref" in low:
        return True
    # Large HTML that is not a CF page is usually a real document
    return len(blob) > 12_000 and b"<!doctype html>" in low


def _playwright_fallback_enabled() -> bool:
    if os.environ.get("FBREF_NO_PLAYWRIGHT", "").strip().lower() in ("1", "true", "yes"):
        return False
    return _PLAYWRIGHT_AVAILABLE


# Reduce obvious automation signals (Turnstile still may block headless / flagged IPs).
_PLAYWRIGHT_STEALTH_INIT = """
Object.defineProperty(navigator, 'webdriver', { get: () => undefined });
if (!window.chrome) { window.chrome = { runtime: {} }; }
"""


def _playwright_launch_extras() -> tuple[list[str], list[str]]:
    """Returns (ignore_default_args, chromium_args)."""
    ignore = ["--enable-automation"]
    args = [
        "--disable-blink-features=AutomationControlled",
        "--disable-dev-shm-usage",
    ]
    return ignore, args


def _fetch_url_playwright(url: str) -> bytes:
    """Fetch page HTML with Playwright (Chromium or real Chrome). Use when tls_requests gets 403."""
    if not _PLAYWRIGHT_AVAILABLE or sync_playwright is None:
        raise RuntimeError(
            "Playwright not installed. Run: pip install playwright && playwright install chromium"
        )
    headful = os.environ.get("FBREF_PLAYWRIGHT_HEADFUL", "").strip().lower() in ("1", "true", "yes")
    use_chrome = os.environ.get("FBREF_PLAYWRIGHT_CHROME", "").strip().lower() in ("1", "true", "yes")
    user_data = os.environ.get("FBREF_PLAYWRIGHT_USER_DATA_DIR", "").strip()
    do_warm = os.environ.get("FBREF_PLAYWRIGHT_WARM", "").strip().lower() in ("1", "true", "yes")
    manual_continue = os.environ.get("FBREF_MANUAL_CONTINUE", "").strip().lower() in ("1", "true", "yes")
    try:
        challenge_timeout_s = float(os.environ.get("FBREF_CHALLENGE_TIMEOUT_S", "600"))
    except ValueError:
        challenge_timeout_s = 600.0

    ua = (
        "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) "
        "AppleWebKit/537.36 (KHTML, like Gecko) Chrome/131.0.0.0 Safari/537.36"
    )
    ignore_default_args, chromium_args = _playwright_launch_extras()
    pw = sync_playwright().start()
    browser = None
    context = None
    try:
        launch_kw: dict = {
            "headless": not headful,
            "ignore_default_args": ignore_default_args,
            "args": chromium_args,
        }
        if use_chrome:
            launch_kw["channel"] = "chrome"

        common_ctx_kw = {
            "user_agent": ua,
            "locale": "en-US",
            "viewport": {"width": 1365, "height": 900},
            "extra_http_headers": {
                "Accept-Language": "en-US,en;q=0.9",
                "Accept": "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
            },
        }

        if user_data:
            # Persistent profile: complete a CF challenge once in this profile, then reuse cookies.
            context = pw.chromium.launch_persistent_context(
                user_data,
                **launch_kw,
                **common_ctx_kw,
            )
        else:
            try:
                browser = pw.chromium.launch(**launch_kw)
            except Exception:
                if use_chrome:
                    raise
                retry_kw = dict(launch_kw)
                retry_kw.pop("channel", None)
                browser = pw.chromium.launch(**retry_kw)
            context = browser.new_context(**common_ctx_kw)

        # Apply before any navigation so every load gets the patch
        context.add_init_script(_PLAYWRIGHT_STEALTH_INIT)
        page = context.pages[0] if context.pages else context.new_page()

        # Extra homepage visit often triggers a second Turnstile; off unless FBREF_PLAYWRIGHT_WARM=1
        if do_warm:
            try:
                page.goto("https://fbref.com/", wait_until="domcontentloaded", timeout=45_000)
                page.wait_for_timeout(1500)
            except Exception:
                pass

        page.goto(url, wait_until="domcontentloaded", timeout=120_000)

        if headful and manual_continue:
            print(
                "\n[FBref] Browser is open. Complete Cloudflare / Turnstile in the window.\n"
                "When you see the real FBref page (stats or navigation), return here and press Enter.\n"
            )
            try:
                input("Press Enter to capture HTML and continue… ")
            except EOFError:
                page.wait_for_timeout(5000)

        # Poll: Turnstile finishes asynchronously; fixed sleeps often capture too early.
        deadline = time.time() + (challenge_timeout_s if headful else 45.0)
        poll_interval_s = 2.0
        last_log = 0.0
        while time.time() < deadline:
            html_bytes = page.content().encode("utf-8")
            if fbref_html_looks_valid(html_bytes):
                return html_bytes
            now = time.time()
            if headful and now - last_log > 20:
                print(
                    "[FBref] Still waiting for page after Cloudflare (poll up to "
                    f"{challenge_timeout_s:.0f}s). Complete the check in the browser…"
                )
                last_log = now
            page.wait_for_timeout(int(poll_interval_s * 1000))
            try:
                page.wait_for_load_state("domcontentloaded", timeout=5_000)
            except Exception:
                pass

        return page.content().encode("utf-8")
    finally:
        if context is not None:
            try:
                context.close()
            except Exception:
                pass
        if browser is not None:
            try:
                browser.close()
            except Exception:
                pass
        pw.stop()


def purge_cloudflare_html_caches(
    roots: Optional[list[Path]] = None,
    *,
    dry_run: bool = False,
) -> tuple[int, list[Path]]:
    """
    Delete cached *.html files that are Cloudflare challenge pages.
    Scans: ./cache_fbref (scraper.py), soccerdata CACHE_DIR, default package FBref path.
    """
    if roots is None:
        roots = [
            Path("cache_fbref").resolve(),
            CACHE_DIR,
            Path.home() / "soccerdata" / "data" / "FBref",
        ]
    removed: list[Path] = []
    for root in roots:
        if not root.exists():
            continue
        for p in root.rglob("*.html"):
            try:
                sample = p.read_bytes()[:100_000]
            except OSError:
                continue
            if is_cloudflare_challenge_page(sample):
                removed.append(p)
                if not dry_run:
                    p.unlink(missing_ok=True)
    return len(removed), removed


class FBrefCloudflareSafe(sd.FBref):
    """
    FBref reader that (1) drops poisoned cache files, (2) refuses to store CF interstitials,
    (3) on HTTP 403/429/503 or CF HTML from tls_requests, falls back to Playwright when installed
        (disable with FBREF_NO_PLAYWRIGHT=1), or use FBREF_PLAYWRIGHT_FIRST=1 to skip tls for HTML.
    """

    def _init_session(self, headers=None):
        import tls_requests
        from soccerdata.fbref import FBREF_HEADERS

        merged = dict(FBREF_HEADERS)
        if headers:
            merged.update(headers)
        return tls_requests.Client(proxy=self.proxy(), headers=merged)

    def get(
        self,
        url: str,
        filepath: Optional[Path] = None,
        max_age: Optional[Union[int, timedelta]] = None,
        no_cache: bool = False,
        var: Optional[Union[str, Iterable[str]]] = None,
    ) -> IO[bytes]:
        from soccerdata._config import MAXAGE, logger

        if max_age is None:
            max_age = MAXAGE

        is_cached = self._is_cached(filepath, max_age)
        if (
            not no_cache
            and not self.no_cache
            and is_cached
            and filepath is not None
            and filepath.exists()
        ):
            try:
                blob = filepath.read_bytes()
            except OSError:
                blob = b""
            if is_cloudflare_challenge_page(blob):
                logger.warning("Removing poisoned cache (Cloudflare) for %s", filepath)
                filepath.unlink(missing_ok=True)
                is_cached = False

        if no_cache or self.no_cache or not is_cached:
            logger.debug("Scraping %s", url)
            return self._download_and_save(url, filepath, var)
        logger.debug("Retrieving %s from cache", url)
        if filepath is None:
            raise ValueError("No filepath provided for cached data.")
        return filepath.open(mode="rb")

    def _download_and_save(
        self,
        url: str,
        filepath: Optional[Path] = None,
        var: Optional[Union[str, Iterable[str]]] = None,
    ) -> IO[bytes]:
        from soccerdata._config import logger

        pw_ok = _playwright_fallback_enabled()
        playwright_first = os.environ.get("FBREF_PLAYWRIGHT_FIRST", "").strip().lower() in (
            "1",
            "true",
            "yes",
        )

        for i in range(5):
            try:
                payload: Optional[bytes] = None

                # --- HTML pages: optional browser-first (tls often returns 403 on FBref) ---
                if var is None and playwright_first and pw_ok:
                    try:
                        payload = _fetch_url_playwright(url)
                    except Exception as exc:
                        logger.warning("Playwright-first failed for %s: %s", url, exc)
                        payload = None
                    if not fbref_html_looks_valid(payload or b""):
                        payload = None

                if payload is None:
                    response = self._session.get(url)
                    time.sleep(self.rate_limit + random.random() * self.max_delay)
                    status = int(getattr(response, "status_code", 200) or 200)
                    raw = response.content

                    if var is not None:
                        response.raise_for_status()
                        if isinstance(var, str):
                            var_list = [var]
                        else:
                            var_list = list(var)
                        var_names = "|".join(var_list)
                        template_understat = rb"(%b)+[\s\t]*=[\s\t]*JSON\.parse\('(.*)'\)"
                        pattern_understat = template_understat % bytes(var_names, encoding="utf-8")
                        results = re.findall(pattern_understat, response.content)
                        data = {
                            key.decode("unicode_escape"): json.loads(value.decode("unicode_escape"))
                            for key, value in results
                        }
                        payload = json.dumps(data).encode("utf-8")
                    else:
                        blocked = status in (403, 429, 503) or status >= 500
                        bad_body = bool(raw) and is_cloudflare_challenge_page(raw)
                        if blocked or bad_body:
                            logger.warning(
                                "tls_requests got HTTP %s for %s (attempt %d/5); %s",
                                status,
                                url,
                                i + 1,
                                "trying Playwright"
                                if pw_ok
                                else "set up Playwright or use FBREF_PLAYWRIGHT_FIRST=1",
                            )
                            if pw_ok:
                                try:
                                    payload = _fetch_url_playwright(url)
                                except Exception as exc:
                                    logger.warning("Playwright fallback failed: %s", exc)
                                    payload = raw if raw else None
                            else:
                                payload = raw if raw else None

                            if not fbref_html_looks_valid(payload or b""):
                                self._session = self._init_session()
                                continue
                        elif status >= 400:
                            response.raise_for_status()
                        else:
                            payload = raw

                        if not fbref_html_looks_valid(payload or b""):
                            if pw_ok and payload is not None:
                                try:
                                    payload = _fetch_url_playwright(url)
                                except Exception as exc:
                                    logger.warning("Playwright retry failed: %s", exc)
                            if not fbref_html_looks_valid(payload or b""):
                                logger.warning(
                                    "Still no usable FBref HTML for %s (attempt %d/5)",
                                    url,
                                    i + 1,
                                )
                                self._session = self._init_session()
                                continue

                assert payload is not None

                if not self.no_store and filepath is not None:
                    filepath.parent.mkdir(parents=True, exist_ok=True)
                    with filepath.open(mode="wb") as fh:
                        fh.write(payload)
                return io.BytesIO(payload)
            except Exception:
                logger.exception(
                    "Error while scraping %s. Retrying... (attempt %d of 5).",
                    url,
                    i + 1,
                )
                self._session = self._init_session()
                continue

        raise ConnectionError(f"Could not download {url}.")


# ============================================================
# HELPERS
# ============================================================

def canon_country(name: object) -> Optional[str]:
    if pd.isna(name):
        return None
    s = str(name).strip()
    s = COUNTRY_ALIASES.get(s, s)
    s = COUNTRY_ALIASES_EXTRA.get(s, s)
    return s


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
    for c in candidates:
        if c in df.columns:
            return c
    lower_map = {col.lower(): col for col in df.columns}
    for c in candidates:
        if c.lower() in lower_map:
            return lower_map[c.lower()]
    return None


def first_existing(df: pd.DataFrame, candidates: list[str], default=np.nan):
    col = find_col(df, candidates)
    if col is None:
        return pd.Series([default] * len(df), index=df.index)
    return df[col]


def normalize_position(pos: object) -> Optional[str]:
    if pd.isna(pos):
        return None
    s = str(pos).strip()
    return POSITION_MAP.get(s, s)


def derive_nation_from_fbref_field(val: object) -> Optional[str]:
    """
    FBref / soccerdata sometimes returns nation labels in slightly different forms.
    Keep this permissive and avoid over-parsing.
    """
    if pd.isna(val):
        return None
    s = str(val).strip()
    s = re.sub(r"\s+", " ", s)

    # If nation values come in like "eng ENG", keep last token only if full string looks codey.
    parts = s.split()
    if len(parts) == 2 and len(parts[-1]) == 3 and parts[-1].isalpha():
        # not enough info to map cleanly without a dedicated code map
        # keep original string; canonicalization may still work if soccerdata already uses names
        return canon_country(s)

    return canon_country(s)


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
# SOCCERDATA EXTRACTION
# ============================================================

def init_fbref() -> FBrefCloudflareSafe:
    warnings.filterwarnings("ignore", category=UserWarning)
    fb = FBrefCloudflareSafe(
        leagues=LEAGUES,
        seasons=SEASONS,
        data_dir=CACHE_DIR,
        no_cache=False,
        no_store=False,
        # Tor can help if your IP is blocked; start Tor on 9050 and use proxy="tor".
        proxy=None,
    )
    return fb


def load_fbref_player_tables(fb: sd.FBref) -> dict[str, pd.DataFrame]:
    stat_types = [
        "standard",
        "shooting",
        "passing",
        "defense",
        "possession",
        "misc",
        "keeper",
        "keeper_adv",
    ]

    tables: dict[str, pd.DataFrame] = {}
    for stat_type in stat_types:
        print(f"Loading FBref player season stats: {stat_type}")
        df = fb.read_player_season_stats(stat_type=stat_type)
        df = flatten_columns(df)
        df = clean_strings(df)
        tables[stat_type] = df

    return tables


# ============================================================
# TABLE STANDARDIZATION
# ============================================================

def standardize_indexed_table(df: pd.DataFrame, source_name: str) -> pd.DataFrame:
    """
    soccerdata tables often come indexed by league/season/team/player or similar.
    Reset the index so merges are easier.
    """
    df = df.copy().reset_index()
    df["source_table"] = source_name
    return df


def prep_standard_table(df: pd.DataFrame) -> pd.DataFrame:
    df = standardize_indexed_table(df, "standard")

    # Column discovery
    player_col = find_col(df, ["player", "Player"])
    squad_col = find_col(df, ["team", "squad", "Squad"])
    season_col = find_col(df, ["season", "Season"])
    league_col = find_col(df, ["league", "League"])
    nation_col = find_col(df, ["nation", "Nation"])
    pos_col = find_col(df, ["pos", "Pos"])
    age_col = find_col(df, ["age", "Age"])
    min_col = find_col(df, ["min", "minutes", "Min", "Playing Time_Min"])

    rename_map = {}
    if player_col: rename_map[player_col] = "player"
    if squad_col: rename_map[squad_col] = "team"
    if season_col: rename_map[season_col] = "season"
    if league_col: rename_map[league_col] = "league"
    if nation_col: rename_map[nation_col] = "nation_raw"
    if pos_col: rename_map[pos_col] = "pos_raw"
    if age_col: rename_map[age_col] = "age"
    if min_col: rename_map[min_col] = "minutes"

    df = df.rename(columns=rename_map)

    df["nation"] = df["nation_raw"].map(derive_nation_from_fbref_field) if "nation_raw" in df.columns else np.nan
    df["position_group"] = df["pos_raw"].map(normalize_position) if "pos_raw" in df.columns else np.nan

    numeric_candidates = ["minutes", "age", "90s", "starts", "matches"]
    df = safe_numeric(df, [c for c in numeric_candidates if c in df.columns])

    return df


def prep_stat_table(df: pd.DataFrame, source_name: str) -> pd.DataFrame:
    df = standardize_indexed_table(df, source_name)

    # Common merge keys
    rename_map = {}
    for src, dst in [
        ("player", "player"),
        ("Player", "player"),
        ("team", "team"),
        ("squad", "team"),
        ("Squad", "team"),
        ("season", "season"),
        ("Season", "season"),
        ("league", "league"),
        ("League", "league"),
    ]:
        if src in df.columns:
            rename_map[src] = dst

    df = df.rename(columns=rename_map)
    return df


def merge_player_tables(tables: dict[str, pd.DataFrame]) -> pd.DataFrame:
    std = prep_standard_table(tables["standard"])

    merge_keys = [k for k in ["league", "season", "team", "player"] if k in std.columns]

    keep_from_each = {
        "shooting": [
            "player", "team", "season", "league",
            "Gls", "Sh", "SoT", "SoT%", "Sh/90", "SoT/90",
            "xG", "npxG", "npxG/Sh", "G-xG", "np:G-xG",
        ],
        "passing": [
            "player", "team", "season", "league",
            "Ast", "xAG", "xA", "A-xAG", "KP", "1/3", "PPA", "CrsPA", "PrgP",
        ],
        "defense": [
            "player", "team", "season", "league",
            "Tkl", "TklW", "Blocks", "Int", "Clr",
        ],
        "possession": [
            "player", "team", "season", "league",
            "Touches", "Succ", "Att", "PrgC", "PrgR",
        ],
        "misc": [
            "player", "team", "season", "league",
            "Fls", "Fld", "Off", "Crs", "Recov", "Won", "Lost",
        ],
        "keeper": [
            "player", "team", "season", "league",
            "GA", "GA90", "SoTA", "Saves", "Save%",
        ],
        "keeper_adv": [
            "player", "team", "season", "league",
            "PSxG", "PSxG/SoT", "PSxG+/-", "Cmp", "Att", "Cmp%",
        ],
    }

    merged = std.copy()

    for name, cols in keep_from_each.items():
        if name not in tables:
            continue

        df = prep_stat_table(tables[name], name)

        available_cols = [c for c in cols if c in df.columns]
        for mk in merge_keys:
            if mk not in available_cols and mk in df.columns:
                available_cols.append(mk)

        if not all(k in df.columns for k in merge_keys):
            print(f"Skipping merge for {name}: missing merge keys")
            continue

        df = df[available_cols].copy()

        # Prefix non-key cols to prevent collisions
        rename_nonkeys = {c: f"{name.lower()}__{c}" for c in df.columns if c not in merge_keys}
        df = df.rename(columns=rename_nonkeys)

        merged = merged.merge(df, on=merge_keys, how="left")

    # Standardize important final columns
    merged["minutes"] = pd.to_numeric(first_existing(merged, ["minutes", "standard__Min"]), errors="coerce")
    merged["xg"] = pd.to_numeric(first_existing(merged, ["shooting__xG", "xG"]), errors="coerce")
    merged["npxg"] = pd.to_numeric(first_existing(merged, ["shooting__npxG", "npxG"]), errors="coerce")
    merged["xag"] = pd.to_numeric(first_existing(merged, ["passing__xAG", "xAG"]), errors="coerce")
    merged["goals"] = pd.to_numeric(first_existing(merged, ["shooting__Gls", "Gls"]), errors="coerce")
    merged["shots"] = pd.to_numeric(first_existing(merged, ["shooting__Sh", "Sh"]), errors="coerce")
    merged["sot"] = pd.to_numeric(first_existing(merged, ["shooting__SoT", "SoT"]), errors="coerce")
    merged["kp"] = pd.to_numeric(first_existing(merged, ["passing__KP", "KP"]), errors="coerce")
    merged["prog_passes"] = pd.to_numeric(first_existing(merged, ["passing__PrgP", "PrgP"]), errors="coerce")
    merged["prog_carries"] = pd.to_numeric(first_existing(merged, ["possession__PrgC", "PrgC"]), errors="coerce")
    merged["prog_rec"] = pd.to_numeric(first_existing(merged, ["possession__PrgR", "PrgR"]), errors="coerce")
    merged["tackles_won"] = pd.to_numeric(first_existing(merged, ["defense__TklW", "TklW"]), errors="coerce")
    merged["interceptions"] = pd.to_numeric(first_existing(merged, ["defense__Int", "Int"]), errors="coerce")
    merged["clearances"] = pd.to_numeric(first_existing(merged, ["defense__Clr", "Clr"]), errors="coerce")
    merged["recoveries"] = pd.to_numeric(first_existing(merged, ["misc__Recov", "Recov"]), errors="coerce")
    merged["psxg_plus_minus"] = pd.to_numeric(first_existing(merged, ["keeper_adv__PSxG+/-", "PSxG+/-"]), errors="coerce")

    merged["xg_plus_xag"] = merged[["xg", "xag"]].sum(axis=1, min_count=1)
    merged["npxg_plus_xag"] = merged[["npxg", "xag"]].sum(axis=1, min_count=1)

    merged = add_per90(
        merged,
        numerators=[
            "xg", "npxg", "xag", "goals", "shots", "sot", "kp",
            "prog_passes", "prog_carries", "prog_rec",
            "tackles_won", "interceptions", "clearances", "recoveries",
        ],
        minutes_col="minutes",
    )

    return merged


# ============================================================
# FILTERING + RANKING
# ============================================================

def filter_wc_players(df: pd.DataFrame, wc_teams: Optional[set[str]] = None, min_minutes: int = MIN_MINUTES_DEFAULT) -> pd.DataFrame:
    if wc_teams is None:
        wc_teams = build_wc_team_set()

    out = df.copy()

    if "nation" in out.columns:
        out = out[out["nation"].isin(wc_teams)]

    if "minutes" in out.columns:
        out = out[out["minutes"].fillna(0) >= min_minutes]

    return out.reset_index(drop=True)


def compute_position_score(df: pd.DataFrame) -> pd.DataFrame:
    out = df.copy()

    out["position_group"] = out["position_group"].fillna("UNK")

    out["rank_score"] = np.nan

    # Attackers
    mask_att = out["position_group"].isin(["FWD", "ATT"])
    out.loc[mask_att, "rank_score"] = (
        0.70 * out.loc[mask_att, "npxg_per90"].fillna(0)
        + 0.30 * out.loc[mask_att, "xag_per90"].fillna(0)
    )

    # Midfielders
    mask_mid = out["position_group"].isin(["MID"])
    out.loc[mask_mid, "rank_score"] = (
        0.45 * out.loc[mask_mid, "xag_per90"].fillna(0)
        + 0.20 * out.loc[mask_mid, "prog_passes_per90"].fillna(0)
        + 0.20 * out.loc[mask_mid, "prog_carries_per90"].fillna(0)
        + 0.15 * out.loc[mask_mid, "kp_per90"].fillna(0)
    )

    # Defenders
    mask_def = out["position_group"].isin(["DEF"])
    out.loc[mask_def, "rank_score"] = (
        0.25 * out.loc[mask_def, "prog_passes_per90"].fillna(0)
        + 0.20 * out.loc[mask_def, "interceptions_per90"].fillna(0)
        + 0.20 * out.loc[mask_def, "clearances_per90"].fillna(0)
        + 0.20 * out.loc[mask_def, "recoveries_per90"].fillna(0)
        + 0.15 * out.loc[mask_def, "tackles_won_per90"].fillna(0)
    )

    # Goalkeepers
    mask_gk = out["position_group"].isin(["GK"])
    out.loc[mask_gk, "rank_score"] = out.loc[mask_gk, "psxg_plus_minus"].fillna(0)

    # Small reliability bump for bigger samples
    out["reliability_weight"] = np.log1p(out["minutes"].fillna(0))
    out["rank_score_weighted"] = out["rank_score"] * out["reliability_weight"]

    return out


def top_n_per_position(
    df: pd.DataFrame,
    nations: list[str],
    n: int = 2,
) -> pd.DataFrame:
    nations = [canon_country(x) for x in nations]
    out = df.copy()
    out = out[out["nation"].isin(nations)]
    out = compute_position_score(out)

    out = out.sort_values(
        by=["nation", "position_group", "rank_score_weighted", "minutes"],
        ascending=[True, True, False, False],
    )

    out["rank_within_group"] = out.groupby(["nation", "position_group"]).cumcount() + 1
    out = out[out["rank_within_group"] <= n].reset_index(drop=True)
    return out


# ============================================================
# MAIN
# ============================================================

def main():
    if os.environ.get("FBREF_PURGE_BAD_CACHE", "").strip().lower() in ("1", "true", "yes"):
        n, paths = purge_cloudflare_html_caches()
        print(f"FBREF_PURGE_BAD_CACHE: removed {n} Cloudflare-poisoned HTML cache file(s).")
        if n and n <= 15:
            for p in paths:
                print("  ", p)

    print("Initializing soccerdata FBref reader (Cloudflare-safe wrapper)...")
    print(
        "Tips: Use FBREF_PLAYWRIGHT_CHROME=1 + HEADFUL=1; FBREF_MANUAL_CONTINUE=1 to press Enter "
        "after you pass Turnstile; persistent profile: FBREF_PLAYWRIGHT_USER_DATA_DIR. "
        "Or proxy='tor' in init_fbref(). FBref may still block datacenter/VPN IPs."
    )
    fb = init_fbref()

    print("Loading player season tables...")
    tables = load_fbref_player_tables(fb)

    print("Merging tables...")
    player_df = merge_player_tables(tables)

    print(f"Raw merged rows: {len(player_df):,}")

    print("Filtering to WC-relevant nations and minimum minutes...")
    wc_df = filter_wc_players(player_df, min_minutes=MIN_MINUTES_DEFAULT)

    print(f"WC-filtered rows: {len(wc_df):,}")

    # Save full outputs
    raw_path = DATA_DIR / "fbref_big5_player_season_2122_2526_raw_merged.csv"
    wc_path = DATA_DIR / "fbref_big5_player_season_2122_2526_wc_filtered.csv"

    player_df.to_csv(raw_path, index=False)
    wc_df.to_csv(wc_path, index=False)

    print(f"Saved raw merged dataset to: {raw_path}")
    print(f"Saved WC-filtered dataset to: {wc_path}")

    # Example LCQ shortlist
    lcq_df = top_n_per_position(wc_df, nations=DEFAULT_LCQ_NATIONS, n=2)
    lcq_path = DATA_DIR / "lcq_top2_per_position_example.csv"
    lcq_df.to_csv(lcq_path, index=False)

    print(f"Saved example LCQ shortlist to: {lcq_path}")

    # Quick preview
    preview_cols = [
        c for c in [
            "league", "season", "team", "player", "nation", "pos_raw", "position_group",
            "minutes", "xg", "npxg", "xag", "xg_plus_xag",
            "npxg_per90", "xag_per90", "rank_score_weighted"
        ] if c in lcq_df.columns
    ]

    if not lcq_df.empty and preview_cols:
        print("\nExample LCQ shortlist preview:")
        print(lcq_df[preview_cols].head(20).to_string(index=False))
    else:
        print("\nNo LCQ rows found with the current nation matching. You may need to inspect the nation field.")


if __name__ == "__main__":
    main()