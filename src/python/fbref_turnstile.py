"""
FBref + Cloudflare Turnstile helper (Turnstilesolver-compatible pattern).

The Turnstilesolver repo (Turnstilesolver/sync_solver.py) solves Turnstile by routing
the target URL to a minimal HTML page that embeds the widget, then reads
cf-turnstile-response. That same pattern is used here, but we **export cookies**
from the browser before closing — TurnstileSolver.solve() closes the context
without returning cookies, which FBref follow-up requests usually need.

Flow:
  1. GET the FBref URL with requests (browser-like headers).
  2. If the response is already real FBref HTML, return it.
  3. Else extract Turnstile sitekey from the challenge HTML.
  4. Open patchright/playwright, route-fulfill the URL with the widget (same idea as Turnstilesolver).
  5. Poll for cf-turnstile-response, then copy context cookies into a requests.Session and re-GET FBref.

Requires: playwright or patchright (Turnstilesolver uses patchright — either works).

Respect FBref's Terms of Use and robots.txt; use for coursework/research only.

Env:
  FBREF_TURNSTILE_HEADLESS=0|1   default 0 (visible often works better)
  FBREF_TURNSTILE_INVISIBLE=0|1  default 0; set 1 if widget is invisible
  FBREF_TURNSTILE_REAL_BROWSER=1  always use real URL navigation (skip synthetic widget)
  FBREF_BROWSER_WAIT_S=180       max seconds to wait for tables after real navigation
"""

from __future__ import annotations

import os
import re
import time
from typing import Optional
from urllib.parse import urlparse

import requests

# Prefer patchright (Turnstilesolver stack); fall back to playwright.
try:
    from patchright.sync_api import sync_playwright
except ImportError:
    from playwright.sync_api import sync_playwright  # type: ignore

from scraper import HEADERS, make_session

# Minimal page — same structure as Turnstilesolver.TurnstileSolver.HTML_TEMPLATE
_HTML_TEMPLATE = """<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1.0" />
    <title>Turnstile</title>
    <script
      src="https://challenges.cloudflare.com/turnstile/v0/api.js?onload=onloadTurnstileCallback"
      async defer></script>
  </head>
  <body>
    <!-- cf turnstile -->
  </body>
</html>
"""


def extract_turnstile_sitekey(html: str) -> Optional[str]:
    """Parse Cloudflare / FBref challenge HTML for Turnstile sitekey."""
    if not html:
        return None
    m = re.search(r'data-sitekey=["\']([^"\']+)["\']', html, re.I)
    if m:
        return m.group(1).strip()
    # Some pages embed the key in JS
    m = re.search(
        r'["\']sitekey["\']\s*:\s*["\']([^"\']+)["\']',
        html,
        re.I,
    )
    if m:
        return m.group(1).strip()
    m = re.search(r"\b(0x4AAAAAA[A-Za-z0-9_-]{10,})\b", html)
    if m:
        return m.group(1)
    # Managed challenge: script src like .../turnstile/v0/g/<id>/api.js (no data-sitekey in DOM)
    m = re.search(
        r"challenges\.cloudflare\.com/turnstile/v0/g/([a-zA-Z0-9_-]{8,})/api\.js",
        html,
        re.I,
    )
    if m:
        return m.group(1).strip()
    return None


def _looks_like_cloudflare_challenge(html: str) -> bool:
    h = html[:120_000].lower()
    return any(
        x in h
        for x in (
            "just a moment",
            "cf-turnstile",
            "challenges.cloudflare.com",
            "performing security verification",
            "/cdn-cgi/challenge",
        )
    )


def _looks_like_fbref_content(html: str) -> bool:
    if not html or _looks_like_cloudflare_challenge(html):
        return False
    low = html[:250_000].lower()
    return (
        "stats_standard" in low
        or "stats_table" in low
        or ("fbref" in low and "<table" in low)
    )


def _poll_turnstile_token(page, invisible: bool, max_attempts: int = 30) -> Optional[str]:
    for attempt in range(max_attempts):
        try:
            val = page.eval_on_selector("[name=cf-turnstile-response]", "el => el.value")
        except Exception:
            val = ""
        if val:
            el = page.query_selector("[name=cf-turnstile-response]")
            if el:
                return el.get_attribute("value")
        if not invisible:
            try:
                page.evaluate(
                    "const w = document.querySelector('.cf-turnstile'); "
                    "if (w) w.style.width = '70px'"
                )
                page.click(".cf-turnstile", timeout=3000)
            except Exception:
                pass
        time.sleep(0.6)
    return None


def _fetch_fbref_playwright_real_navigate(url: str, headless: bool) -> str:
    """
    Open the real FBref URL in a browser and wait for tables (no route hijack).

    Use when requests returns 403 with empty body, or challenge HTML has no extractable
    standard Turnstile sitekey (managed / explicit-render pages).
    """
    try:
        wait_s = float(os.environ.get("FBREF_BROWSER_WAIT_S", "180"))
    except ValueError:
        wait_s = 180.0

    browser_args = [
        "--disable-blink-features=AutomationControlled",
        "--disable-dev-shm-usage",
        f"--user-agent={HEADERS['User-Agent']}",
    ]

    with sync_playwright() as p:
        try:
            browser = p.chromium.launch(headless=headless, channel="chrome", args=browser_args)
        except Exception:
            browser = p.chromium.launch(headless=headless, args=browser_args)
        try:
            context = browser.new_context(
                user_agent=HEADERS["User-Agent"],
                locale="en-US",
                viewport={"width": 1280, "height": 900},
            )
            page = context.new_page()
            page.goto(url, wait_until="domcontentloaded", timeout=120_000)
            deadline = time.time() + wait_s
            poll = 2.0
            while time.time() < deadline:
                html = page.content()
                if _looks_like_fbref_content(html):
                    return html
                # Still on interstitial — give Turnstile / redirect time
                time.sleep(poll)
                try:
                    page.wait_for_load_state("domcontentloaded", timeout=5_000)
                except Exception:
                    pass

            html = page.content()
            if _looks_like_fbref_content(html):
                return html
            raise RuntimeError(
                f"Real-browser navigation did not reach FBref tables within {wait_s:.0f}s. "
                "Try FBREF_TURNSTILE_HEADLESS=0 and complete any check manually."
            )
        finally:
            browser.close()


def _apply_cookies_to_session(session: requests.Session, cookies: list, url: str) -> None:
    host = urlparse(url).hostname or "fbref.com"
    for c in cookies:
        domain = c.get("domain") or host
        if domain and domain.startswith("."):
            domain = domain.lstrip(".")
        path = c.get("path") or "/"
        name = c.get("name")
        value = c.get("value")
        if not name:
            continue
        session.cookies.set(name, value, domain=domain, path=path)


def fetch_fbref_html_with_turnstile(
    url: str,
    *,
    headless: Optional[bool] = None,
    invisible: Optional[bool] = None,
) -> str:
    """
    Fetch FBref HTML, solving Turnstile when needed (Turnstilesolver-style widget + cookie handoff).

    You can also call Turnstilesolver's get_turnstile_token() manually for debugging,
    but this function is what you want for actual FBref page fetches.
    """
    if headless is None:
        headless = os.environ.get("FBREF_TURNSTILE_HEADLESS", "").strip().lower() in (
            "1",
            "true",
            "yes",
        )
    if invisible is None:
        invisible = os.environ.get("FBREF_TURNSTILE_INVISIBLE", "").strip().lower() in (
            "1",
            "true",
            "yes",
        )

    session = make_session()
    session.headers.update(HEADERS)
    r = session.get(url, timeout=60)
    text = r.text or ""

    if _looks_like_fbref_content(text):
        return text

    if not _looks_like_cloudflare_challenge(text) and r.status_code == 200:
        return text

    # 403 often has empty body. Managed CF pages use /turnstile/v0/g/<shortid>/api.js — that id is
    # NOT the same as a normal data-sitekey; synthetic widget solve will fail → use real navigation.
    sitekey = extract_turnstile_sitekey(text)
    short_cf_id = bool(sitekey) and len(sitekey) < 24 and not sitekey.startswith("0x")
    force_real = os.environ.get("FBREF_TURNSTILE_REAL_BROWSER", "").strip().lower() in (
        "1",
        "true",
        "yes",
    )
    if (
        not sitekey
        or short_cf_id
        or r.status_code == 403
        or len(text) < 200
        or force_real
    ):
        return _fetch_fbref_playwright_real_navigate(url, headless=headless)

    url_norm = url if url.endswith("/") else url + "/"
    turnstile_div = (
        f'<div class="cf-turnstile" data-sitekey="{sitekey}" data-theme="light"></div>'
    )
    page_html = _HTML_TEMPLATE.replace("<!-- cf turnstile -->", turnstile_div)

    browser_args = [
        "--disable-blink-features=AutomationControlled",
        "--disable-dev-shm-usage",
        f"--user-agent={HEADERS['User-Agent']}",
    ]

    with sync_playwright() as p:
        try:
            browser = p.chromium.launch(headless=headless, channel="chrome", args=browser_args)
        except Exception:
            browser = p.chromium.launch(headless=headless, args=browser_args)
        try:
            context = browser.new_context(
                user_agent=HEADERS["User-Agent"],
                locale="en-US",
                viewport={"width": 1280, "height": 900},
            )
            page = context.new_page()

            def _fulfill(route):
                route.fulfill(body=page_html, status=200, content_type="text/html")

            page.route(url_norm, _fulfill)
            page.goto(url_norm, wait_until="domcontentloaded", timeout=120_000)

            token = _poll_turnstile_token(page, invisible=invisible)
            if not token:
                context.close()
                browser.close()
                raise RuntimeError(
                    "Turnstile token not produced in time. "
                    "Try headless=False, or FBREF_TURNSTILE_INVISIBLE=1, or complete any visible check."
                )

            cookies = context.cookies()
            context.close()
        finally:
            browser.close()

    _apply_cookies_to_session(session, cookies, url)
    r2 = session.get(url, timeout=90, allow_redirects=True)
    out = r2.text or ""
    if not _looks_like_fbref_content(out):
        raise RuntimeError(
            "After Turnstile, FBref HTML still looks like a block page. "
            "Cookies may be insufficient; try visible browser (headless=False) or residential IP."
        )
    return out


def try_turnstilesolver_token_only(url: str, sitekey: str) -> dict:
    """
    Optional: call the vendored Turnstilesolver sync API (returns token dict only, no cookies).

    Useful for debugging. Requires Turnstilesolver deps (patchright, logmagix; camoufox only for camoufox mode).
    """
    import sys
    from pathlib import Path

    root = Path(__file__).resolve().parent / "Turnstilesolver"
    if not root.is_dir():
        return {"status": "error", "error": f"Turnstilesolver not found at {root}"}
    sys.path.insert(0, str(root))
    try:
        from sync_solver import get_turnstile_token  # type: ignore
    except ImportError as e:
        return {
            "status": "error",
            "error": f"Import sync_solver failed: {e}. pip install -r Turnstilesolver/requirements.txt",
        }

    return get_turnstile_token(
        url=url,
        sitekey=sitekey,
        headless=False,
        browser_type="chrome",
        user_agent=HEADERS["User-Agent"],
        invisible=os.environ.get("FBREF_TURNSTILE_INVISIBLE", "").strip().lower()
        in ("1", "true", "yes"),
    )
