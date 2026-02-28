"""Web scraping helpers: static (requests + BeautifulSoup) and dynamic (Selenium).

Also includes safe code execution and page-type auto-detection.
"""

import re
import signal
import traceback

import requests
from bs4 import BeautifulSoup


# ── Static fetching ──────────────────────────────────────────────────────────

def fetch_page_source(url: str, timeout: int = 15) -> str:
    """Fetch a URL and return the HTML source as a string."""
    resp = requests.get(
        url,
        headers={"User-Agent": "nlp-scraper/1.0 (Python web scraper)"},
        timeout=timeout,
    )
    resp.raise_for_status()
    return resp.text


# ── Dynamic (Selenium) ──────────────────────────────────────────────────────

def start_selenium(browser: str = "chrome", headless: bool = True):
    """Start a headless Selenium WebDriver.

    Returns a WebDriver instance.
    """
    from selenium import webdriver
    from selenium.webdriver.chrome.options import Options as ChromeOptions
    from selenium.webdriver.firefox.options import Options as FirefoxOptions

    if browser == "chrome":
        opts = ChromeOptions()
        if headless:
            opts.add_argument("--headless=new")
        opts.add_argument("--no-sandbox")
        opts.add_argument("--disable-dev-shm-usage")
        return webdriver.Chrome(options=opts)
    else:
        opts = FirefoxOptions()
        if headless:
            opts.add_argument("-headless")
        return webdriver.Firefox(options=opts)


def stop_selenium(driver) -> None:
    """Quit a Selenium WebDriver, ignoring errors."""
    try:
        driver.quit()
    except Exception:
        pass


def selenium_navigate(driver, url: str, wait_css: str | None = None, timeout: int = 10) -> str:
    """Navigate to a URL, optionally wait for an element, return page source."""
    from selenium.webdriver.common.by import By
    from selenium.webdriver.support import expected_conditions as EC
    from selenium.webdriver.support.ui import WebDriverWait

    driver.get(url)

    if wait_css:
        WebDriverWait(driver, timeout).until(
            EC.presence_of_element_located((By.CSS_SELECTOR, wait_css))
        )

    return driver.page_source


# ── Page-type detection ──────────────────────────────────────────────────────

# Only markers that reliably indicate framework-rendered SPAs
SPA_MARKERS = ['id="__next"', 'id="__nuxt"', "ng-app", "data-reactroot"]

# Secondary hints: a near-empty body + one of these generic ids
SPA_GENERIC_IDS = ['id="app"', 'id="root"']


def detect_page_type(url: str) -> str:
    """Heuristic: 'static' if the page has meaningful text, 'dynamic' if SPA-like.

    Falls back to 'static' if the page can't be fetched (let the user decide).
    """
    try:
        html = fetch_page_source(url, timeout=10)
    except Exception:
        return "static"  # default to static; user can override

    soup = BeautifulSoup(html, "lxml")
    body_text = soup.body.get_text(strip=True) if soup.body else ""
    text_is_sparse = len(body_text) < 100

    # Strong SPA signal: framework-specific markers + sparse text
    has_strong_spa = any(m in html for m in SPA_MARKERS)
    if has_strong_spa and text_is_sparse:
        return "dynamic"

    # Weak signal: generic div ids + very sparse text + many script tags
    has_generic_id = any(m in html for m in SPA_GENERIC_IDS)
    script_count = html.lower().count("<script")
    if has_generic_id and text_is_sparse and script_count >= 5:
        return "dynamic"

    return "static"


# ── Safe code execution ──────────────────────────────────────────────────────

class _Timeout(Exception):
    pass


def _timeout_handler(signum, frame):
    raise _Timeout("Code execution timed out.")


def run_code(code: str, driver=None, timeout_sec: int = 30) -> dict:
    """Execute generated Python code in a restricted namespace.

    Returns dict:
      - On success: {"success": True, "result": <value of `result` variable>}
      - On failure: {"success": False, "error": <error string>}
    """
    namespace = {"__builtins__": __builtins__}
    if driver is not None:
        namespace["driver"] = driver

    # Set an alarm-based timeout (Unix only)
    old_handler = None
    try:
        old_handler = signal.signal(signal.SIGALRM, _timeout_handler)
        signal.alarm(timeout_sec)
    except (OSError, AttributeError):
        # SIGALRM not available (Windows) — skip timeout
        pass

    try:
        exec(code, namespace)

        result = namespace.get("result", None)
        if result is None:
            # If no `result` variable, try to grab the last assigned name
            # by scanning local names that aren't dunder or imports
            candidates = [
                k for k in namespace
                if not k.startswith("_") and k not in ("driver",)
            ]
            if candidates:
                result = namespace[candidates[-1]]

        return {"success": True, "result": result}

    except _Timeout:
        return {"success": False, "error": f"Code execution timed out after {timeout_sec}s."}
    except Exception as e:
        tb = traceback.format_exc()
        return {"success": False, "error": f"{type(e).__name__}: {e}\n\n{tb}"}
    finally:
        try:
            signal.alarm(0)
            if old_handler is not None:
                signal.signal(signal.SIGALRM, old_handler)
        except (OSError, AttributeError):
            pass
