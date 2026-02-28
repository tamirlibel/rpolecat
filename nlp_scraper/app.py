"""NLP Web Scraper — Streamlit App

Give a URL + plain-English instruction. Claude generates Python scraping
code (BeautifulSoup for static pages, Selenium for dynamic) and the app
executes it to return structured data.

Launch:
    streamlit run nlp_scraper/app.py
"""

import os

import pandas as pd
import streamlit as st

from claude_api import call_claude
from scraper import (
    detect_page_type,
    fetch_page_source,
    run_code,
    selenium_navigate,
    start_selenium,
    stop_selenium,
)

# ── Page config ──────────────────────────────────────────────────────────────
st.set_page_config(
    page_title="NLP Web Scraper",
    page_icon="🔍",
    layout="wide",
)

# ── Session state defaults ───────────────────────────────────────────────────
for key, default in [
    ("generated_code", ""),
    ("explanation", ""),
    ("result", None),
    ("editor_result", None),
    ("last_error", ""),
]:
    if key not in st.session_state:
        st.session_state[key] = default


# ── Header ───────────────────────────────────────────────────────────────────
st.markdown(
    """
    <div style="background:#2c3e50; color:#fff; padding:18px 24px;
                border-radius:8px; margin-bottom:20px;">
      <h2 style="margin:0;">NLP Web Scraper</h2>
      <p style="margin:4px 0 0; opacity:.8;">
        Give a URL + plain-English instruction &rarr; Claude writes &amp; runs the Python code
      </p>
    </div>
    """,
    unsafe_allow_html=True,
)


# ── Sidebar ──────────────────────────────────────────────────────────────────
with st.sidebar:
    st.header("Settings")

    url = st.text_input("Target URL", placeholder="https://example.com/page")

    instruction = st.text_area(
        "What do you want to extract?",
        height=120,
        placeholder="e.g. Extract all article titles, dates, and links from the blog listing",
    )

    method = st.selectbox(
        "Scraping method",
        ["Auto-detect", "Static (BeautifulSoup)", "Dynamic (Selenium)"],
    )

    api_key = st.text_input(
        "Anthropic API key",
        type="password",
        placeholder="Leave blank → uses ANTHROPIC_API_KEY env var",
    )

    model = st.selectbox(
        "Claude model",
        ["claude-sonnet-4-20250514", "claude-haiku-4-5-20251001"],
    )

    execute = st.checkbox("Execute generated code", value=True)

    scrape_clicked = st.button("🪄 Scrape", use_container_width=True, type="primary")

    st.divider()
    st.subheader("Quick examples")

    if st.button("Wikipedia table", use_container_width=True):
        st.session_state["_ex"] = {
            "url": "https://en.wikipedia.org/wiki/List_of_countries_by_population_(United_Nations)",
            "instruction": "Extract the country names and population figures from the main table into a pandas DataFrame",
            "method": "Static (BeautifulSoup)",
        }
        st.rerun()

    if st.button("Hacker News links", use_container_width=True):
        st.session_state["_ex"] = {
            "url": "https://news.ycombinator.com",
            "instruction": "Get each story title and its URL from the front page",
            "method": "Static (BeautifulSoup)",
        }
        st.rerun()

    if st.button("Quotes to Scrape", use_container_width=True):
        st.session_state["_ex"] = {
            "url": "https://quotes.toscrape.com",
            "instruction": "Extract every quote text and its author name",
            "method": "Static (BeautifulSoup)",
        }
        st.rerun()

# Apply quick-example if set
if "_ex" in st.session_state:
    ex = st.session_state.pop("_ex")
    st.session_state["_prefill_url"] = ex["url"]
    st.session_state["_prefill_instruction"] = ex["instruction"]
    st.session_state["_prefill_method"] = ex["method"]
    st.rerun()


# ── Main scrape logic ────────────────────────────────────────────────────────
def do_scrape(url, instruction, method_label, api_key, model, execute):
    """Run the full pipeline: detect → fetch → Claude → execute."""
    resolved_key = api_key if api_key else os.environ.get("ANTHROPIC_API_KEY", "")
    if not resolved_key:
        st.error("No API key. Set `ANTHROPIC_API_KEY` or enter it in the sidebar.")
        return

    # Map UI label to code method
    method_map = {
        "Auto-detect": "auto",
        "Static (BeautifulSoup)": "static",
        "Dynamic (Selenium)": "dynamic",
    }
    method = method_map.get(method_label, "auto")

    progress = st.progress(0, text="Starting...")

    # 1. Detect page type
    if method == "auto":
        progress.progress(10, text="Detecting page type...")
        method = detect_page_type(url)
        st.info(f"Auto-detected: **{method}** page")

    # 2. Fetch page source
    progress.progress(25, text="Fetching page source...")
    page_html = None
    driver = None

    if method == "static":
        try:
            page_html = fetch_page_source(url)
        except Exception as e:
            st.warning(f"Could not prefetch page: {e}")
    else:
        try:
            progress.progress(25, text="Starting headless browser...")
            driver = start_selenium()
            page_html = selenium_navigate(driver, url)
        except Exception as e:
            st.error(f"Selenium error: {e}")
            if driver:
                stop_selenium(driver)
            progress.empty()
            return

    # 3. Call Claude
    progress.progress(50, text="Asking Claude to generate code...")
    try:
        llm = call_claude(
            url=url,
            instruction=instruction,
            page_html=page_html,
            method=method,
            api_key=resolved_key,
            model=model,
        )
    except Exception as e:
        st.error(f"Claude API error: {e}")
        if driver:
            stop_selenium(driver)
        progress.empty()
        return

    st.session_state["generated_code"] = llm["code"]
    st.session_state["explanation"] = llm["explanation"]

    # 4. Execute
    if execute:
        progress.progress(75, text="Executing generated code...")
        result = run_code(llm["code"], driver=driver, timeout_sec=60)
        st.session_state["result"] = result
    else:
        st.session_state["result"] = None

    if driver:
        stop_selenium(driver)

    progress.progress(100, text="Done!")


if scrape_clicked and url and instruction:
    do_scrape(url, instruction, method, api_key, model, execute)


# ── Output tabs ──────────────────────────────────────────────────────────────
tab_code, tab_results, tab_explain, tab_editor = st.tabs(
    ["Generated Code", "Results", "Explanation", "Code Editor"]
)

# Tab 1: Generated Code
with tab_code:
    code = st.session_state["generated_code"]
    if code:
        st.code(code, language="python")
    else:
        st.info("Click **Scrape** to generate code.")

# Tab 2: Results
with tab_results:
    result = st.session_state["result"]
    if result is None:
        st.info("No results yet.")
    elif not result["success"]:
        st.error(result["error"])
    else:
        data = result["result"]
        if isinstance(data, pd.DataFrame):
            st.dataframe(data, use_container_width=True)
            csv = data.to_csv(index=False)
            st.download_button("Download CSV", csv, "scraped_data.csv", "text/csv")
        elif isinstance(data, list) and data and isinstance(data[0], dict):
            df = pd.DataFrame(data)
            st.dataframe(df, use_container_width=True)
            csv = df.to_csv(index=False)
            st.download_button("Download CSV", csv, "scraped_data.csv", "text/csv")
        elif data is not None:
            st.write(data)
        else:
            st.warning("Code ran but produced no `result` variable.")

# Tab 3: Explanation
with tab_explain:
    explanation = st.session_state["explanation"]
    if explanation:
        st.markdown(explanation)
    else:
        st.info("Explanation will appear after scraping.")

# Tab 4: Code Editor
with tab_editor:
    edited_code = st.text_area(
        "Edit the generated code and re-run:",
        value=st.session_state["generated_code"],
        height=400,
        key="code_editor",
    )

    if st.button("▶ Run edited code", type="secondary"):
        if edited_code.strip():
            with st.spinner("Running..."):
                editor_result = run_code(edited_code)
                st.session_state["editor_result"] = editor_result

    editor_result = st.session_state["editor_result"]
    if editor_result is not None:
        if not editor_result["success"]:
            st.error(editor_result["error"])
        else:
            data = editor_result["result"]
            if isinstance(data, pd.DataFrame):
                st.dataframe(data, use_container_width=True)
            elif isinstance(data, list) and data and isinstance(data[0], dict):
                st.dataframe(pd.DataFrame(data), use_container_width=True)
            elif data is not None:
                st.write(data)
