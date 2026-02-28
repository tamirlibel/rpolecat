"""CLI for the NLP Web Scraper.

Usage:
    python nlp_scraper/cli.py --url URL --instruction "what to extract"
    python nlp_scraper/cli.py --url URL --instruction "..." --method dynamic
    python nlp_scraper/cli.py --url URL --instruction "..." --no-execute
"""

import argparse
import json
import os
import sys

# Allow running as `python nlp_scraper/cli.py` from repo root
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

import pandas as pd

from claude_api import call_claude
from scraper import (
    detect_page_type,
    fetch_page_source,
    run_code,
    selenium_navigate,
    start_selenium,
    stop_selenium,
)


def main():
    parser = argparse.ArgumentParser(description="NLP Web Scraper — CLI")
    parser.add_argument("--url", required=True, help="Target URL to scrape")
    parser.add_argument("--instruction", required=True, help="What to extract (natural language)")
    parser.add_argument("--method", choices=["auto", "static", "dynamic"], default="auto")
    parser.add_argument("--model", default="claude-sonnet-4-20250514")
    parser.add_argument("--api-key", default=None, help="Anthropic API key (or set ANTHROPIC_API_KEY)")
    parser.add_argument("--no-execute", action="store_true", help="Only generate code, don't run it")
    parser.add_argument("--output", default=None, help="Save results to CSV file")
    args = parser.parse_args()

    api_key = args.api_key or os.environ.get("ANTHROPIC_API_KEY", "")
    if not api_key:
        print("Error: No API key. Set ANTHROPIC_API_KEY or pass --api-key", file=sys.stderr)
        sys.exit(1)

    method = args.method

    # 1. Detect page type
    if method == "auto":
        print("Detecting page type...", flush=True)
        method = detect_page_type(args.url)
        print(f"  -> {method}")

    # 2. Fetch page source
    print(f"Fetching page source ({method})...", flush=True)
    page_html = None
    driver = None

    if method == "static":
        try:
            page_html = fetch_page_source(args.url)
            print(f"  -> {len(page_html)} chars")
        except Exception as e:
            print(f"  -> Warning: could not prefetch: {e}")
    else:
        try:
            driver = start_selenium()
            page_html = selenium_navigate(driver, args.url)
            print(f"  -> {len(page_html)} chars (via Selenium)")
        except Exception as e:
            print(f"  -> Selenium error: {e}", file=sys.stderr)
            if driver:
                stop_selenium(driver)
            sys.exit(1)

    # 3. Call Claude
    print("Asking Claude to generate code...", flush=True)
    try:
        llm = call_claude(
            url=args.url,
            instruction=args.instruction,
            page_html=page_html,
            method=method,
            api_key=api_key,
            model=args.model,
        )
    except Exception as e:
        print(f"Claude API error: {e}", file=sys.stderr)
        if driver:
            stop_selenium(driver)
        sys.exit(1)

    # Print generated code
    print("\n" + "=" * 60)
    print("GENERATED CODE")
    print("=" * 60)
    print(llm["code"])
    print("=" * 60)

    if llm["explanation"]:
        print("\nEXPLANATION:", llm["explanation"])

    # 4. Execute
    if not args.no_execute:
        print("\nExecuting...", flush=True)
        result = run_code(llm["code"], driver=driver, timeout_sec=60)

        if driver:
            stop_selenium(driver)

        if not result["success"]:
            print(f"\nExecution error:\n{result['error']}", file=sys.stderr)
            sys.exit(1)

        data = result["result"]

        # Display results
        print("\n" + "=" * 60)
        print("RESULTS")
        print("=" * 60)

        if isinstance(data, pd.DataFrame):
            print(data.to_string(index=False))
            print(f"\n({len(data)} rows x {len(data.columns)} columns)")

            if args.output:
                data.to_csv(args.output, index=False)
                print(f"\nSaved to {args.output}")

        elif isinstance(data, list) and data and isinstance(data[0], dict):
            df = pd.DataFrame(data)
            print(df.to_string(index=False))
            print(f"\n({len(df)} rows)")

            if args.output:
                df.to_csv(args.output, index=False)
                print(f"\nSaved to {args.output}")

        elif data is not None:
            print(json.dumps(data, indent=2, default=str) if not isinstance(data, str) else data)
        else:
            print("Code ran but produced no `result` variable.")
    else:
        if driver:
            stop_selenium(driver)
        print("\n(--no-execute: code was not run)")


if __name__ == "__main__":
    main()
