"""Claude API integration for generating web scraping code.

Sends a URL + natural language instruction to Claude, which returns
Python scraping code using BeautifulSoup (static) or Selenium (dynamic).
"""

import anthropic
import re


SYSTEM_PROMPTS = {
    "static": """You are an expert Python programmer specializing in web scraping.
Generate clean, working Python code based on the user's natural language instruction.

RULES:
1. Return ONLY valid Python code inside a single ```python code block, followed by a brief explanation.
2. The code must be self-contained and immediately runnable.
3. Use try/except for error handling.
4. Store the final result in a variable called `result` — as a pandas DataFrame (preferred) or a list of dicts.
5. Add short comments on key steps.
6. Do NOT call pip install; only use import statements.

Use 'requests' and 'BeautifulSoup' (from bs4) for static HTML scraping.
Also import 'pandas' for structuring results.
Key patterns: requests.get(url), BeautifulSoup(html, 'lxml'),
soup.select(), soup.find_all(), tag.get_text(), tag['href'].""",

    "dynamic": """You are an expert Python programmer specializing in web scraping.
Generate clean, working Python code based on the user's natural language instruction.

RULES:
1. Return ONLY valid Python code inside a single ```python code block, followed by a brief explanation.
2. The code must be self-contained and immediately runnable.
3. Use try/except for error handling.
4. Store the final result in a variable called `result` — as a pandas DataFrame (preferred) or a list of dicts.
5. Add short comments on key steps.
6. Do NOT call pip install; only use import statements.

Use the 'selenium' package. A WebDriver object called `driver` is already open
and available in the local scope.
Key methods: driver.get(url), driver.find_element(By.CSS_SELECTOR, sel),
driver.find_elements(By.CSS_SELECTOR, sel), element.text, element.get_attribute('href'),
driver.execute_script(js), driver.page_source.
For waits use: WebDriverWait(driver, 10).until(EC.presence_of_element_located(...)).""",
}


def call_claude(
    url: str,
    instruction: str,
    page_html: str | None = None,
    method: str = "static",
    api_key: str | None = None,
    model: str = "claude-sonnet-4-20250514",
) -> dict:
    """Call Claude to generate scraping code.

    Returns dict with 'code' and 'explanation' keys.
    """
    client = anthropic.Anthropic(api_key=api_key)

    system_prompt = SYSTEM_PROMPTS.get(method, SYSTEM_PROMPTS["static"])
    user_prompt = _build_user_prompt(url, instruction, page_html, method)

    message = client.messages.create(
        model=model,
        max_tokens=4096,
        system=system_prompt,
        messages=[{"role": "user", "content": user_prompt}],
    )

    response_text = message.content[0].text
    return _parse_response(response_text)


def _build_user_prompt(
    url: str, instruction: str, page_html: str | None, method: str
) -> str:
    prompt = f"URL: {url}\nInstruction: {instruction}\nMethod: {method}"

    if page_html:
        # Truncate to ~8000 chars to stay within context limits
        if len(page_html) > 8000:
            page_html = page_html[:8000] + "\n... [truncated]"
        prompt += f"\n\nPage HTML (for context):\n{page_html}"

    return prompt


def _parse_response(text: str) -> dict:
    """Extract code block and explanation from Claude's response."""
    pattern = r"```(?:python|py)?\n(.*?)```"
    match = re.search(pattern, text, re.DOTALL)

    if match:
        code = match.group(1).strip()
    else:
        code = text.strip()

    explanation = re.sub(pattern, "", text, flags=re.DOTALL).strip()

    return {"code": code, "explanation": explanation}
