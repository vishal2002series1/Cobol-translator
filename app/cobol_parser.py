import re
from app.llm_cobol_parser import llm_parse_cobol_paragraphs

def parse_cobol_paragraphs(cobol_code: str, model_name=None):
    """
    Try regex-based parsing first. If no paragraphs found, fall back to LLM-based parsing.
    """
    # Regex: match lines like '0000-INITIALIZE.' or '1000-ACCEPT-PAYMENT.'
    pattern = re.compile(r"^(\s*\d{4}-[A-Z0-9-]+)\.", re.MULTILINE | re.IGNORECASE)
    matches = list(pattern.finditer(cobol_code))
    paragraphs = []
    for i, match in enumerate(matches):
        start = match.end()
        end = matches[i+1].start() if i+1 < len(matches) else len(cobol_code)
        name = match.group(1).strip()
        text = cobol_code[start:end].strip()
        paragraphs.append({"name": name, "text": text})
    if paragraphs:
        print(f"Regex parser: Parsed {len(paragraphs)} paragraphs: {[p['name'] for p in paragraphs]}")
        return paragraphs
    # Fallback to LLM parser
    print("Regex parser found 0 paragraphs. Falling back to LLM-based parser...")
    return llm_parse_cobol_paragraphs(cobol_code, model_name=model_name)