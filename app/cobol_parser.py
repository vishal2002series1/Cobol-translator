import re

def parse_cobol_paragraphs(cobol_code: str):
    """
    Extracts paragraphs from COBOL code.
    Returns a list of dicts: [{name, text}]
    """
    # Match lines like '0000-INITIALIZE.' or '1000-ACCEPT-PAYMENT.'
    pattern = re.compile(r"^(\s*\d{4}-[A-Z0-9-]+)\.", re.MULTILINE | re.IGNORECASE)
    matches = list(pattern.finditer(cobol_code))
    paragraphs = []
    for i, match in enumerate(matches):
        start = match.end()
        end = matches[i+1].start() if i+1 < len(matches) else len(cobol_code)
        name = match.group(1).strip()
        text = cobol_code[start:end].strip()
        paragraphs.append({"name": name, "text": text})
    print(f"Parsed {len(paragraphs)} paragraphs: {[p['name'] for p in paragraphs]}")
    return paragraphs