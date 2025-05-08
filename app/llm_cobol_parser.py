import json
from app.model_interface import ModelInterface

LLM_PARSER_PROMPT = """
You are a COBOL expert.
Given the following COBOL code, extract all procedure (paragraph) names and their code, ignoring comments and copybook includes.
Return a JSON array in this format:
[
  {"name": "0000-INITIALIZE", "code": "<code for this paragraph>"},
  {"name": "1000-ACCEPT-PAYMENT", "code": "<code for this paragraph>"},
  ...
]
COBOL CODE:
{cobol_code}
"""

def llm_parse_cobol_paragraphs(cobol_code: str, model_name=None):
    model = ModelInterface(model_name=model_name)
    prompt = LLM_PARSER_PROMPT.format(cobol_code=cobol_code[:8000])
    response = model.generate(prompt)
    print("LLM raw response:\n", response)  # For debugging
    try:
        # Try to extract the JSON array from the response
        start = response.find('[')
        end = response.rfind(']')
        if start == -1 or end == -1:
            raise ValueError("No JSON array found in LLM response.")
        json_str = response[start:end+1]
        paragraphs = json.loads(json_str)
        # Only keep items that are dicts and have both 'name' and 'code'
        clean_paragraphs = []
        for p in paragraphs:
            if isinstance(p, dict) and "name" in p and "code" in p:
                clean_paragraphs.append({"name": p["name"].strip(), "text": p["code"].strip()})
        if not clean_paragraphs:
            raise ValueError("No valid paragraphs found in LLM output.")
        return clean_paragraphs
    except Exception as e:
        print(f"LLM parsing failed: {e}\nResponse was:\n{response}")
        return []