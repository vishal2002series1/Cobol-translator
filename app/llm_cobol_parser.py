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
    prompt = LLM_PARSER_PROMPT.format(cobol_code=cobol_code[:8000])  # Truncate if needed
    response = model.generate(prompt)
    # Try to extract JSON from the response
    try:
        # Find the first '[' and last ']' to extract JSON array
        start = response.find('[')
        end = response.rfind(']')
        json_str = response[start:end+1]
        paragraphs = json.loads(json_str)
        # Ensure each item has 'name' and 'code'
        return [
            {"name": p["name"].strip(), "text": p["code"].strip()}
            for p in paragraphs if "name" in p and "code" in p
        ]
    except Exception as e:
        print(f"LLM parsing failed: {e}\nResponse was:\n{response}")
        return []