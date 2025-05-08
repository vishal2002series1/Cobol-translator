from app.model_interface import ModelInterface

# BUSINESS_LOGIC_PROMPT = """
# You are a software architect.
# Analyze the following COBOL program and extract its business logic in plain English.
# Then, suggest a modular design for rewriting this program in a modern language (such as Python, C#, or Java).
# The modular design should include:
# - Main modules/classes/functions
# - Their responsibilities
# - How they interact (call relationships)
# Present the modular design as a list and as a simple diagram (ASCII or Mermaid).
# COBOL CODE:
# {cobol_code}
# """

def load_prompt(filepath):
    with open(filepath, "r") as f:
        return f.read()

BUSINESS_LOGIC_PROMPT = load_prompt("app/prompts/business_logic_extraction.txt")

def extract_business_logic_and_modular_design(cobol_code: str, model_name=None):
    model = ModelInterface(model_name=model_name)
    prompt = BUSINESS_LOGIC_PROMPT.format(cobol_code=cobol_code[:8000])
    try:
        result = model.generate(prompt)
    except Exception as e:
        result = f"Error from LLM: {e}"
    return result