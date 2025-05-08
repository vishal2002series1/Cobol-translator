from app.model_interface import ModelInterface
from app.cobol_parser import parse_cobol_paragraphs
from app.business_logic_extractor import extract_business_logic_and_modular_design
import re



# TRANSLATION_PROMPT = """
# You are an expert software engineer.
# Translate the following COBOL paragraph into {target_lang} code.
# Preserve the logic and variable names as much as possible.
# Add comments to explain the code.
# COBOL CODE:
# {cobol_code}
# """

# FULL_PROGRAM_PROMPT = """
# You are an expert software engineer.
# Translate the following COBOL program into a single, runnable {target_lang} code file.
# - Each COBOL paragraph should become a function/method.
# - The main entry point should call these functions in the same order as the COBOL program's execution flow.
# - Preserve variable names and logic as much as possible.
# - Add comments to explain the code.
# - Output only the code file, no explanation.
# - Once you have generated all the codes. Once again write a complete code and put your final code in between <Final Code> </Final Code>.

# COBOL CODE:
# {cobol_code}
# """

def load_prompt(filepath):
    with open(filepath, "r") as f:
        return f.read()

BUSINESS_LOGIC_PROMPT = load_prompt("app/prompts/business_logic_extraction.txt")

def clean_llm_output(text):
    # Remove  and similar tokens
    # return re.sub(r"|<\|im_start\|>|<\|im_end\|>", "", text, flags=re.IGNORECASE).strip()
    return re.sub(r"<think>.*?</think>", "", text, flags=re.DOTALL | re.IGNORECASE).strip()

def translate_cobol_code(cobol_code: str, target_lang: str, model_name=None, max_paragraphs=5):
    model = ModelInterface(model_name=model_name)
    paragraphs = parse_cobol_paragraphs(cobol_code, model_name=model_name)
    total = min(len(paragraphs), max_paragraphs)
    translations = []
    for i, para in enumerate(paragraphs[:max_paragraphs]):
        # prompt = TRANSLATION_PROMPT.format(target_lang=target_lang, cobol_code=para["text"])
        prompt = load_prompt("app/prompts/translation_prompt.txt").format(target_lang=target_lang, cobol_code=para["text"])
        try:
            translation = model.generate(prompt)
            translation = clean_llm_output(translation)
        except Exception as e:
            translation = f"Error from LLM: {e}"
        translations.append({"name": para["name"], "translation": translation})
        yield i + 1, total, para["name"], translation

def translate_full_cobol_program(cobol_code: str, target_lang: str, model_name=None):
    model = ModelInterface(model_name=model_name)
    # prompt = FULL_PROGRAM_PROMPT.format(target_lang=target_lang, cobol_code=cobol_code[:8000])
    prompt = load_prompt("app/prompts/translate_full_program.txt").format(target_lang=target_lang, cobol_code=cobol_code[:8000])
    try:
        translation = model.generate(prompt)
        translation = clean_llm_output(translation)
    except Exception as e:
        translation = f"Error from LLM: {e}"
    return translation

def translate_full_cobol_program_with_modularity(cobol_code: str, target_lang: str, model_name=None):
    modular_design = extract_business_logic_and_modular_design(cobol_code, model_name=model_name)
    model = ModelInterface(model_name=model_name)
    # prompt = f"""
    # You are an expert developer.
    # Using the following modular design and COBOL code, generate a modern, modular {target_lang} code file.
    # Modular Design:
    # {modular_design}

    # - Once you have generated all the codes. Once again write a complete code and put your final code in between <Final Code> </Final Code> xml tags.
    # COBOL CODE:
    # {cobol_code[:8000]}
    # """
    prompt = load_prompt("app/prompts/translate_with_modularity.txt").format(target_lang=target_lang,modular_design=modular_design,cobol_code=cobol_code[:8000])
    try:
        translation = model.generate(prompt)
    except Exception as e:
        translation = f"Error from LLM: {e}"
    return translation