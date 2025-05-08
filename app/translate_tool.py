from app.model_interface import ModelInterface
from app.utils import load_prompt, generate_metadata
from app.business_logic_extractor import extract_business_logic_and_modular_design
import re

def clean_llm_output(text):
    # Remove  and similar tokens
    # return re.sub(r"|<\|im_start\|>|<\|im_end\|>", "", text, flags=re.IGNORECASE).strip()
    return re.sub(r"<think>.*?</think>", "", text, flags=re.DOTALL | re.IGNORECASE).strip()

TRANSLATION_PROMPT = """
You are an expert software engineer.
Translate the following COBOL paragraph into {target_lang} code.
Preserve the logic and variable names as much as possible.
Add comments to explain the code.
COBOL CODE:
{cobol_code}
"""

FULL_PROGRAM_PROMPT = """
You are an expert developer.
Using the following modular design and COBOL code, generate a modern, modular {target_lang} code file.
Modular Design:
{modular_design}
COBOL CODE:
{cobol_code}
"""

def translate_cobol_code(cobol_code: str, target_lang: str, model_name=None, max_paragraphs=5):
    """Translate COBOL code paragraph by paragraph"""
    model = ModelInterface(model_name=model_name)
    paragraphs = parse_cobol_paragraphs(cobol_code, model_name=model_name)
    total = min(len(paragraphs), max_paragraphs)
    translations = []

    # Generate metadata
    metadata = generate_metadata(model_name, "prompts/translate_paragraph.txt")

    for i, para in enumerate(paragraphs[:max_paragraphs]):
        prompt = TRANSLATION_PROMPT.format(target_lang=target_lang, cobol_code=para["text"])
        try:
            translation = model.generate(prompt)
        except Exception as e:
            translation = f"Error from LLM: {e}"
        translations.append({
            "name": para["name"],
            "translation": translation
        })
        yield i + 1, total, para["name"], translation, metadata

def translate_full_cobol_program_with_modularity(cobol_code: str, target_lang: str, model_name=None):
    """Translate full COBOL program with modular design"""
    modular_design = extract_business_logic_and_modular_design(cobol_code, model_name=model_name)
    model = ModelInterface(model_name=model_name)

    # Generate metadata
    metadata = generate_metadata(model_name, "prompts/translate_full_program.txt")

    prompt = FULL_PROGRAM_PROMPT.format(
        target_lang=target_lang,
        modular_design=modular_design,
        cobol_code=cobol_code[:8000]
    )

    try:
        translation = model.generate(prompt)
        translation = clean_llm_output(translation)
        return {
            "metadata": metadata,
            "output": translation,
            "modular_design": modular_design
        }
    except Exception as e:
        return {
            "metadata": metadata,
            "error": f"Error from LLM: {e}",
            "modular_design": modular_design
        }