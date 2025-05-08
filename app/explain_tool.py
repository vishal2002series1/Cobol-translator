from app.model_interface import ModelInterface
from app.cobol_parser import parse_cobol_paragraphs

def explain_cobol_code(cobol_code: str, model_name=None, max_paragraphs=5):
    """
    Yields (current_index, total, paragraph_name, explanation) for progress feedback.
    Limits to max_paragraphs for speed.
    """
    model = ModelInterface(model_name=model_name)
    paragraphs = parse_cobol_paragraphs(cobol_code)
    total = min(len(paragraphs), max_paragraphs)
    for i, para in enumerate(paragraphs[:max_paragraphs]):
        prompt = (
            f"You are an expert COBOL analyst. "
            f"Explain the following COBOL paragraph in plain English, keeping variable names:\n\n"
            f"{para['text']}"
        )
        try:
            explanation = model.generate(prompt)
        except Exception as e:
            explanation = f"Error from LLM: {e}"
        yield i + 1, total, para["name"], explanation