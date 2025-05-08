import os
from app.model_interface import ModelInterface

def test_translation_against_golden():
    model = ModelInterface()
    with open("golden/sample_cobol.golden", "r") as f:
        cobol_code = f.read()
    # This is a dummy test; in real use, compare to a stored expected output
    translation = model.generate(f"Translate this COBOL code to Python:\n{cobol_code[:2000]}")
    assert "def" in translation or "class" in translation  # crude check for Python code