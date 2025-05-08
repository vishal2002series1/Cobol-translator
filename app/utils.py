import os
import datetime

def load_prompt(filepath):
    """Load prompt template from a file"""
    try:
        with open(filepath, "r") as f:
            return f.read()
    except FileNotFoundError:
        print(f"Warning: Prompt file {filepath} not found. Using default prompt.")
        return None

def generate_metadata(model_name: str, prompt_file: str):
    """Generate metadata for tracking model and prompt versions"""
    return {
        "model_name": model_name or os.getenv("MODEL_NAME", "deepseek-r1:1.5b"),
        "prompt_file": prompt_file,
        "timestamp": datetime.datetime.now().isoformat()
    }