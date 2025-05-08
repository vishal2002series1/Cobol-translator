import os
from langchain_community.llms import Ollama

# In the future, you can add Bedrock or other providers here
class ModelInterface:
    def __init__(self, model_name=None, base_url=None):
        self.model_name = model_name or os.getenv("MODEL_NAME", "deepseek-r1:1.5b")
        self.base_url = base_url or os.getenv("OLLAMA_BASE_URL", "http://localhost:11434")
        self.llm = Ollama(model=self.model_name, base_url=self.base_url)

    def generate(self, prompt, **kwargs):
        return self.llm(prompt, **kwargs)