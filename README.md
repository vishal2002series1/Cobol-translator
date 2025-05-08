# COBOL Translator (GenAI Agentic POC)

A modern, agentic AI tool to **explain and translate COBOL code** into Python, C#, or Java.  
Built with Streamlit, LangChain, and Ollama (using `deepseek-r1:1.5b`), with LLMOps best practices and a migration path to AWS Bedrock.

---

## Features

- **COBOL Code Upload:** Paste or upload COBOL source files.
- **Automated Explanation:** Get plain-English explanations of COBOL paragraphs and programs.
- **Code Translation:** Translate COBOL to Python, C#, or Java.
- **Interactive UI:** Streamlit web app for easy use and comparison.
- **LLMOps Ready:** Prompt versioning, golden file tests, modular model interface.
- **Easy Migration:** Swap between Ollama (local) and AWS Bedrock (cloud) with minimal changes.

---

## Quickstart

### 1. Prerequisites

- [Docker](https://www.docker.com/)
- [Ollama](https://ollama.com/download) (running locally)
- [VS Code](https://code.visualstudio.com/) + "Dev Containers" extension
- Python 3.11+ (if running outside container)
- Git

### 2. Clone and Setup

```sh
git clone https://github.com/your-username/cobol-translator.git
cd cobol-translator