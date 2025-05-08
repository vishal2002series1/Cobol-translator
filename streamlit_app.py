import streamlit as st
from app.model_interface import ModelInterface
from app.explain_tool import explain_cobol_code
from app.translate_tool import translate_cobol_code, translate_full_cobol_program, translate_full_cobol_program_with_modularity
from app.business_logic_extractor import extract_business_logic_and_modular_design


st.set_page_config(page_title="COBOL Translator", layout="wide")

st.title("COBOL Translator (GenAI Agentic POC)")

# Sidebar: Model and Language Selection
st.sidebar.header("Settings")
model_name = st.sidebar.text_input("Model Name", value="deepseek-r1:1.5b")
target_lang = st.sidebar.selectbox("Target Language", ["Python", "C#", "Java"])

# File uploader
uploaded_file = st.file_uploader("Upload COBOL file", type=["cbl", "cob", "txt"])
cobol_code = ""
if uploaded_file:
    cobol_code = uploaded_file.read().decode("utf-8")
else:
    cobol_code = st.text_area("Or paste COBOL code here", height=300)

if st.button("Explain & Translate"):
    if not cobol_code.strip():
        st.warning("Please upload or paste COBOL code.")
    else:
        st.info("Processing...")
        model = ModelInterface(model_name=model_name)
        # For now, just echo the input and a dummy translation
        explanation = model.generate(f"Explain this COBOL code:\n{cobol_code[:2000]}")
        translation = model.generate(f"Translate this COBOL code to {target_lang}:\n{cobol_code[:2000]}")
        st.subheader("Explanation")
        st.write(explanation)
        st.subheader(f"Translation ({target_lang})")
        st.code(translation, language=target_lang.lower())
if st.button("Explain Only"):
    if not cobol_code.strip():
        st.warning("Please upload or paste COBOL code.")
    else:
        st.info("Explaining COBOL code, please wait...")
        explanations = []
        progress_bar = st.progress(0)
        debug_area = st.empty()
        try:
            for i, total, name, explanation in explain_cobol_code(cobol_code, model_name=model_name, max_paragraphs=5):
                progress_bar.progress(i / total)
                st.subheader(name)
                st.write(explanation)
                explanations.append({"name": name, "explanation": explanation})
                debug_area.info(f"Explained {i} of {total} paragraphs...")
            progress_bar.empty()
            debug_area.empty()
        except Exception as e:
            st.error(f"Error during explanation: {e}")

if st.button("Translate Only"):
    if not cobol_code.strip():
        st.warning("Please upload or paste COBOL code.")
    else:
        st.info(f"Translating COBOL code to {target_lang}, please wait...")
        translations = []
        progress_bar = st.progress(0)
        debug_area = st.empty()
        try:
            for i, total, name, translation in translate_cobol_code(
                cobol_code, target_lang, model_name=model_name, max_paragraphs=5
            ):
                progress_bar.progress(i / total)
                st.subheader(name)
                st.code(translation, language=target_lang.lower())
                translations.append({"name": name, "translation": translation})
                debug_area.info(f"Translated {i} of {total} paragraphs...")
            progress_bar.empty()
            debug_area.empty()
        except Exception as e:
            st.error(f"Error during translation: {e}")

if st.button("Translate Full Program"):
    if not cobol_code.strip():
        st.warning("Please upload or paste COBOL code.")
    else:
        st.info(f"Translating full COBOL program to {target_lang}, please wait...")
        translation = translate_full_cobol_program(cobol_code, target_lang, model_name=model_name)
        st.subheader(f"Full Program Translation ({target_lang})")
        st.code(translation, language=target_lang.lower())
if st.button("Extract Business Logic & Modular Design"):
    if not cobol_code.strip():
        st.warning("Please upload or paste COBOL code.")
    else:
        st.info("Extracting business logic and modular design...")
        modular_design = extract_business_logic_and_modular_design(cobol_code, model_name=model_name)
        st.subheader("Business Logic & Modular Design")
        st.markdown(modular_design)

if st.button("Translate Full Program (Modular)"):
    if not cobol_code.strip():
        st.warning("Please upload or paste COBOL code.")
    else:
        st.info(f"Translating full COBOL program to {target_lang} with modular design, please wait...")
        translation = translate_full_cobol_program_with_modularity(cobol_code, target_lang, model_name=model_name)
        st.subheader(f"Full Program Translation ({target_lang}, Modular)")
        st.code(translation, language=target_lang.lower())