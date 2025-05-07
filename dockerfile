FROM python:3.11-slim

WORKDIR /workspace

COPY requirements.txt .
RUN pip install --no-cache-dir -r requirements.txt

# Optional: install git, curl, etc.
RUN apt-get update && apt-get install -y git curl

CMD [ "bash" ]