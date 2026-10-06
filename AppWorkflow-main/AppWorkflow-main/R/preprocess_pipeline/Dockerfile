FROM python:3.9-slim

ENV PYTHONUNBUFFERED=1 \
    PIP_DISABLE_PIP_VERSION_CHECK=1

WORKDIR /

# Keep runtime small while retaining tools needed by preprocessing scripts.
RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates \
    bash \
    && rm -rf /var/lib/apt/lists/*

RUN pip install --no-cache-dir \
    kraken-biom \
    metaphlan==4.2.2

COPY scripts/ /scripts/
RUN ln -s /scripts /opt/classifier

CMD ["bash"]
