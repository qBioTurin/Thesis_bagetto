# syntax=docker/dockerfile:1.7

FROM rocker/r-ver:4.6.0

ENV DEBIAN_FRONTEND=noninteractive
ENV R_PROFILE_USER=/dev/null
ENV R_ENVIRON_USER=/dev/null

RUN --mount=type=cache,target=/var/cache/apt,sharing=locked \
    --mount=type=cache,target=/var/lib/apt,sharing=locked \
    apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    cmake \
    pkg-config \
    gfortran \
    ca-certificates \
    curl \
    git \
    python3 \
    python3-pip \
    libcurl4-openssl-dev \
    libssl-dev \
    libsodium-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libcairo2-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libgit2-dev \
    libxt-dev \
    libudunits2-dev \
    libgsl-dev \
    libglpk-dev \
    libproj-dev \
    proj-bin \
    proj-data \
    libnlopt-dev \
    libmbedtls-dev \
    libgmp3-dev \
    libmpfr-dev \
    zlib1g-dev \
    libbz2-dev \
    liblzma-dev \
    libuv1-dev \
    pandoc \
    docker.io \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /opt/app

# Copy dependency installer first to leverage Docker layer caching.
COPY R/Packages.R /opt/app/R/Packages.R

# Install all runtime dependencies only at build time via Packages.R.
RUN R --vanilla -q -e "options(timeout=1200); Sys.setenv(MICROWORKFLOW_INSTALL_DEPS='true'); source('/opt/app/R/Packages.R')"

# Fail fast during image build if auth dependencies are missing.
RUN R --vanilla -q -e "stopifnot(requireNamespace('sodium', quietly=TRUE)); stopifnot(requireNamespace('shinyauthr', quietly=TRUE))"

# Copy the rest of the project after dependency installation.
COPY . /opt/app

RUN chmod +x /opt/app/docker/entrypoint.sh

EXPOSE 3838

CMD ["/opt/app/docker/entrypoint.sh"]
