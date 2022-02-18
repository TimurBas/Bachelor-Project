FROM ubuntu:20.04

ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update && apt-get -y install \
    unzip \
    zip \
    make \
    gcc \
    m4 \
    rlwrap \
    clang \
    curl \
    patch \
    git \
    # for VS Code live sharing
    libicu66 \
    # for dune build --watch
    inotify-tools \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

ENV OPAMYES=true OPAMROOTISOK=true
RUN curl -sL https://github.com/ocaml/opam/releases/download/2.1.0/opam-2.1.0-x86_64-linux -o opam \
    && install opam /usr/local/bin/opam \
    && opam init --disable-sandboxing -a -y --bare \
    && opam update

RUN opam switch create 4.12.0

# Install these dependencies early to increase intermediate image reuse
COPY ./tiger.opam* .

RUN opam install .  --deps-only --locked && \
    opam install ocaml-lsp-server -y && \
    opam user-setup install && \
    eval $(opam env)