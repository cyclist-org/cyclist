FROM ocaml/opam2

LABEL maintainer="Reuben N. S. Rowe (reuben.rowe@rhul.ac.uk)"

ARG DEBIAN_FRONTEND=noninteractive

ENV TERM linux

USER root

RUN apt-get update && apt-get install -y --no-install-recommends apt-utils
RUN apt-get update && apt-get install -y --no-install-recommends \
      autoconf \
      libpcre3-dev \
      m4 \
      pkg-config \
      software-properties-common \
  && sudo rm -rf /var/lib/apt/lists/*
RUN (curl -s https://www.lrde.epita.fr/repo/debian.gpg | apt-key add -) \
  && echo 'deb http://www.lrde.epita.fr/repo/debian/ stable/' >> /etc/apt/sources.list \
  && apt-get update \
  && apt-get install -y --no-install-recommends \
       spot \
       libspot-dev

USER opam

# 4.05 is the latest compiler version that we can build the Jane Street testbed on
RUN  opam repo -a add ocaml.org https://opam.ocaml.org/ \
  && opam update \
  && opam install -y dune hashset hashcons pcre mparser ocamlgraph

WORKDIR /home/opam

RUN git clone https://github.com/ngorogiannis/cyclist.git \
  && cd cyclist \
  && eval `opam config env` \
  && dune build

ENV PATH /home/opam/cyclist/_build/install/default/bin:$PATH