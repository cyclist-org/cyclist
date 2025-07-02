FROM ocaml/opam

LABEL maintainer="Reuben N. S. Rowe (reuben.rowe@rhul.ac.uk)"

ARG DEBIAN_FRONTEND=noninteractive

ENV TERM=linux

USER root

RUN apt-get --allow-releaseinfo-change update && apt-get install -y --no-install-recommends apt-utils
RUN apt-get --allow-releaseinfo-change update && apt-get install -y --no-install-recommends \
      autoconf \
      m4 \
      pkg-config \
      software-properties-common \
      rlwrap \
      grep \
      pcregrep \
      datamash \
  && sudo rm -rf /var/lib/apt/lists/*

# Install Spot
RUN (curl -s https://www.lrde.epita.fr/repo/debian.gpg | apt-key add -) \
  && echo 'deb http://www.lrde.epita.fr/repo/debian/ stable/' >> /etc/apt/sources.list \
  && apt-get update \
  && apt-get install -y --no-install-recommends \
       spot \
       libspot-dev

USER opam

WORKDIR /home/opam

RUN git clone https://github.com/cyclist-org/cyclist.git \
  && cd cyclist \
  && eval `opam config env` \
  && opam update \
  && opam install -y --deps-only ./cyclist.opam \
  && dune build

WORKDIR /home/opam/cyclist