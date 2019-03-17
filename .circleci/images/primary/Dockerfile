FROM circleci/openjdk:latest-node-browsers

WORKDIR /home/

ENV PATH /home/circleci/.local/bin:$PATH

RUN sudo apt-get --quiet update
RUN sudo apt-get --quiet install python python-pip rsync texlive-latex-base texlive-latex-extra libtinfo-dev
RUN pip install html5validator
RUN (curl -sSL https://get.haskellstack.org/ | sh) || true
RUN stack upgrade

RUN mkdir -pv /home/circleci/project
WORKDIR /home/circleci/project

COPY stack.yaml stack.yaml
COPY markkarpov-com.cabal markkarpov-com.cabal
COPY Setup.hs Setup.hs

RUN stack setup
RUN stack build --only-dependencies --test --no-run-tests

WORKDIR /home/circleci
RUN rm -rvf project
