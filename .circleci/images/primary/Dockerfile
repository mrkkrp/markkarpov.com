FROM circleci/openjdk:latest-node-browsers

WORKDIR /home/

ENV PATH /home/circleci/.local/bin:$PATH

RUN sudo apt-get --quiet update
RUN sudo apt-get --quiet install python python-pip rsync texlive-latex-base texlive-latex-extra libtinfo-dev
RUN pip install html5validator
RUN (curl -sSL https://get.haskellstack.org/ | sh) || true
RUN stack upgrade

WORKDIR /home/circleci
RUN git clone https://github.com/mrkkrp/markkarpov.com project
WORKDIR /home/circleci/project
COPY stack.yaml stack.yaml
RUN stack setup
RUN stack build
RUN stack test
WORKDIR /home/circleci
RUN rm -rvf project
