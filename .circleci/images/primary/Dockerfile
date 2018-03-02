FROM circleci/openjdk:latest-node-browsers

WORKDIR /home/

ENV PATH /home/circleci/.local/bin:$PATH

RUN sudo apt-get --quiet update
RUN sudo apt-get --quiet install python python-pip rsync texlive-latex-base texlive-latex-extra
RUN pip install html5validator
RUN (curl -sSL https://get.haskellstack.org/ | sh) || true
RUN stack upgrade
