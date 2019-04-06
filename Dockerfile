FROM nixos/nix:2.1.3

RUN apk add --no-cache ca-certificates

WORKDIR /home/

ENV PATH /home/circleci/.local/bin:$PATH

RUN mkdir -pv /home/circleci/project
WORKDIR /home/circleci/project

COPY markkarpov-com.cabal markkarpov-com.cabal
COPY app/Main.hs app/Main.hs
COPY default.nix default.nix
COPY nix/nixpkgs/default.nix nix/nixpkgs/default.nix
COPY nix/texlive-custom/default.nix nix/texlive-custom/default.nix
COPY nix/html5validator/default.nix nix/html5validator/default.nix

# Try to build all the stuff to make sure that everything is cached locally,
# so subsequent builds on CI are really fast.

RUN nix-build
RUN nix-build nix/texlive-custom
RUN nix-build nix/nixpkgs -A rsync
RUN nix-build nix/nixpkgs -A openssh
RUN nix-build nix/nixpkgs -A openjdk
RUN nix-build nix/nixpkgs -A glibcLocales
RUN nix-build nix/html5validator

WORKDIR /home/circleci
RUN rm -rvf project
