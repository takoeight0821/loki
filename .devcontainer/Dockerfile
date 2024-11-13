FROM mcr.microsoft.com/devcontainers/base:ubuntu

RUN apt-get update && apt-get install -y build-essential curl libffi-dev libffi8ubuntu1 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5

USER vscode

ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=1

ENV BOOTSTRAP_HASKELL_GHC_VERSION=9.10.1
ENV BOOTSTRAP_HASKELL_CABAL_VERSION=3.12.1.0
ENV BOOTSTRAP_HASKELL_INSTALL_HLS=1
ENV BOOTSTRAP_HASKELL_HLS_VERSION=2.9.0.1

RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

# https://learn.microsoft.com/ja-jp/dotnet/core/install/linux-scripted-manual#scripted-install

WORKDIR /home/vscode

RUN wget https://dot.net/v1/dotnet-install.sh -O dotnet-install.sh

RUN chmod +x dotnet-install.sh

RUN ./dotnet-install.sh --channel 9.0