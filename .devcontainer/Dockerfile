# accelerate requires nvcc, which is included in cuda's devel images
# https://hub.docker.com/r/nvidia/cuda/
FROM nvidia/cuda:11.7.1-devel-ubuntu20.04
LABEL maintainer "Trevor L. McDonell <trevor.mcdonell@gmail.com>"

ENV LANG C.UTF-8
ENV LC_ALL C.UTF-8
ENV LD_LIBRARY_PATH "/usr/local/cuda/lib64:/usr/local/cuda/nvvm/lib64:${LD_LIBRARY_PATH}"

# tzdata is a dependency of the packages, which requires a set timezone
ARG CONTAINER_TIMEZONE=Europe/Amsterdam
RUN ln -snf /usr/share/zoneinfo/${CONTAINER_TIMEZONE} /etc/localtime && echo ${CONTAINER_TIMEZONE} > /etc/timezone

# install depedencies to add package sources
ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get update \
 && apt-get install -y gnupg2 curl pkg-config software-properties-common

# add latest stable git source to allow ssh commit signing
RUN add-apt-repository ppa:git-core/ppa -y \
 && apt-get update \
 && apt-get install -y git

# install LLVM
ARG LLVM_VERSION=12
RUN curl https://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add - \
 && add-apt-repository "deb http://apt.llvm.org/focal/ llvm-toolchain-focal-${LLVM_VERSION} main" \
 && apt-get update \
 && apt-get install -y \
        clang-${LLVM_VERSION} \
        libclang-${LLVM_VERSION}-dev \
        llvm-${LLVM_VERSION}-dev \
        g++ \
        make

# install dependencies for quickhull assignment
RUN apt-get update \
 && apt-get install -y \
        freeglut3 \
        freeglut3-dev \
        libgmp10 \
        libgmp-dev \
        libcapstone-dev \
        libfreetype6-dev \
        libglfw3-dev \
        libspice-client-gtk-3.0-dev \
        libtbb-dev

# finished installing with apt
RUN apt-get clean

# create the vscode user
ARG USERNAME=vscode
RUN adduser ${USERNAME}
# RUN mkdir -p /home/${USERNAME}/.ssh \
#  && mkdir -p /home/${USERNAME}/.ghcup \
#  && mkdir -p /home/${USERNAME}/.cabal \
#  && mkdir -p /home/${USERNAME}/.stack \
#  && mkdir -p /home/${USERNAME}/.vscode-server/extensions \
#  && chown -R ${USERNAME}:${USERNAME} /home/${USERNAME}
ENV PATH "/home/${USERNAME}/.local/bin:/home/${USERNAME}/.cabal/bin:/home/${USERNAME}/.ghcup/bin:${PATH}"
USER "${USERNAME}"

# install minimal ghcup
ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=True
ENV BOOTSTRAP_HASKELL_MINIMAL=True
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

# install ghc and related tools
ARG GHC_VERSION=9.0.2
ARG HSL_VERSION=1.9.0.0
RUN ghcup install cabal
RUN ghcup install stack
RUN ghcup install ghc --set ${GHC_VERSION}
RUN ghcup install hls ${HSL_VERSION}

# set shell to bash to use auto completion (e.g. arrow up for last command)
ENV SHELL="/bin/bash"

