FROM nixos/nix

ARG REPO=geni-icfp25-ae
ARG URL=https://github.com/geni-icfp25-ae/${REPO}.git

RUN git clone ${URL}
WORKDIR /${REPO}
RUN nix --extra-experimental-features  nix-command --extra-experimental-features flakes develop 

# ENV LC_ALL en_US.UTF-8
# ENV LANG en_US.UTF-8
# ENV LANGUAGE en_US:en

CMD ["nix", "--extra-experimental-features",  "nix-command", "--extra-experimental-features", "flakes", "develop"]