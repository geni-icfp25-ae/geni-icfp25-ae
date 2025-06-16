FROM nixos/nix

RUN mkdir -p /etc/nix && echo "experimental-features = nix-command flakes" > /etc/nix/nix.conf

# Copy the current directory into the image
COPY . /geni-icfp25-ae
WORKDIR /geni-icfp25-ae

RUN nix build .#pyro-ppl
RUN nix build .#ocaml
RUN nix build .#rustc
RUN nix build .#cargo
RUN nix build .#c-memo
RUN nix build .#bngen
RUN nix build .#dice
RUN nix build .#problog
RUN nix build .#genfer
RUN nix build .#gennifer

# ENV LC_ALL=en_US.UTF-8
ENV LANG=en_US.UTF-8
ENV LANGUAGE=en_US:en

CMD ["nix", "develop"]
# CMD ["nix", "--extra-experimental-features",  "nix-command", "--extra-experimental-features", "flakes", "develop"]