{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-24.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; config.allowUnfree = true; config.allowUnsupportedSystem = true; config.allowBroken = true; };
        dice = if pkgs.stdenv.isLinux then pkgs.callPackage ./nix/dice.nix { } else null;
        bngen = if pkgs.stdenv.isLinux then pkgs.callPackage ./nix/bngen.nix { } else null;
        c-memo = if pkgs.stdenv.isLinux then pkgs.callPackage ./nix/c-memo.nix { } else null;                
        genfer = pkgs.callPackage ./nix/genfer.nix { };
        gennifer = pkgs.callPackage ./nix/gennifer.nix { };
        problog = pkgs.callPackage ./nix/problog.nix { };
        plot = pkgs.callPackage ./nix/plot.nix { };
      in
      {
        devShell = pkgs.mkShell {
          buildInputs = (with pkgs; [
            which
            bash
            fish
            zsh
            hyperfine
            nano
            vim
            tree
            unixtools.top
            htop
            (python3.withPackages (ps: with ps; [ 
              matplotlib
              pandas
              pyro-ppl
            ]))
            (with ocamlPackages; [
              ocaml
              findlib
            ])
            rustc
            cargo
          ]) ++ [
            bngen
            dice
            gennifer
            genfer
            problog
            c-memo
          ] ++ [
            plot
          ];
          # LD_LIBRARY_PATH = "${pkgs.stdenv.cc.cc.lib}/lib";
          #  nix eval --raw --impure --expr 'with import <nixpkgs> {}; stdenv.cc.cc.lib'
          shellHook = ''exec fish'';
        };
        packages = {
          gennifer = gennifer;
          dice = dice;
          problog = problog;
          genfer = genfer;
          bngen = bngen;
          c-memo = c-memo;
          plot = plot;
          pyro-ppl = pkgs.python3.pkgs.pyro-ppl;
          ocaml = pkgs.ocamlPackages.ocaml;
          cargo = pkgs.cargo;
          rustc = pkgs.rustc;
        };
      }
    );
}
