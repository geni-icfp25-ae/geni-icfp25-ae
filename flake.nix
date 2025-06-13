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
        profiling = if pkgs.stdenv.isLinux then pkgs.callPackage ./nix/profiling.nix { } else null;
        valgrind = if pkgs.stdenv.isLinux then pkgs.valgrind else null;
        c-memo = if pkgs.stdenv.isLinux then pkgs.callPackage ./nix/c-memo.nix { } else null;
        clad = if pkgs.stdenv.isLinux then pkgs.callPackage ./nix/clad.nix { llvmPackages = pkgs.llvmPackages_18; } else null;
        gennifer = pkgs.callPackage ./nix/gennifer.nix { };
        genfer = pkgs.callPackage ./nix/genfer.nix { };
        problog = pkgs.callPackage ./nix/problog.nix { };
        plot = pkgs.callPackage ./nix/plot.nix { };
        plot-csv = pkgs.callPackage ./nix/plot-csv.nix { };
        cmemo = pkgs.callPackage ./nix/cmemo.nix { };
      in
      {
        devShell = pkgs.mkShell {
          buildInputs = (with pkgs; [
            clang
            which
            bash
            fish
            zsh
            hyperfine
            (python3.withPackages (ps: with ps; [ 
              matplotlib
              pandas
              pygraphviz
              pyro-ppl
              numpyro
              funsor
            ]))
            (with ocamlPackages; [
              ocaml
              (if pkgs.stdenv.isLinux then owl else null)
              findlib
            ])
            rustc
            cargo
          ]) ++ [
            bngen
            dice
            gennifer
            c-memo
            genfer
            problog
            clad
          ] ++ [
            profiling
            valgrind
            plot
            plot-csv
          ];
          shellHook = ''exec fish'';
        };
      }
    );
}
