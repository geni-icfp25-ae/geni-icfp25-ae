{ lib, fetchgit, pkgs, callPackage, ocaml-ng }:

let 
  # Dice uses OCaml 4.09. Since we only need the `dice` 
  # executable, we use our own OCaml version and libraries 
  # without interfering with Mappl's OCaml environment.
  ocamlPackages = ocaml-ng.ocamlPackages_4_09; 
in

let 
  cudd = callPackage ./mlcuddidl.nix { ocamlPackages = ocamlPackages; };  
in 

ocamlPackages.buildDunePackage rec {
  pname = "dice";
  version = "097363cb38dfee2bc8108387d0671d566db11a5e";

  src = fetchgit {
    url = "https://nix:gldt-8yJk9KRtP4CJyezMTU6g@git.uwaterloo.ca/jianlin.li/dice.git";
    rev = version;
    hash = "sha256-niRs/TCedTi4y3oSg8jvRV5MVTstZTGcfj43F6F3Fks=";
  };

  nativeBuildInputs = with ocamlPackages; [
    menhir
  ] ++ [
    pkgs.makeWrapper
  ];

  propagatedBuildInputs = with ocamlPackages; [
    menhir
    core
    ounit2
    ppx_sexp_conv
    sexplib
    core_bench
    ppx_deriving
    yojson
    ctypes
    bignum
    menhirLib
    ppx_jane
  ] ++ [ cudd ];

  # set stack size to 64 GB 
  postInstall = ''
      wrapProgram $out/bin/dice --run "ulimit -s ${builtins.toString (64 * 1024 * 1024 * 1024)}"
  '';

}
