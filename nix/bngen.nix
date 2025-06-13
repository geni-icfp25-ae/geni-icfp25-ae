{ocamlPackages, ocaml-ng, fetchgit}:

let 
  ocamlPackages = ocaml-ng.ocamlPackages_4_09; 
in
ocamlPackages.buildDunePackage rec {
  pname = "bngen";
  version = "36ecad6f14dc65fbf86cfa85b1872aea6f2aaf42";

  src = fetchgit {
    url = "https://nix:gldt-QJsJae7VYsFms1QE5Ay2@git.uwaterloo.ca/jianlin.li/bngen.git";
    rev = version;
    hash = "sha256-YRD6m0/GA3HfzaZTEYRKa9LTVvq6qK3HXhy/QAPqdfE=";
  };

  nativeBuildInputs = with ocamlPackages; [
    menhir
  ];

  propagatedBuildInputs = with ocamlPackages; [
    dune_3
    core
    menhir
  ];
}
