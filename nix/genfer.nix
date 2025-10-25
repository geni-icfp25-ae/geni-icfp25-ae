{ lib, fetchgit, rustPlatform, pkgs, applyPatches}:

rustPlatform.buildRustPackage rec {
  pname = "genfer";
  version = "1b7309b97e278ad09752de4317b2c7cca9b937fe";
  
  nativeBuildInputs = with pkgs; [
    gnum4
  ];

  src = fetchgit {
    url = "https://nix:gldt-LcpiWj2MgHrHXjgNSWsF@git.uwaterloo.ca/jianlin.li/genfer.git";
    rev = version;
    hash = "sha256-aLBSUzWtniHAtXsh7xiLfG9Lx/xl6VQSp9QPrflTafc=";
  };

  cargoHash = "sha256-wMuQj1IRZkRxRccpXI7BBqZ26AwxKdeYmV6OFtT0YhM=";

  # Skip running cargo tests
  doCheck = false;

  meta = {
    description = "Tool for Bayesian inference on discrete models expressed as probabilistic programs, via generating functions";
    homepage = "https://github.com/fzaiser/genfer";
    license = lib.licenses.mit;
    maintainers = [ ];
  };
}
