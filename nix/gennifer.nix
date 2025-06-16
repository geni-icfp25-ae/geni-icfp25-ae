{ lib, fetchFromGitHub, rustPlatform, pkgs, applyPatches}:

rustPlatform.buildRustPackage rec {
  useFetchCargoVendor = true;

  pname = "gennifer";
  version = "1.0";

  src = ../.;

  cargoHash = "sha256-W9YAXdMxARa5mQ+WGg/UhtRlTeGG56MP9LMJwxwhmak=";

  nativeBuildInputs = with pkgs; [
    gnum4
  ];
  
  meta = {
    license = lib.licenses.mit;
    maintainers = [ ];
  };
}
