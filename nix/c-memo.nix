{ pkgs, fetchgit}:

pkgs.stdenv.mkDerivation rec {
  pname = "C-Memo";
  version = "7e5a785690df7a5eb77794361b33797f9740073e";

  src = fetchgit {
    url = "https://git.uwaterloo.ca/jianlin.li/c-memo.git";
    rev = version;
    hash = "sha256-d47wduAu+/OUcPqPSb3D+sd7VYRafRLzo1/Y8n33Umc=";
  };

  buildInputs = [ pkgs.gcc pkgs.gnumake ];

  installPhase = ''
      mkdir -p $out/include
      mkdir -p $out/lib
      make install
      cp *.h $out/include
      cp *.a $out/lib
      '';
}
