{ pkgs, fetchFromGitHub, llvmPackages }:

pkgs.stdenv.mkDerivation rec {
  pname = "clad";
  version = "1b45486adcff1dd9911855af2d7cd4dcdea2a467";

  src = fetchFromGitHub {
    # https://github.com/vgvassilev/clad
    owner = "vgvassilev";
    repo = pname;
    rev = version;
    sha256 = "sha256-TKCRAfwdTp/uDH7rk9EE4z2hwqBybklHhhYH6hQFYpg=";
  };

  # nativeBuildInputs = with pkgs; [
  #   cmake
  #   ninja
  # ] ++ (with llvmPackages ; [
  #   clang-unwrapped # for Clang library
  #   clangUseLLVM # for clang wrapper (useful to compile code that tests Cland)
  #   libcxxStdenv # standard C++ library for Clang
  #   stdenv # standard C library for Clang
  #   llvm # using LLVM 16, because this is also what ROOT uses
  # ]);

  nativeBuildInputs = with pkgs; [
    cmake
    ninja
  ];

  buildInputs = with llvmPackages ; [
    clang-unwrapped # for Clang library
    clangUseLLVM # for clang wrapper (useful to compile code that tests Cland)
    libcxxStdenv # standard C++ library for Clang
    stdenv # standard C library for Clang
    llvm # using LLVM 16, because this is also what ROOT uses
  ];

  cmakeFlags = [
    "-DCLAD_DISABLE_TESTS=ON"
    "-DLLVM_DIR=${llvmPackages.llvm.dev}"
    "-DClang_DIR=${llvmPackages.clang-unwrapped.dev}"
    # "-DCMAKE_INSTALL_PREFIX=$out"
    # "-DLLVM_EXTERNAL_LIT=${pkgs.python3Packages.lit}/bin/lit"
  ];

  postInstall = ''
    mkdir -p $out/nix-support
    echo "export CLAD_PLUGIN_PATH=$out/lib/clad.so" >> $out/nix-support/setup-hook
  '';

}
