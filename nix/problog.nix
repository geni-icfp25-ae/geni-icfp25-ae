{lib, stdenv, python3Packages, autoPatchelfHook}:

python3Packages.buildPythonPackage rec {

  pname = "ProbLog";
  version = "5bb0485992fc670b32f699f5ddb62519c2ac1442";

  src = builtins.fetchGit {
    url = "https://github.com/ML-KULeuven/problog";
    rev = version;
  };
  
  # Only use autoPatchelf on Linux
  nativeBuildInputs = lib.optionals stdenv.isLinux [ autoPatchelfHook ];

  propagatedBuildInputs = with python3Packages; [
    setuptools
  ];
  format = "pyproject";
}
