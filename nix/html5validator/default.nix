let
  pkgs = import ../nixpkgs;
in with pkgs.python36Packages; buildPythonPackage rec {
   pname = "html5validator";
   version = "0.3.3";

   src = fetchPypi {
     inherit pname version;
     sha256 = "sha256-uSa+s7nHPC/vM3UJ9PTy8R0llmI9Wwat6ahgS4Tn20c=";
   };

   propagatedBuildInputs = [
     pkgs.openjdk
   ];

   doCheck = false;

   meta = with pkgs.stdenv.lib; {
     description = "Command line tool that tests files for HTML5 validity";
     homepage = https://github.com/svenkreiss/html5validator;
     license = licenses.mit;
   };
}
