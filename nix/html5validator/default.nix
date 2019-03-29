let
  pkgs = import ../nixpkgs;
in with pkgs.python36Packages; buildPythonPackage rec {
   pname = "html5validator";
   version = "0.3.1";

   src = fetchPypi {
     inherit pname version;
     sha256 = "f587dac897b7d8f8009cfc8707f67d4cb4524facf9489c6e7a35c0801cc0e79e";
   };

   doCheck = false;

   meta = with pkgs.stdenv.lib; {
     description = "Command line tool that tests files for HTML5 validity";
     homepage = https://github.com/svenkreiss/html5validator;
     license = licenses.mit;
   };
}
