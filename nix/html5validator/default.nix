let
  pkgs = (import ../pkgs.nix);
in with pkgs.python39Packages; buildPythonPackage rec {
   pname = "html5validator";
   version = "0.4.0";

   src = fetchPypi {
     inherit pname version;
     sha256 = "sha256-PObj5zbJx7N+XibrFzugd3rgB3ZUm9YIkz+hSVUmDUk=";
   };

   propagatedBuildInputs = [
     pkgs.openjdk
     pyyaml
   ];

   doCheck = false;

   meta = {
     description = "Command line tool that tests files for HTML5 validity";
     homepage = https://github.com/svenkreiss/html5validator;
     license = pkgs.lib.licenses.mit;
   };
}
