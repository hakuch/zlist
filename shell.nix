let
  nixpkgs = builtins.fetchGit {
    url = "https://github.com/NixOS/nixpkgs.git";
    rev = "d7357d08d500b65c32b61160174cafde95828d30";
  };

  pkgs = import nixpkgs {};
  ocamlPackages = pkgs.ocaml-ng.ocamlPackages_4_11;
in

pkgs.mkShell {
  buildInputs = [
    ocamlPackages.ocaml
  ] ++ (with ocamlPackages; [
    dune_2
    findlib
    ppx_inline_test
    odoc
    utop
  ]);
}
