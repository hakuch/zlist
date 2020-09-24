let
  nixpkgs = builtins.fetchGit {
    url = "https://github.com/NixOS/nixpkgs.git";
    rev = "d7357d08d500b65c32b61160174cafde95828d30";
  };

  pkgs = import nixpkgs {};
  ocamlPackages = pkgs.ocaml-ng.ocamlPackages_4_11;
  ocamlformat = pkgs.ocamlformat.override { inherit ocamlPackages; };
in

pkgs.mkShell {
  buildInputs = [
    ocamlformat
    ocamlPackages.ocaml
  ] ++ (with ocamlPackages; [
    dune-release
    dune_2
    findlib
    ppx_expect
    odoc
    merlin
    utop
  ]);
}
