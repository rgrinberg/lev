{
  description = "Lev Nix Flake";

  inputs.nix-filter.url = "github:numtide/nix-filter";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs.url = "github:nix-ocaml/nix-overlays";

  outputs = { self, nixpkgs, flake-utils, nix-filter }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages."${system}";
        inherit (pkgs.ocamlPackages) buildDunePackage;
      in
      rec {
        packages = rec {
          default = lev;
          lev = buildDunePackage {
            pname = "lev";
            version = "n/a";
            src = ./.;
            duneVersion = "3";
            propagatedBuildInputs = with pkgs.ocamlPackages; [ ];
            checkInputs = with pkgs.ocamlPackages; [ ppx_inline_test ppx_expect ];
            doCheck = true;
          };
          lev-fiber = buildDunePackage {
            pname = "lev-fiber";
            inherit (packages.lev) src version;
            propagatedBuildInputs = with pkgs.ocamlPackages; [ lev dyn fiber stdune ];
            duneVersion = "3";
          };
          lev-fiber-csexp = buildDunePackage {
            pname = "lev-fiber-csexp";
            inherit (packages.lev) src version;
            propagatedBuildInputs = with pkgs.ocamlPackages; [ lev-fiber stdune dyn csexp fiber ];
            duneVersion = "3";
          };
        };
        devShells.default = pkgs.mkShell {
          inputsFrom = pkgs.lib.attrValues packages;
          buildInputs = with pkgs.ocamlPackages; [ pkgs.ccls ocaml-lsp pkgs.ocamlformat ];
        };
      });
}
