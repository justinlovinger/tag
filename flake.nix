{
  inputs = {
    naersk.url = "github:nix-community/naersk/master";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      naersk,
      nixpkgs,
      utils,
    }:
    utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
        naersk-lib = pkgs.callPackage naersk { };
      in
      {
        packages = rec {
          default = tag;

          tag = naersk-lib.buildPackage {
            src = ./.;
            doCheck = true;
          };

          tag-view = naersk-lib.buildPackage {
            pname = "tag-view";
            src = ./.;
            overrideMain = old: {
              preConfigure = ''
                cargo_build_options="$cargo_build_options --example tag-view"
              '';
            };
          };

          tag-organize = naersk-lib.buildPackage {
            pname = "tag-organize";
            src = ./.;
            overrideMain = old: {
              preConfigure = ''
                cargo_build_options="$cargo_build_options --example tag-organize"
              '';
            };
          };
        };

        devShell =
          with pkgs;
          mkShell {
            buildInputs = [
              cargo
              cargo-tarpaulin
              rust-analyzer
              rustc
              rustfmt
              rustPackages.clippy
            ];
            RUST_SRC_PATH = rustPlatform.rustLibSrc;
            PROPTEST_DISABLE_FAILURE_PERSISTENCE = 1;
          };
      }
    );
}
